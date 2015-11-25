":"; C="exec sbcl --noinform --end-runtime-options --disable-debugger"
":"; $C --load "$0" --eval '(main)' --quit --end-toplevel-options "$@"

;; example usage: "filmsearch.lisp config-file"

(with-open-file (*standard-output*
                 "/dev/null" :direction :output :if-exists :append)
  (dolist (package '(:cl-ppcre :cl-csv :cl-sendmail))
    (require package)))
(unless (constantp '+config-file+)
  (defconstant +config-file+ (second sb-ext:*posix-argv*)
    "configuration file")
  (defconstant +defaults+
    '(:channels         "/vdr/etc/channels.conf"
      :epg-data-file    "/var/cache/vdr/epg.data"
      :epg-lisp-file    "/dev/shm/filmsearch/epg"
      :fs-entries       "/dev/shm/filmsearch/entries"
      :max-days         4
      :max-matches      4) "default values")
  (defconstant +imdb-pre+ "http://akas.imdb.com/title/tt" "IMDB prefix.")
  (defconstant +vdr-admin-page+
    "http://~a/vdradmin.pl?aktion=timer_new_form&epg_id=~a&vdr_id=~a"
    "page for creating a timer")
  (defconstant +unix-epoch-difference+ (encode-universal-time 0 0 0 1 1 1970 0)
    "difference between unix and common lisp"))
(defvar *config*         nil            "configuration list")
(defvar *epg*            nil            "epg data")
(defvar *channels*       nil            "informations about channels")
(defvar *fs-entries*     nil            "filmsearch entries")

(defun read-config ()
  "Read configuration from configuration file."
  (when +config-file+
    (with-open-file (in +config-file+)
      (with-standard-io-syntax
        (setf *config* (read in))))))

(defun cv (name)
  "Get configuration value."
  (or (getf *config* name) (getf +defaults+ name)))

(defun new-epg ()
  "Check if epg is new."
  (let ((elf (cv :epg-lisp-file)))
    (if (probe-file elf)
        (< (file-write-date elf) (file-write-date (cv :epg-data-file)))
        (ensure-directories-exist elf))))

(defun channels->list ()
  "Make list of csv data from channels-file."
  (with-open-file (in (cv :channels))
    (loop with list and lc = (cv :last-channel)
       for line = (read-line in nil)
       while line
       do (setf list (car (cl-csv:read-csv line :separator #\:)))
       when (= (length list) 13)
         collect list
       when (and lc (string= lc (car list)))
         do (loop-finish))))

(defun get-lang (apid)
  "Get main-language from APID."
  (multiple-value-bind (l r)
      (cl-ppcre:regex-replace "^[^=]*=(fr|de|en).*$" apid "\\1")
    (when r
      l)))

(defun read-channels ()
  "Read channels."
  (loop with num = 0
     for (a b c d e f g h i j k l m) in (channels->list)
     collect (list :name a :freq b :params c :src d :srate e :vpid f :apid g
                   :tpid h :ca i :sid j :nid k :tid l :rid m :num (incf num)
                   :lang (get-lang g))))

(defun get-matches (reg text)
  "Get matches as vector."
  (multiple-value-bind (dummy v) (cl-ppcre:scan-to-strings reg text)
    (declare (ignore dummy))
    v))

(defun channel-by-name (name)
  "Get the channel by name."
  (find-if (lambda (x) (cl-ppcre:scan (format nil "^~a(,.*|;.*)?$" name)
                                      (getf x :name))) *channels*))

(defun universal-to-unix-time (universal-time)
  "Conversion from universal time to unix time."
  (- universal-time +unix-epoch-difference+))

(defun unix-to-universal-time (unix-time)
  "Conversion from unix time to universal time."
  (+ unix-time +unix-epoch-difference+))

(defun get-unix-time ()
  "Get current unix time."
  (universal-to-unix-time (get-universal-time)))

(defun check-date (start)
  "Check if date is not too far in the future."
  (and
   (< (get-unix-time) start)
   (> (+ (get-unix-time) (* (cv :max-days) 24 60 60)) start)))

(defun epg-entries (s)
  "Get epg-entries from stream as list."
  (loop with list and id and start and duration and title and
     short and description and cn and channel
     for line = (read-line s nil)
     while line
     do (let ((type (elt line 0)) vec
              (cont (if (< (length line) 2) "" (subseq line 2))))
          (case type
            (#\C
             (setf cn (cl-ppcre:regex-replace "^[^ ]* (.*)$" cont "\\1")
                   channel (channel-by-name cn)))
            (#\E (setf vec (get-matches "^([^ ]+) ([^ ]+) ([^ ]+).*$" cont)
                       id (elt vec 0) start (parse-integer (elt vec 1))
                       duration (parse-integer (elt vec 2))))
            (#\T (setf title cont))
            (#\S (setf short cont))     (#\D (setf description cont))
            (#\e (when (and channel (check-date start))
                   (setf list (list :id id :start start :duration duration
                                    :title title :channel cn :description
                                    (format nil "~@[~a ~]~@[~a~]"
                                            short description)
                                    :channel-number (getf channel :num)
                                    :lang (getf channel :lang))))
                 (setf title nil short nil description nil))
            (#\c (setf cn nil channel nil))))
     when list
       collect list
     do (setf list nil)))

(defun remove-old-epg-entries ()
  "Remove too old entries."
  (setf *epg*
        (remove-if-not (lambda (x) (check-date (getf x :start))) *epg*)))

(defun check-duration (d runtimes)
  "Check the duration."
  (if runtimes
      (> d (* 60 8/10 (apply 'min runtimes)))
      t))

(defun check-year (d years)
  "Check the release year."
  (if years
      (cl-ppcre:scan (format nil "~{~a~^|~}"
                             (loop for i from (1- (apply 'min years)) to
                                  (1+ (apply 'max years)) collect i)) d)
      t))

(defun string->reg (s)
  "Convert string to regular expression."
  (let ((map '(("([:.!,?])"  . "[\\1]?")
               ("['–]"       . ".")
               ("([+|])"     . "[\\1]")
               ("( [*-/] )"  . "(\\1| )")
               ("([*])"      . "[\\1]")
               ("([áàâ])"    . "[a\\1]")
               ("([éèêë])"   . "[e\\1]")
               ("([îï])"     . "[i\\1]")
               ("(ôóò)"      . "[o\\1]")
               ("([ûúù])"    . "[u\\1]"))))
    (dolist (m map)
      (setf s (cl-ppcre:regex-replace-all (format nil "(?i)~a" (car m))
                                          s (cdr m)))))
  s)

(defun check-title (epg-title epg-lang fs exactly)
  "Check title for a match."
  (let ((lang (if epg-lang (intern (string-upcase epg-lang)) 'all)))
    (macrolet ((fs-field (x) `(cdr (assoc ,x fs)))
               (scanners () '(fs-field 'scanners))
               (scanner () '(cdr (assoc lang (scanners)))))
      (unless (scanners)
        (rplacd (last fs)
                (list (copy-alist '(scanners (all) (de) (en) (fr))))))
      (unless (scanner)
        (setf
         (scanner)
         (cl-ppcre:create-scanner
          (let ((list (fs-field 'original-titles))
                (a-titles (fs-field 'alternative-titles)))
            (dolist (l a-titles)
              (when (or (eq lang 'all) (eq lang (car l)))
                (setf list (append list (cdr l)))))
            (setf list (delete-duplicates list :test 'string-equal))
            (format nil "(?i)~:[~;^(~]~{~a~^|~}~:[~;)$~]" exactly
                              (mapcar 'string->reg list) exactly)))))
      (cl-ppcre:scan (scanner) epg-title))))

(defun orig-in-description ()
  "Check if original titles are in description."
  )

(defun check-all-keywords ()
  "Check if all keywords are in description."
  )

(defun check-one-keyword ()
  "Check if at least one keyword is in description."
  )

(defun find-match (fs epg)
  "Try to find a match."
  (macrolet ((fs-field (x) `(cdr (assoc ,x fs)))
             (checks () '(fs-field 'checks)))
    (unless (checks)
      (rplacd (last fs) (list (list 'checks 'title))))
    (dolist (c '(all-keywords one-keyword))
      (if (fs-field c) (pushnew c (checks))))
    (dolist (c (checks) t)
      (unless
          (case c
            (title
             (check-title (getf epg :title) (getf epg :lang) fs nil))
            (exact-title
             (check-title (getf epg :title) (getf epg :lang) fs t))
            (runtime
             (check-duration (getf epg :duration)
                             (fs-field 'runtimes)))
            (year
             (check-year (getf epg :description)
                         (fs-field 'release-years)))
            (orig-desc
             (orig-in-description))
            (all-keywords
             (check-all-keywords))
            (one-keyword
             (check-one-keyword)))
        (return)))))

(defun send-email (fs epg)
  "Send email about a match."
  (multiple-value-bind (s m h d mm y) (decode-universal-time
                                       (unix-to-universal-time
                                        (getf epg :start)))
    (declare (ignore s))
    (let* ((title (getf epg :title))
           (imdb-id (caddr (assoc 'ids fs)))
           (imdb-title (cdr (assoc 'imdb-title fs)))
           (years (cdr (assoc 'release-years fs)))
           (epg-id (getf epg :id))
           (to (cv :email-to)) (from (cv :email-from))
           (rcpts (append (and to (list to)) (cdr (assoc 'emails fs))))
           (channel-number (getf epg :channel-number))
           (channel-name (getf epg :channel))
           (date (format nil "~2,'0d-~2,'0d-~4,'0d" d mm y))
           (time (format nil "~2,'0d:~2,'0d" h m))
           (vdradmin (format nil +vdr-admin-page+ (cv :vdradmin-host)
                             epg-id channel-number))
           (imdb (format nil "~a~a/~@[  (~a)~]~@[ ~a~]" +imdb-pre+ imdb-id
                         imdb-title years))
           (desc (getf epg :description))
           (subject (format nil "[FS] ~a: ~a" date title))
           (content
            (format
             nil "~@[~%*** ~a ***~%~%~]~@{~#[~;~%~a~%~:;~7a:   ~a~%~]~}"
             (cdr (assoc 'comment fs)) "Title" title "Channel" channel-name
             "Date" date "Time" time "IMDB" imdb "Timer" vdradmin
             (cl-ppcre:regex-replace-all "\\|" desc (string #\Newline)))))
      (if rcpts
          (cl-sendmail:with-email (stream rcpts :from from :subject subject)
            (setf (cl-sendmail::content stream) content))
          (format t "~a~%" subject)))))

(defun search-films ()
  "Do the search."
  (dolist (epg *epg*)
    (unless (getf epg :searched)
      (dolist (fs *fs-entries*)
        (symbol-macrolet ((match-count '(cdr (assoc 'match-count fs))))
          (unless match-count
            (rplacd (last fs) (copy-alist (list (cons 'match-count 0)))))
          (when (and (< match-count (cv :max-matches))
                     (find-match fs epg))
            (incf match-count)
            (send-email fs epg))))
      (setf (getf epg :searched) t))))

(defun filmsearch ()
  "Update EPG, do the search and write back the EPG."
  (let ((elf (cv :epg-lisp-file)))
    (when (probe-file elf)
      (with-open-file (in elf)
        (with-standard-io-syntax
          (setf *epg* (read in)))))
    (remove-old-epg-entries)
    (with-open-file (in (cv :epg-data-file))
      (dolist (e (epg-entries in))
        (setf (getf e :searched) t)
        (unless (member e *epg* :test 'equal)
          (setf (getf e :searched) nil)
          (push e *epg*))))
    (search-films)
    (with-open-file (out elf :direction :output :if-exists :supersede)
      (let ((*print-case* :downcase))
        (pprint *epg* out)))))

(defun read-fs ()
  "Read filmsearch entries."
  (with-open-file (in (cv :fs-entries))
    (with-standard-io-syntax
      (read in))))

(defun main ()
  "Main program."
  (read-config)
  (when (new-epg)
    (setf *channels* (read-channels)
          *fs-entries* (read-fs))
    (filmsearch)))
