":"; C="exec sbcl --noinform --end-runtime-options --disable-debugger"
":"; $C --load "$0" --eval '(main)' --quit --end-toplevel-options "$@"

(let ((*compile-verbose* nil))
  (dolist (package '(:cl-ppcre :cl-csv))
    (require package)))

(unless (constantp '+defaults+)
  (defconstant +defaults+
    '(:channels         "/etc/vdr/channels.conf"
      :epg-data-file    "/var/cache/vdr/epg.data"
      :epg-lisp-file    "/tmp/epg.lisp"
      :fs-entries       "/tmp/entries"
      :max-days         4
      :max-matches      4) "default values")
  (defconstant +imdb-pre+ "https://www.imdb.com/title/" "IMDB prefix.")
  (defconstant +vdr-admin-page+
    "http://~a/vdradmin.pl?aktion=timer_new_form&epg_id=~a&vdr_id=~a&imdb=~a"
    "page for creating a timer")
  (defconstant +unix-epoch-difference+ (encode-universal-time 0 0 0 1 1 1970 0)
    "difference between unix and common lisp"))
(defvar *config*         nil            "configuration list")
(defvar *epg*            nil            "epg data")
(defvar *channels*       nil            "informations about channels")
(defvar *fs-entries*     nil            "filmsearch entries")
(defvar *config-file*
  (merge-pathnames ".config/filmsearch.conf" (user-homedir-pathname))
  "configuration file")

(defun read-config ()
  "Read configuration from configuration file."
  (when *config-file*
    (with-open-file (in *config-file*)
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

(defun universal->unix-time (universal-time)
  "Conversion from universal time to unix time."
  (- universal-time +unix-epoch-difference+))

(defun unix->universal-time (unix-time)
  "Conversion from unix time to universal time."
  (+ unix-time +unix-epoch-difference+))

(defun get-unix-time ()
  "Get current unix time."
  (universal->unix-time (get-universal-time)))

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
                                    (format nil "~@[~a|~]~@[~a~]"
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

(defun check-duration (d runtime)
  "Check the duration."
  (if runtime
      (> d (* 60 8/10 runtime))
      t))

(defun check-year (d years)
  "Check the release year."
  (if years
      (if (cl-ppcre:scan
           (format nil "~{~a~^|~}"
                   (loop for i from (- (apply 'min years) 2)
                           to (+ (apply 'max years) 2)
                         collect i)) d)
          t
          (not (cl-ppcre:scan "[^0-9](1[89]|2[01])[0-9]{2}[^0-9]" d)))
      t))

(defun string->reg (s)
  "Convert string to regular expression."
  (let ((map '(("([:.!,?()])"   . "[\\1]?")
               ("[’'–_]"        . ".")
               ("([+|])"        . "[\\1]")
               ("( [*-/] )"     . "(\\1| )")
               ("([*])"         . "[\\1]")
               ("([áàâ])"       . "[a\\1]")
               ("([éèêë])"      . "[e\\1]")
               ("([îïíìıİ])"    . "[i\\1]")
               ("([ôóò])"       . "[o\\1]")
               ("([ûúù])"       . "[u\\1]")
               ("([ţ])"         . "[t\\1]")
               ("([ý])"         . "[y\\1]")
               ("æ"             . "(æ|ae)")
               ("œ"             . "(œ|oe)")
               ("ß"             . "(ß|ss)")
               ("ş"             . "[sş]")
               ("ž"             . "[zž]")
               ("([çč])"        . "[c\\1]"))))
    (dolist (m map)
      (setf s (cl-ppcre:regex-replace-all (format nil "(?i)~a" (car m))
                                          s (cdr m)))))
  s)

(defun check-title (epg-title epg-lang fs exactly)
  "Check title for a match."
  (let ((lang (if epg-lang (intern (string-upcase epg-lang)) 'all)))
    (macrolet ((fs-field1 (x) `(cdr (assoc ,x fs)))
               (fs-field2 (x) `(car (fs-field1 ,x)))
               (scanners () '(fs-field1 'scanners))
               (scanner () '(cdr (assoc lang (scanners)))))
      (unless (scanners)
        (rplacd (last fs)
                (list (copy-alist '(scanners (all) (de) (en) (fr))))))
      (unless (scanner)
        (setf
         (scanner)
         (cl-ppcre:create-scanner
          (or (fs-field2 'title-regex)
              (let ((list (fs-field1 'titles))
                    (akas (fs-field1 'akas)))
                (dolist (l akas)
                  (when (or (eq lang 'all) (eq lang (car l)))
                    (setf list (append list (cdr l)))))
                (setf list (delete-duplicates list :test 'string-equal))
                (format nil "(?i)~:[~;^(~]~{~a~^|~}~:[~;)$~]" exactly
                        (mapcar 'string->reg list) exactly))))))
      (cl-ppcre:scan (scanner) epg-title))))

(defun orig-in-description ()
  "Check if original titles are in description."
  )

;(defun check-all-keywords (description fs)
(defun check-all-keywords ()
  "Check if all keywords are in description."
  )

(defun check-one-keyword (description fs)
  "Check if at least one keyword is in description."
  (macrolet ((fs-field (x) `(cdr (assoc ,x fs)))
             (scanner () '(fs-field 'one-keyword-scanner)))
    (unless (scanner)
      (rplacd
       (last fs)
       (list (cons 'one-keyword-scanner
                   (cl-ppcre:create-scanner
                    (format nil "(?i)~{~a~^|~}"
                            (mapcar 'string->reg (fs-field 'one-keyword))))))))
    (cl-ppcre:scan (scanner) description)))

(defun find-match (fs epg)
  "Try to find a match."
  (macrolet ((fs-field1 (x) `(cdr (assoc ,x fs)))
             (fs-field2 (x) `(car (fs-field1 ,x)))
             (checks () '(fs-field1 'checks)))
    (unless (checks)
      (rplacd (last fs) (list (list 'checks 'title))))
    (dolist (c '(all-keywords one-keyword))
      (if (fs-field1 c) (pushnew c (checks))))
    (dolist (c (checks) t)
      (unless
          (case c
            (title (check-title (getf epg :title) (getf epg :lang) fs
                                 (fs-field2 'exact-title)))
            (runtime (check-duration (getf epg :duration)
                                     (fs-field2 'runtime)))
            (year (check-year (getf epg :description)
                              (fs-field1 'years)))
            (orig-desc (orig-in-description))
;           (all-keywords (check-all-keywords (getf epg :description) fs))
            (one-keyword (check-one-keyword (getf epg :description) fs)))
        (return)))))

(defun print-match (fs epg)
  "Print infos about a match."
  (multiple-value-bind (s m h d mm y)
      (decode-universal-time (unix->universal-time (getf epg :start)))
    (declare (ignore s))
    (let* ((title (getf epg :title))
           (imdb-id (cadr (assoc 'id fs)))
           (o-title (format nil "~{~a~^ / ~}" (cdr (assoc 'titles fs))))
           (years (cdr (assoc 'years fs)))
           (rating (cadr (assoc 'rating fs)))
           (epg-id (getf epg :id))
           (channel-number (getf epg :channel-number))
           (channel-name (getf epg :channel))
           (date (format nil "~2,'0d-~2,'0d-~4,'0d" d mm y))
           (time (format nil "~2,'0d:~2,'0d" h m))
           (vdradmin (format nil +vdr-admin-page+ (cv :vdradmin-host)
                             epg-id channel-number imdb-id))
           (imdb (format nil "~a~a/~@[  (~a)~]~@[ ~a~]" +imdb-pre+ imdb-id
                         o-title years))
           (desc (getf epg :description))
           (subject (format nil "[FS] ~a: ~a" date title))
           (content
            (format
             nil "~@[~%*** ~a ***~%~%~]~@{~#[~;~%~a~%~:;~7a:   ~a~%~]~}"
             (cdr (assoc 'comment fs)) "Title" title "Channel" channel-name
             "Date" date "Time" time "Rating" rating "IMDB" imdb "Timer"
             vdradmin
             (cl-ppcre:regex-replace-all "\\|" desc (string #\Newline)))))
      (format t "~a~%~a~%~%" subject content))))

(defun search-films ()
  "Do the search."
  (dolist (epg *epg*)
    (unless (getf epg :searched)
      (dolist (fs *fs-entries*)
        (symbol-macrolet ((mc (cdr (assoc 'match-count fs))))
          (unless mc
            (rplacd (last fs) (copy-alist (list (cons 'match-count 0)))))
          (when (and (< mc (cv :max-matches))
                     (find-match fs epg))
            (incf mc)
            (print-match fs epg))))
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
  (if (second sb-ext:*posix-argv*)
      (setf *config-file* (second sb-ext:*posix-argv*)))
  (read-config)
  (when (new-epg)
    (setf *channels* (read-channels)
          *fs-entries* (read-fs))
    (filmsearch)))

;; Local Variables:
;; pm/slime-auto-load: t
;; End:
