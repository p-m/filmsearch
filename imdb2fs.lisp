":"; C="exec sbcl --noinform --end-runtime-options --disable-debugger"
":"; $C --load "$0" --eval '(main)' --quit --end-toplevel-options "$@"

;; example usage: "imdb2fs.lisp input-file output-file tmdb-key-file"

(dolist (package '(:drakma :cl-json :plump-dom :plump-sexp))
  (require package))
(unless (constantp '+input+)
  (defconstant +input+ (or (second sb-ext:*posix-argv*) "/tmp/imdb-ids")
    "File with IMDB IDs.")
  (defconstant +output+ (or (third sb-ext:*posix-argv*) "/tmp/fs-entries")
    "File with resulting fs-entries.")
  (defconstant +imdb-pre+ "http://akas.imdb.com/title/tt" "IMDB prefix.")
  (defconstant +tmdb-pre+ "https://api.themoviedb.org/3/" "TMDB prefix.")
  (defconstant +tmdb-keyfile+ (or (fourth sb-ext:*posix-argv*) "/tmp/tmdb-key")
    "File with TMDB key.")
  (defconstant +tmdb-key+
    (format nil "?api_key=~a" (with-open-file (in +tmdb-keyfile+)
                                (read-line in)))
    "API key for TMDB."))

(defun get-match (reg text)
  "Get a regex match from multi-line text."
  (multiple-value-bind (dummy v) (cl-ppcre:scan-to-strings reg text)
    (declare (ignore dummy))
    (when v (elt v 0))))

(defun get-imdb-title (imdb)
  "Get title (with year) from IMDB."
  (get-match "<meta property='og:title' content=\"(.*)\" />" imdb))

(defun get-runtime (imdb)
  "Get runtime from imdb."
  (let ((match
            (get-match
             "<time itemprop=\"duration\" datetime=\".*\">([0-9]+) min</time>"
             imdb)))
    (when (stringp match)
      (parse-integer match :junk-allowed t))))

(defun get-lang (imdb)
  "Get original lang from imdb."
  (get-match "<a href=\"/language/([a-z]{2})\\?ref.*\"" imdb))

(defun get-json (url)
  "Get list from json url."
  (let ((stream (drakma:http-request url :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (cl-json:decode-json stream)))

(defmacro alist-add (key value)
  "Add key-value pair to alist."
  `(push (cons ,key ,value) alist))

(defmacro alist-get (key)
  "Get value of key from alist."
  `(cdr (assoc ,key alist)))

(defun check-year (year)
  "Check year and return it if it's ok."
  (when (stringp year)
    (let ((y (parse-integer year :junk-allowed t)))
      (when (and y (< 1000 y 3000))
        y))))

(defun get-table-by-id (node id)
  "Searches the given node and returns the first
table at arbitrary depth that matches the given ID
attribute."
  (labels ((scanren (node)
             (loop for child across (plump-dom:children node)
                do (when (plump-dom:element-p child)
                     (let ((cid (plump-dom:attribute child "id")))
                       (when (and (string-equal id cid)
                                  (string-equal "table"
                                                (plump-dom:tag-name child)))
                         (return-from get-table-by-id child)))
                     (scanren child)))))
    (scanren node))
  nil)

(defun get-table-entries (reg table)
  "Get table entries from imdb release-info, that match reg."
  (loop for e in table
     when (and (listp e)                (listp (car e))
               (eq (caar e) :tr)        (eq (caaddr e) :td)
               (eq (car (nth 4 e)) :td)
               (cl-ppcre:scan (format nil "(?i)~a" reg) (cadr (nth 2 e))))
     collect (cadr (nth 4 e))))

(defun get-tmdb-akas (reg list)
  "Get list entries from tmdb akas-list, that match reg."
  (loop for e in list
     when (and (listp e)                (listp (car e))
               (cl-ppcre:scan (format nil "(?i)~a" reg) (cdar e)))
     collect (cdadr e)))

(defun unique (&rest args)
  "Remove duplicates and NILs. If only one element, return without list."
  (let* ((res1 (delete-if 'null args))
         (res2 (delete-duplicates res1 :test (if (numberp (car res1))
                                                 '= 'string-equal))))
    (if (= (length res2) 1)
        (car res2)
        res2)))

(defun get-akas (tmdb imdb)
  "Get all the alternative titles."
  (let (res (regs '((de tmdb "at|de" imdb "german|austria")
                    (en tmdb "gb|us" imdb "english")
                    (fr tmdb "fr"    imdb "france|french"))))
    (dolist (e regs res)
      (let* ((key (car e)) (plist (cdr e))
             (t-reg (getf plist 'tmdb)) (i-reg (getf plist 'imdb)))
        (push (cons key (apply
                         'unique (append (get-tmdb-akas t-reg tmdb)
                                         (get-table-entries i-reg imdb))))
              res)))))

(defun get-orig-title (imdb)
  "Get original title from imdb titles."
  (car (get-table-entries "original title" imdb)))

(defun check-entry (e)
  "Check the new entry."
  (let* ((imdb-id (caddr (assoc 'ids e)))
         (url1 (format nil "~a~a/" +imdb-pre+ imdb-id))
         (url2 (format nil "~areleaseinfo#akas" url1))
         (o-lang (cdr (assoc 'original-langs e)))
         (akas (cdr (assoc 'alternative-titles e)))
         (runtimes (cdr (assoc 'runtimes e)))
         (years (cdr (assoc 'release-years e)))
         flag)
    (flet ((problem (text)
             (setf flag t) (format t "~a: ~a~%" imdb-id text))
           (check-numbers (nums max-diff)
             (or (null nums)
                 (and (listp nums)
                      (> (abs (- (car nums) (cadr nums))) max-diff)))))
      (when (listp (cdr (assoc 'original-titles e)))
        (problem "check original title"))
      (if (stringp o-lang)
          (if (and (string/= o-lang "de") (null (cdr (assoc 'de akas))))
              (problem "no german title"))
          (problem "check original languages"))
      (when (check-numbers runtimes 5)
        (problem "check runtime"))
      (when (check-numbers years 1)
        (problem "check release-years")))
    (when flag
      (run-program "firefox" (list "-new-tab" url1 url2) :search t))))

(defun imdb-id->entry (imdb-id)
  "Create new fs-entry from imdb-id."
  (let* (alist imdb-title imdb-year
         (imdb (drakma:http-request (format nil "~a~a/" +imdb-pre+ imdb-id)))
         (imdb-title-year (get-imdb-title imdb))
         (movie (get-json (format nil "~afind/tt~a~a&external_source=imdb_id"
                                  +tmdb-pre+ imdb-id +tmdb-key+)))
         (movie-results (cadr (assoc :movie--results movie)))
         (id (write-to-string (cdr (assoc :id movie-results))))
         (movie-results2 (get-json (format nil "~amovie/~a~a"
                                           +tmdb-pre+ id +tmdb-key+)))
         (release-date (cdr (assoc :release--date movie-results2)))
         (tmdb-akas
          (cdr (assoc :titles
                      (get-json (format nil "~amovie/~a/alternative_titles~a"
                                        +tmdb-pre+ id +tmdb-key+)))))
         (imdb-akas
          (let ((akas
                 (get-table-by-id
                  (plump:parse (drakma:http-request
                                (format nil "~a~a/releaseinfo" +imdb-pre+
                                        imdb-id))) "akas")))
            (when akas (plump-sexp:serialize akas)))))
    (multiple-value-bind (dummy v) (cl-ppcre:scan-to-strings
                                    "^(.*) \\(([0-9]*)\\)$" imdb-title-year)
      (declare (ignore dummy))
      (when v (setf imdb-title (elt v 0) imdb-year (check-year (elt v 1)))))
    (alist-add 'release-years (unique (check-year release-date) imdb-year))
    (alist-add 'runtimes (unique (cdr (assoc :runtime movie-results2))
                                 (get-runtime imdb)))
    (alist-add 'alternative-titles (get-akas tmdb-akas imdb-akas))
    (alist-add 'ids (list id imdb-id))
    (alist-add 'original-langs
               (unique (cdr (assoc :original--language movie-results))
                       (get-lang imdb)))
    (alist-add 'original-titles
               (unique (cdr (assoc :original--title movie-results))
                       (get-orig-title imdb-akas)))
    (alist-add 'imdb-title imdb-title)
    alist))

(defun main ()
  "Main program."
  (with-open-file (in +input+)
    (with-open-file (out +output+ :direction :output :if-exists :supersede)
      (loop for imdb-id = (read-line in nil)
         while imdb-id do
           (let ((*print-case* :downcase)
                 (entry (imdb-id->entry imdb-id)))
             (check-entry entry)
             (pprint entry out))))))
