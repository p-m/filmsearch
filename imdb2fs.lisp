":"; C="exec sbcl --noinform --end-runtime-options --disable-debugger"
":"; $C --load "$0" --eval '(main)' --quit --end-toplevel-options "$@"

;; example usage: "imdb2fs.lisp input-file output-file tmdb-key-file"

(require :drakma)
(require :cl-json)
(unless (constantp '+input+)
  (defconstant +input+ (or (second sb-ext:*posix-argv*) "/tmp/imdb-ids")
    "File with IMDB IDs.")
  (defconstant +output+ (or (third sb-ext:*posix-argv*) "/tmp/fs-entries")
    "File with resulting fs-entries.")
  (defconstant +tmdb-pre+ "https://api.themoviedb.org/3/" "TMDB prefix.")
  (defconstant +tmdb-keyfile+ (or (fourth sb-ext:*posix-argv*) "/tmp/tmdb-key")
    "File with TMDB key.")
  (defconstant +tmdb-key+
    (format nil "?api_key=~a" (with-open-file (in +tmdb-keyfile+)
                                (read-line in)))
    "API key for TMDB."))

(defun get-imdb-title (id)
  "Get title (with year) from IMDB."
  (let* ((url (format nil "http://akas.imdb.com/title/tt~a/" id))
         (body (drakma:http-request url)))
    (multiple-value-bind (dummy v)
        (cl-ppcre:scan-to-strings
         "<meta property='og:title' content=\"(.*)\" />" body)
      (declare (ignore dummy))
      (elt v 0))))

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

(defun imdb-id->entry (imdb-id)
  "Create new fs-entry from imdb-id."
  (let* (alist
         (imdb-title-year (get-imdb-title imdb-id))
         (movie (get-json (format nil "~afind/tt~a~a&external_source=imdb_id"
                                  +tmdb-pre+ imdb-id +tmdb-key+)))
         (movie-results (cadr (assoc :movie--results movie)))
         (id (write-to-string (cdr (assoc :id movie-results))))
         (movie-results2 (get-json (format nil "~amovie/~a~a"
                                           +tmdb-pre+ id +tmdb-key+)))
         (release-date (cdr (assoc :release--date movie-results2))))
    (multiple-value-bind (dummy v) (cl-ppcre:scan-to-strings
                                    "^(.*) \\(([0-9]*)\\)$" imdb-title-year)
      (declare (ignore dummy))
      (when v
        (alist-add 'imdb-title (elt v 0))
        (alist-add 'imdb-year (check-year (elt v 1)))))
    (alist-add 'release-year (check-year release-date))
    (alist-add 'runtime (cdr (assoc :runtime movie-results2)))
    (alist-add
     'alternative-titles
     (cdr (assoc :titles
                 (get-json (format nil "~amovie/~a/alternative_titles~a"
                                   +tmdb-pre+ id +tmdb-key+)))))
    (alist-add 'tmdb-id id)   (alist-add 'imdb-id imdb-id)
    (alist-add 'original-lang (cdr (assoc :original--language movie-results)))
    (alist-add 'original-title (cdr (assoc :original--title movie-results)))
    alist))

(defun main ()
  "Main program."
  (with-open-file (in +input+)
    (with-open-file (out +output+ :direction :output :if-exists :supersede)
      (loop for imdb-id = (read-line in nil)
         while imdb-id do
           (let ((*print-case* :downcase))
             (pprint (imdb-id->entry imdb-id) out))))))
