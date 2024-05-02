":"; C="exec sbcl --noinform --end-runtime-options --disable-debugger"
":"; $C --load "$0" --eval '(main)' --quit --end-toplevel-options "$@"

(let ((*compile-verbose* nil))
  (dolist (package '(:gzip-stream :cl-csv :flexi-streams))
    (require package)))

(defvar *config*        nil "configuration list")
(defvar *fs-entries*    nil "filmsearch entries")
(defvar *imdb-ids*      nil)
(defvar *title-akas*    nil)
(defvar *title-basics*  nil)
(defvar *title-ratings* nil)
(defvar *config-file*
  (merge-pathnames ".config/imdb2fs.conf" (user-homedir-pathname))
  "configuration file")

(defun read-config ()
  "Read configuration from configuration file."
  (with-open-file (in *config-file*)
    (with-standard-io-syntax
      (setf *config* (read in)))))

(defun cv (name)
  "Get configuration value."
  (getf *config* name))

(defun fetch-row (stream)
  (cl-csv:read-csv-row stream :separator #\Tab :quote nil))

(defun read-gz (file)
  (gzip-stream:with-open-gzip-file (raw (format nil "~a/title.~a.tsv.gz"
                                                (cv :imdb-dir) file))
    (do* ((utf8  (flexi-streams:make-flexi-stream raw :external-format :utf-8))
          (first (fetch-row utf8))
          (row   (fetch-row utf8)
                 (handler-case (fetch-row utf8)
                   (end-of-file (c) (declare (ignore c)))))
          res)
         ((null row) (push first res))
      (if (member (car row) *imdb-ids* :test #'string-equal)
          (push row res)))))

(defun create-lists ()
  (with-open-file (in (cv :imdb-ids))
    (do ((imdb-id (read-line in) (read-line in nil)))
        ((null imdb-id))
      (push imdb-id *imdb-ids*)))
  (setf *title-basics*  (read-gz "basics")
        *title-akas*    (read-gz "akas")
        *title-ratings* (read-gz "ratings"))
  (with-open-file (in (cv :entries))
    (with-standard-io-syntax
      (setf *fs-entries* (read in)))))

(defun ensure-list (object)
  (if (listp object)
      object
      (list object)))

(defmacro make-item (x)
  `(append '(,x) (ensure-list ,x)))

(defun get-entry (id)
  (let ((labels (car *title-basics*))
        (entry (find id (cdr *title-basics*) :test #'string-equal
                                             :key (lambda (x) (car x)))))
    (list labels entry)))

(defun get-field (name entry)
  "Entry is a list with labels and the real entry."
  (let ((index (position name (car entry) :test #'string-equal)))
    (nth index (cadr entry))))

(defun akas-field (name entry)
  (let ((index (position name (car *title-akas*) :test #'string-equal)))
    (nth index entry)))

(defun unique (list)
  "Remove duplicates and NILs."
  (delete-duplicates (delete-if #'null list) :test #'string-equal))

(defun get-titles (entry)
  (unique (list (get-field "primaryTitle" entry)
                (get-field "originalTitle" entry))))

(defun get-integer (field entry)
  (let ((number (get-field field entry)))
    (if number (parse-integer number :junk-allowed t))))

(defun get-year (entry)
  (get-integer "startYear" entry))

(defun get-runtime (entry)
  (get-integer "runtimeMinutes" entry))

(defun parse-float (string)
  (if string
      (with-input-from-string (in string) (read in))))

(defun get-rating (id)
  (parse-float (cadr (find id (cdr *title-ratings*)
                           :test #'string-equal :key (lambda (x) (car x))))))

(defun check-lang (lang regions entry)
  (let ((region   (akas-field "region"   entry))
        (language (akas-field "language" entry)))
    (or (string-equal lang language)
        (and (string-equal language "\\N")
             (member region regions :test #'string-equal)))))

(defun get-akas (id orig-titles)
  (let* ((akas1 (remove id *title-akas* :test #'string-not-equal :key #'car))
         (akas2 (remove-if #'(lambda (x)
                               (member x orig-titles :test #'string-equal))
                           akas1 :key #'(lambda (x) (akas-field "title" x))))
         res)
    (dolist (lang (cv :languages) res)
      (let* ((lang-id  (car lang))
             (regions  (cdr lang))
             (lang-str (symbol-name lang-id))
             (entries  (remove-if-not
                        #'(lambda (x) (check-lang lang-str regions x))
                        akas2))
             (titles   (unique (mapcar #'(lambda (x)
                                           (akas-field "title" x)) entries))))
        (push lang-id titles)
        (push titles res)))))

(defun imdb-id->entry (id)
  "Create new fs-entry from imdb-id."
  (let* ((entry   (get-entry   id))
         (titles  (get-titles  entry))
         (year    (get-year    entry))
         (runtime (get-runtime entry))
         (rating  (get-rating  id))
         (akas    (get-akas    id titles)))
    (nconc (list (make-item titles) (list 'id id) (make-item year)
                 (make-item runtime)) (if rating (list (make-item rating)))
                 (list (make-item akas)))))

(defun main ()
  "Main program."
  (if (second sb-ext:*posix-argv*)
      (setf *config-file* (second sb-ext:*posix-argv*)))
  (read-config)
  (create-lists)
  (setf *print-case* :downcase)
  (dolist (id *imdb-ids*)
    (let ((entry (imdb-id->entry id)))
      (pprint entry))))

;; Local Variables:
;; pm/slime-auto-load: t
;; End:
