":"; C="exec sbcl --noinform --end-runtime-options --disable-debugger"
":"; $C --load "$0" --eval '(main)' --quit --end-toplevel-options "$@"

;; Make a Web page with the available films.

(require :cl-ppcre)

(unless (constantp '+defaults+)
  (defconstant +defaults+
    `(:src-dir     "/opt/videos"
      :dest-file   "/srv/www/htdocs/my-films.html"
      :dest-title  "Private Movie Database"
      :fs-entries  "/tmp/entries")))

(defvar *config-file*   "~/.config/make-db.conf" "configuration file")
(defvar *config*        nil "configuration list")
(defvar *fs-entries*    nil "search entries")
(defparameter *missing* nil "films with missing information")
(defparameter *films*   nil "all available films")

(defun file-to-list (file)
  (with-open-file (in file)
    (with-standard-io-syntax
      (loop for obj = (read in nil)
            while obj collect obj))))

(defun read-config ()
  "Read configuration from configuration file."
  (when *config-file*
    (setf *config* (file-to-list *config-file*))))

(defun cv (name)
  "Get configuration value."
  (or (getf *config* name) (getf +defaults+ name)))

(defun strmerge (&rest strings)
  (apply #'concatenate 'string strings))

(defun my-pathname (x)
  (namestring (native-pathname x)))

(defun get-fs-entry (id)
  (find id *fs-entries* :test #'string=
        :key #'(lambda (x) (cadr (assoc 'id x)))))

(defun get-runtime (file)
  (round
   (read-from-string
    (with-output-to-string (s)
      (run-program "ffprobe" (list "-v" "error" "-show_entries"
                                   "format=duration" "-of"
                                   "default=nw=1:nk=1"
                                   (native-namestring file))
                   :output s :search t))) 60))

(defun hd-or-sd (file)
  (let ((str
          (with-output-to-string (s)
            (run-program "ffprobe"
                         (list "-v" "error" "-select_streams" "v:0"
                               "-show_entries" "stream=width" "-of"
                               "default=nw=1:nk=1" (native-namestring file))
                         :output s :search t))))
    (if (< 900 (parse-integer str :junk-allowed t)) "HD" "SD")))

(defun get-info (n)
  (let ((file (my-pathname (format nil "~a/~a.inf" (cv :src-dir) n))))
    (with-open-file (out file :direction :output :if-exists nil)
      (format out "T ~a~%~{~a ~%~}" (substitute #\SPACE #\_ n) '(S D R I A U)))
    (let (res)
      (with-open-file (in file)
        (loop for line = (read-line in nil)
              while line do
                (setf (getf res (intern (subseq line 0 1)))
                      (cl-ppcre:regex-replace-all "\\|"
                                                  (subseq line 2) "<br>"))))
      (if (> (length (getf res 'A)) 0)
          (setf (getf res 'TS)
                (multiple-value-bind (s min h d m y)
                    (decode-universal-time (file-write-date file))
                  (declare (ignore s min h))
                  (format nil "~d-~2,'0d-~2,'0d" y m d)))
          (progn (push n *missing*) (setf res nil)))
      res)))

(defun file-name (file)
  (format nil "~a.~a" (pathname-name file) (pathname-type file)))

(defun gather-infos ()
  (setf *fs-entries* (file-to-list (cv :fs-entries)))
  (dolist (film (directory (strmerge (cv :src-dir) "/*.m??"))) ;; mp4 or mkv
    (let* ((name (pathname-name film)) (inf-list (get-info name))
           (hdsd (hd-or-sd film))      (fs-entry (get-fs-entry (getf inf-list 'I)))
           (rating (cadr (assoc 'rating fs-entry)))
           (titles (cdr (assoc 'titles fs-entry)))
           (file (file-name film))     (runtime (get-runtime film)))
      (when inf-list
        (push (append (mapcar
                       #'(lambda (x) (if (keywordp x) x (getf inf-list x)))
                       '(:title T :short S :desc D :genre G :min-age R
                         :errors O :sub-titles U :audio A :imdb I
                         :timestamp TS))
                      (list :name name :hdsd hdsd :rating rating :file file
                            :runtime runtime :titles titles))
              *films*)))))

(defun html-header (s)
  (format s "<html>~%<head><title>~a</title></head>
<body style=\"font-size: 200%;\">~%<h1>~:*~a</h1>~%" (cv :dest-title)))

(defun remove-title-from-titles (e)
  (let ((title  (getf e :title))
        (titles (getf e :titles)))
    (setf (getf e :titles)
          (remove-if #'(lambda (x) (string-equal title x)) titles))))

(defun html-entry (e s)
  (remove-title-from-titles e)
  (format s "~{<hr>~%<div class=\"film\">
  ~a~@[ (~{~a~^, ~})~]<br>
  ~a (in ~a)<br>
  ~a<br>
  Audio: ~a // Subtitles: ~a // Rating: ~a // Runtime: ~a // File: ~a //
  Added: ~a // <a href=\"https://www.imdb.com/title/~a/\">IMDB</a>
</div>~}" (mapcar #'(lambda (x) (getf e x))
                  '(:title :titles :short :hdsd :desc :audio :sub-titles
                    :rating :runtime :file :timestamp :imdb))))

(defun html-footer (s)
  (format s "</body>~%</html>~%"))

(defun make-html ()
  (setf *films*
        (sort *films* #'string> :key #'(lambda (x) (getf x :timestamp))))
  (with-open-file (out (cv :dest-file)
                       :direction :output :if-exists :supersede)
    (html-header out)
    (dolist (film *films*)
      (html-entry film out))
    (html-footer out)))

(defun print-missing ()
  (format t "~:[~;Entries with missing information:~%~]~:*~{~a~%~}" *missing*))

(defun main ()
  "Main program."
  (if (second sb-ext:*posix-argv*)
      (setf *config-file* (second sb-ext:*posix-argv*)))
  (read-config)
  (gather-infos)
  (make-html)
  (print-missing))

;; Local Variables:
;; pm/slime-auto-load: t
;; End:
