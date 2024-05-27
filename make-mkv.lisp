":"; C="exec sbcl --noinform --end-runtime-options --disable-debugger"
":"; $C --load "$0" --eval '(main)' --quit --end-toplevel-options "$@"

;; Make small MKV-files for Smart-TVs

;; For teletext, see https://ffmpeg.org/ffmpeg-codecs.html#Options-9

(dolist (package '(:cl-ppcre :trivial-file-size :cl-diskspace))
  (require package))

(unless (constantp '+defaults+)
  (defconstant +defaults+
    `(:vdr-dir     "/var/spool/video"
      :dest-dir    "/opt/video"
      :in-opts     ("-fix_sub_duration" "-txt_page" "subtitle")
      :out-opts    ("-map" "0" "-c:v" "libx264" "-preset" "faster"
                    "-crf" "20" "-c:a" "copy" "-c:s" "dvdsub" "-palette"
                    ,(format nil "000000~15@{,~a~:*~}" "ffffff") "-dn")
      :cmd         "nice"
      :nice-args  ("-n19" "ffmpeg"))))

(defvar *config-file* "~/.config/make-mkv.conf" "configuration file")
(defvar *config*  nil "configuration list")
(defvar *src-dir* nil "directory with info and TS files")
(defvar *channel* nil "channel name")
(defvar *name*    nil "prefix for the files, name of the film")
(defvar *imdb-id* nil "IMDB identifier")

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

(defun new-ts ()
  "Check, if new TS is to be converted.
There should be at least 2 candidates, because it’s possible,
that a cutting process is still running.
Side effect: *src-dir* is set."
  (let ((l (sort (directory (format nil "~a/%*/*.rec" (cv :vdr-dir)))
                 #'< :key #'(lambda (x) (file-write-date x)))))
    (if (> (length l) 1)
        (setf *src-dir* (native-namestring (first l))))))

(defun get-name ()
  (let* ((seq (subseq *src-dir* (search "%" *src-dir*)))
         (end (search "/" seq)))
    (subseq seq 1 end)))

(defun split-name+imdb-id ()
  (multiple-value-bind (res n+i) (cl-ppcre:scan-to-strings "^(.*)-(tt[0-9]+)$" *name*)
    (if res
        (setf *name*    (elt n+i 0)
              *imdb-id* (elt n+i 1)))))

(defun gather-infos ()
  (with-open-file (in (strmerge *src-dir* "info"))
    (let ((first-line (read-line in)))
      (setf *channel* (subseq first-line
                              (1+ (search " " first-line :start2 2))))))
  (setf *name* (get-name))
  (split-name+imdb-id))

(defun my-exit (str arg code)
  (format t str arg)
  (exit :code code))

(defun convert-to-mkv ()
  (let* ((in-file  (strmerge *src-dir* "00001.ts"))
         (prefix   (strmerge (cv :dest-dir) "/" *name*))
         (out-file (strmerge prefix ".mkv"))
         (size     (trivial-file-size:file-size-in-octets in-file))
         (free     (diskspace:disk-available-space (cv :dest-dir))))
    (if (> size free)
        (my-exit "Very low available space: ~d MiB. Exiting...~%"
                 (round (/ free 1024 1024)) 1))
    (if (probe-file out-file)
        (my-exit "File ~a already exists. Exiting...~%" out-file 1))
    (let* ((inf-file  (strmerge prefix ".inf"))
           (log-file  (strmerge prefix ".log"))
           (nice-args (cv :nice-args))
           (args      (append nice-args (cv :in-opts) (list "-i" in-file)
                              (cv :out-opts) (list out-file)))
           (proc      (run-program (cv :cmd) args :output log-file :search t))
           (code      (process-exit-code proc)))
      (when (/= code 0)
        (my-exit "Error, please see ~a.~%" log-file code))
      (rename-file (strmerge *src-dir* "info") inf-file)
      (with-open-file (s inf-file :direction :output :if-exists :append)
        (format s "I ~a~%" *imdb-id*))
      (format t "Successfully created ~a~%Arguments were:~%~a~%"
              out-file (nthcdr (length nice-args) args)))))

(defun main ()
  "Main program."
  (if (second sb-ext:*posix-argv*)
      (setf *config-file* (second sb-ext:*posix-argv*)))
  (read-config)
  (when (new-ts)
    (gather-infos)
    (convert-to-mkv)
    (delete-directory *src-dir* :recursive t)))

;; Local Variables:
;; pm/slime-auto-load: t
;; End:
