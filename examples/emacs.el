;; Nice to have:
;; Press F6 to jump from FilmSearch message directly to search entry.
;; Press F7 to start VLC and jump from make-db message directly to info file.

(defun pm/goto-fs-entry ()
  (interactive)
  (re-search-forward "imdb=\\(tt[0-9]*\\)")
  (let ((imdb (match-string 1)))
    (find-file "~/fs/entries")
    (goto-char (point-min))
    (re-search-forward (format "(id \"%s\")" imdb))))

(defun pm/check-film ()
  "Fill A line for audio tracks and U line for subtitles."
  (interactive)
  (let ((n (string-trim-right (thing-at-point 'line t))))
    (forward-line)
    (apply 'start-process (append (list "vlc" nil "vlc")
                                  (file-expand-wildcards
                                   (format "%s/%s.m??" pm/video-dir n) t)))
    (find-file (format "%s/%s.inf" pm/video-dir n))))

(define-key gnus-article-mode-map [f6] 'pm/goto-fs-entry)
(define-key gnus-article-mode-map [f7] 'pm/check-film)
