(defvar setup-use-load-history-tracker nil
  "When non-nil, track load history and report after init.")

(defmacro setup-tracker--initialize ()
  (when setup-use-load-history-tracker
    '(progn
       (defvar setup-tracker--level 0)
       (defvar setup-tracker--parents nil)
       (defvar setup-tracker--times nil)
       (when load-file-name
         (push load-file-name setup-tracker--parents)
         (push (current-time) setup-tracker--times)
         (setq setup-tracker--level (1+ setup-tracker--level)))
       (add-variable-watcher
        'load-file-name
        (lambda (_ v &rest __)
          (cond ((equal v (car setup-tracker--parents))
                 nil)
                ((equal v (cadr setup-tracker--parents))
                 (setq setup-tracker--level (1- setup-tracker--level))
                 (let* ((now (current-time))
                        (start (pop setup-tracker--times))
                        (elapsed (+ (* (- (nth 1 now) (nth 1 start)) 1000)
                                    (/ (- (nth 2 now) (nth 2 start)) 1000))))
                 (with-current-buffer (get-buffer-create "*setup-tracker*")
                   (save-excursion
                     (goto-char (point-min))
                     (dotimes (_ setup-tracker--level) (insert "> "))
                     (insert
                      (file-name-nondirectory (pop setup-tracker--parents))
                      " (" (number-to-string elapsed) " msec)\n")))))
                (t
                 (push v setup-tracker--parents)
                 (push (current-time) setup-tracker--times)
                 (setq setup-tracker--level (1+ setup-tracker--level)))))))))

(defmacro setup-tracker--after-init ()
  (when setup-use-load-history-tracker
    '(display-buffer "*setup-tracker*")))

(provide 'setup-tracker)
