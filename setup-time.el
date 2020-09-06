(defmacro setup-time--initialize ()
  `(defconst setup-time--start-time (current-time)))

(defmacro setup-time--after-init ()
  `(message ">> [init] TOTAL: %d msec"
            (let ((now (current-time)))
              (+ (* (- (nth 1 now) (nth 1 setup-time--start-time)) 1000)
                 (/ (- (nth 2 now) (nth 2 setup-time--start-time)) 1000)))))

(provide 'setup-time)
