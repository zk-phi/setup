(require 'setup-utils)

(defvar setup-delay-interval 0.1
  "Delay for delayed setup `!-'.")

(defvar setup-delay-with-threads nil
  "When non-nil, delayed setup is evaluated with threads, instead
of run-with-idle-timer. This may break setup process if it's not
thread-safe.

This is an experimental feature and interface may change in
future versions.")

(defvar setup-delay-silent nil
  "When non-nil, delayed setup does not message.")

(defmacro setup-delay--initialize ()
  `(progn
     (defvar setup-delay--queue nil)
     (defvar setup-delay--priority-queue nil)))

(defmacro setup-delay--after-init ()
  `(progn
     (setq setup-delay--queue (nconc setup-delay--priority-queue setup-delay--queue))
     ,(unless setup-delay-with-threads
        `(defconst setup-delay--timer-object
           (when setup-delay--queue
             (run-with-timer ,setup-delay-interval ,setup-delay-interval
                             (lambda ()
                               (when setup-delay--queue
                                 (eval (pop setup-delay--queue)))
                               (unless setup-delay--queue
                                 (message ">> [init] all delayed setup completed.")
                                 (cancel-timer setup-delay--timer-object)))))))
     ,(when setup-delay-with-threads
        '(when setup-delay--queue
           (make-thread
            (lambda ()
              (while setup-delay--queue
                (thread-yield)
                (eval (pop setup-delay--queue)))
              (message ">> [init] all delayed setup completed.")))))))

;; API
(defmacro !- (&rest body)
  "Eval BODY when Emacs started up with slight delay. This works
like a pseudo asynchronous process."
  (if (not (setup--byte-compiling-p))
      `(progn ,@body)
    (let* ((place (cond ((eq (car body) :prepend)
                         (pop body)
                         'setup-delay--priority-queue)
                        (t
                         'setup-delay--queue)))
           (form (cond (setup-delay-silent
                        `(let ((inhibit-message t)) ,@body))
                       ((cadr body)
                        `(progn ,@body))
                       (t
                        (car body)))))
      `(push ',(macroexpand-all form) ,place))))

(provide 'setup-delay)
