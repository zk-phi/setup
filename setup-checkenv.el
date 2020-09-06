(defvar setup-environ-warning-alist
  '(((system-name)
     . "Init script is not compiled with this system.")
    (window-system
     . "Init script is not compiled with this window system.")
    (user-login-name
     . "Init script is not compiled by this user.")
    (emacs-version
     . "Init script is not compiled with this version of Emacs."))
  "Alist of expressions Vs messages. Each expressions are
evaluated everytime on startup, and when some of them evaluates
to a different value from the value evaluated during compile,
warning message are shown.")

(defmacro setup--checkenv ()
  `(unless (and ,@(mapcar (lambda (pair)
                            `(or (equal ',(eval (car pair)) ,(car pair))
                                 (y-or-n-p
                                  ,(concat (cdr pair) " Really continue ? "))))
                          setup-environ-warning-alist))
     (error "Setup canceled.")))

(provide 'setup-checkenv)
