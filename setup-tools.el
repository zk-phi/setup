(require 'setup-utils)

(defun setup--list->tuples (lst)
  (when lst (cons (cons (car lst) (cadr lst)) (setup--list->tuples (cddr lst)))))
(defmacro setup-keybinds (keymap &rest binds)
  "Add BINDS to KEYMAP and return KEYMAP. If KEYMAP is nil, add
to the global map instead. Each element in BINDS must be a list
of the form (KEYS DEF KEYS DEF ...)  where KEYS can be one of a
string accepted by `kbd', an event accepted by `define-key', or a
list of above. DEF can be an arbitrary object that `define-key'
accepts, or a list of the form (\"FILE\" THENCOMMAND :optional
ELSECOMMAND]). In that case, bind THENCOMMAND when FILE exists,
or ELSECOMMAND otherwise."
  (declare (indent 1))
  `(let ((kmap ,(if (null keymap) `(current-global-map) keymap)))
     ,@(mapcar
        (lambda (bind)
          (let* ((keys (eval (car bind)))
                 (def (eval (cdr bind)))
                 (command (cond ((not (and (listp def) (stringp (car def))))
                                 `',def)
                                ((setup--locate-library (car def))
                                 `',(cadr def))
                                (t
                                 `',(or (nth 2 def) 'ignore)))))
            (cond
             ((listp keys)
              `(progn
                 ,@(mapcar (lambda (k)
                             (if (stringp k)
                                 `(define-key kmap ,(kbd k) ,command)
                               `(define-key kmap ,k ,command)))
                           keys)))
             (t
              (if (stringp keys)
                  `(define-key kmap ,(kbd keys) ,command)
                `(define-key kmap ,keys ,command))))))
        (setup--list->tuples binds))
     kmap))

(defmacro setup-hook (hook &rest exprs)
  "Add (lambda () ,@exprs) to HOOK. If the first expression of
EXPRS is :oneshot, then the hook will be removed once called. If
EXPRS is just a symbol, add it without wrapping with
`lambda'. HOOK must be already declared."
  (declare (indent 1))
  (let* ((oneshotp (eq (car exprs) :oneshot))
         (sym (and oneshotp (gensym "setup-")))
         (fn (cond (oneshotp
                    `(lambda () ,@(cdr exprs) (remove-hook ,hook ,sym)))
                   ((eq (caar exprs) 'quote)
                    (car exprs))
                   (t
                    `(lambda () ,@exprs)))))
    `(let ((oldvalue (when (default-boundp ,hook) (default-value ,hook))))
       ,(when oneshotp `(defvar ,sym ,fn))
       (if (and oldvalue
                (or (not (consp oldvalue)) (eq (car oldvalue) 'lambda)))
           (set-default ,hook (list ,fn oldvalue))
         (set-default ,hook (cons ,fn oldvalue))))))

(defmacro setup-with-delayed-redisplay (&rest body)
  `(let ((original-redisplay-fn (symbol-function 'redisplay)))
     ;; why is "flet" obsolete ?
     (unwind-protect
         (progn
           (fset 'redisplay (lambda (&rest _) nil))
           ,@body)
       (fset 'redisplay original-redisplay-fn))))

(defmacro setup-silently (&rest body)
  "Eval body without messages"
  `(let ((inhibit-message t))
     ,@body))

(provide 'setup-tools)
