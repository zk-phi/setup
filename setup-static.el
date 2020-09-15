(defun setup--make-anaphoric-env (value)
  "Internal function for anaphoric macros."
  `((,'\, . (lambda (&rest body) `',(funcall `(lambda (it) ,@body) ',value)))))

(defalias '! 'eval-when-compile)

(defmacro !if (test then &rest else)
  "Like `if' but anaphoric (TEST value can be referred with
`,it') and expanded statically."
  (declare (indent 2))
  (setq test (eval test))
  (macroexpand-all (if test then (if (cadr else) `(progn,@else) (car else)))
                   (setup--make-anaphoric-env test)))

(defmacro !when (test &rest body)
  "Like `when' but anaphoric (TEST value can be referred with
`,it') and expanded statically."
  (declare (indent 1))
  (setq test (eval test))
  (macroexpand-all (when test (if (cadr body) `(progn ,@body) (car body)))
                   (setup--make-anaphoric-env test)))

(defmacro !unless (test &rest body)
  "Like `unless' but anaphoric (TEST value can be referred with
`,it') and expanded statically."
  (declare (indent 1))
  (setq test (eval test))
  (macroexpand-all (unless test (if (cadr body) `(progn ,@body) (car body)))
                   (setup--make-anaphoric-env test)))

(defmacro !cond (&rest clauses)
  "Like `cond' but anaphoric (TEST value can be referred with
`,it') and expanded statically."
  (let (val)
    (while (and clauses
                (not (setq val (eval (caar clauses)))))
      (setq clauses (cdr clauses)))
    (setq clauses (cdar clauses))
    (macroexpand-all (if (cadr clauses) `(progn ,@clauses) (car clauses))
                     (setup--make-anaphoric-env val))))

(defmacro !case (expr &rest clauses)
  "Like `case' but anaphoric (TEST value can be referred with
`,it') and expanded statically."
  (declare (indent 1))
  (setq expr (eval expr))
  (while (and clauses
              (let ((keylist (caar clauses)))
                (and (not (and (null (cdr clauses))
                               (memq keylist '(t otherwise))))
                     (not (and (consp keylist)
                               (memql expr keylist)))
                     (not (and (atom keylist)
                               (eql expr keylist))))))
    (setq clauses (cdr clauses)))
  (setq clauses (cdar clauses))
  (macroexpand-all (if (cadr clauses) `(progn ,@clauses) (car clauses))
                   (setup--make-anaphoric-env expr)))

(defmacro !foreach (list &rest body)
  "Eval BODY for each elements in LIST. The current element can
  be referred with `,it'."
  (declare (indent 1))
  `(progn ,@(mapcar
             (lambda (elem)
               (macroexpand-all
                (if (cadr body) `(progn ,@body) (car body))
                (setup--make-anaphoric-env elem)))
             (eval list))))

(provide 'setup-static)
