;;; setup.el --- helper macros to write faster, portable and robust init script

;; Copyright (C) 2013-2015 zk_phi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: zk_phi
;; URL: http://zk-phi.github.io/
;; Version: 1.0.6

;;; Commentary:

;; Require this script
;;
;;   (require 'setup)
;;
;; and put
;;
;;   (setup-initialize)
;;
;; at the beginning of your init script. Then you can use following macros :
;;
;;   setup-eval, setup-if, setup-when, setup-unless, setup-case,
;;   setup-cond setup, setup-include, setup-lazy, setup-after,
;;   setup-expecting, setup-in-idle, setup-keybinds, setup-hook

;; For more informations, see "Readme".

;;; Change Log:

;; 1.0.0 first released
;; 1.0.1 add setup-hook macro
;; 1.0.2 change anaphoric macros' API
;; 1.0.3 add delayed execution "!-"
;; 1.0.4 Fix minor bugs
;; 1.0.5 add option setup-silent and macro setup-silently
;; 1.0.6 add macro setup-fallback, setup-with-delayed-redisplay and setup-simulate-absense

;;; Code:

(require 'find-func)    ; find-library-name
(require 'macroexp)     ; macroexpand-all
(require 'bytecomp)     ; byte-compile-current-file, byte-compile-warn

(defconst setup-version "1.0.6")

;; + customizable vars

(defvar setup-include-allow-runtime-load 'undef
  "When non-nil, allow `setup-include' to load libraries in
runtime, if the source file is not found.")

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

(defvar setup-delay-interval 0.1
  "Delay for delayed setup `!-'.")

(defvar setup-delay-with-threads nil
  "When non-nil, delayed setup is evaluated with threads, instead
of run-with-idle-timer. This may break setup process if it's not
thread-safe.

This is an experimental feature and interface may change in
future versions.")

(defvar setup-silent nil
  "When non-nil, setup and setup-include does not message after
loading libraries.")

(defvar setup-delay-silent nil
  "When non-nil, delayed setup does not message.")

(defvar setup-use-profiler nil
  "When non-nil, profile setup time and report after init.")

(defvar setup-disable-magic-file-name nil
  "When non-nil, file-name-handler-alist is set nil during
startup for performance.")

(defvar setup-enable-gc-threshold-hacks nil
  "When non-nil, enlarge gc-threshold during startup.")

(defvar setup-idle-threshold 13
  "Idle time threshold in seconds to invoke `setup-in-idle'.")

;; + font-lock keywords for elisp mode

(eval-after-load "lisp-mode"
  '(font-lock-add-keywords
    'emacs-lisp-mode
    '(("(\\(setup\\(?:-\\(?:in\\(?:clude\\|-idle\\)\\|after\\|expecting\\|lazy\\)\\)?\\)\\_>"
       1 font-lock-keyword-face)
      ("(\\(!-?\\(?:it\\_>\\|foreach\\_>\\|when\\_>\\|if\\_>\\|unless\\_>\\|cond\\_>\\|case\\_>\\)?\\)"
       1 font-lock-keyword-face))))

;; + initialize

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

(defmacro setup-initialize ()
  "This macro is replaced with an initializing routine when expanded.
*PUT THIS MACRO AT THE VERY BEGINNING OF YOUR INIT SCRIPT.*"
  `(progn
     (defconst setup-last-compiled-time ,(format-time-string "%D %T"))
     ;; setup stopwatch
     (defconst setup--start-time (current-time))
     (defvar setup--delay-queue nil)
     (defvar setup--delay-priority-queue nil)
     ;; check and warn about environ
     (unless (and ,@(mapcar (lambda (pair)
                              `(or (equal ',(eval (car pair)) ,(car pair))
                                   (y-or-n-p
                                    ,(concat (cdr pair) " Really continue ? "))))
                            setup-environ-warning-alist))
       (error "Setup canceled."))
     ;; setup profiler
     ,@(when setup-use-profiler
         '((require 'profiler)
           (profiler-start 'cpu)))
     (add-hook 'after-init-hook
               (lambda  ()
                 (setq setup--delay-queue (nconc setup--delay-priority-queue setup--delay-queue))
                 ,(unless setup-delay-with-threads
                    `(defconst setup--delay-timer-object
                       (when setup--delay-queue
                         (run-with-timer ,setup-delay-interval ,setup-delay-interval
                                         (lambda ()
                                           (when setup--delay-queue
                                             (eval (pop setup--delay-queue)))
                                           (unless setup--delay-queue
                                             (message ">> [init] all delayed setup completed.")
                                             (cancel-timer setup--delay-timer-object)))))))
                 ,(when setup-delay-with-threads
                    '(when setup--delay-queue
                       (make-thread
                        (lambda ()
                          (while setup--delay-queue
                            (thread-yield)
                            (eval (pop setup--delay-queue)))
                          (message ">> [init] all delayed setup completed.")))))
                 ,(when setup-disable-magic-file-name
                    `(unless file-name-handler-alist
                       (setq file-name-handler-alist ',file-name-handler-alist)))
                 ,(when setup-enable-gc-threshold-hacks
                    '(setq gc-cons-threshold  16777216 ; 16mb
                           gc-cons-percentage 0.1))
                 ,@(when setup-use-profiler
                     '((profiler-report)
                       (profiler-stop)))
                 (message ">> [init] TOTAL: %d msec"
                          (let ((now (current-time)))
                            (+ (* (- (nth 1 now) (nth 1 setup--start-time)) 1000)
                               (/ (- (nth 2 now) (nth 2 setup--start-time)) 1000))))))
     ,(when setup-enable-gc-threshold-hacks
        '(setq gc-cons-threshold  most-positive-fixnum
               gc-cons-percentage 0.6))
     ,(when setup-disable-magic-file-name
        '(setq file-name-handler-alist nil))))

;; + load and configure libraries

(defvar setup--simulated-absent-libraries nil)
(defun setup--locate-library (file)
  (and (not (member file setup--simulated-absent-libraries))
       (locate-library file)))

(defun setup--byte-compiling-p ()
  "Return non-nil iff byte-compile is in progress."
  (and (boundp 'byte-compile-current-file)
       byte-compile-current-file))

(defun setup--declare-defuns (body)
  "Declare symbols defun-ed in the forms BODY, to avoid
`undefined function` warnings."
  (dolist (form body)
    (when (eq (car form) 'defun)
      (defalias (cadr form) `(lambda ,(caddr form) nil)))))

(defmacro setup (file &rest body)
  "Load FILE and evaluate BODY, iff FILE exists."
  (declare (indent defun))
  (let ((feature (intern file))
        (libfile (setup--locate-library file)))
    (cond (libfile
           ;; load during compile to avoid warnings
           (when (setup--byte-compiling-p)
             (let ((byte-compile-warnings nil))
               (or (ignore-errors (require feature nil t)) (load libfile t t)))
             (setup--declare-defuns body))
           `(let* ((beg-time ,(unless setup-silent '(current-time)))
                   (loaded ,(if (featurep feature)
                                `(unless (featurep ',feature)
                                   (load ,libfile t t))
                              `(load ,libfile t t))))
              (condition-case err
                  (progn ,@body
                         ,(unless setup-silent
                            `(when loaded
                               (message ">> [init] %s: loaded in %d msec" ,file
                                        (let ((now (current-time)))
                                          (+ (* (- (nth 1 now) (nth 1 beg-time)) 1000)
                                             (/ (- (nth 2 now) (nth 2 beg-time)) 1000)))))))
                (error (message "XX [init] %s: %s" ,file (error-message-string err))))))
          (t
           (byte-compile-warn "%s not found" file)
           nil))))

(defun setup--read-all (stream)
  (ignore-errors (cons (read stream) (setup--read-all stream))))
(defmacro setup-include (file &rest body)
  "Like `setup', but includes FILE to the compiled init script
instead of loading it."
  (declare (indent 1))
  (let* ((feature (intern file))         ; string->symbol
         (libfile (setup--locate-library file))
         (srcfile (and libfile
                       (or (ignore-errors (find-library-name file))
                           (expand-file-name file)))))
    (cond ((or (not (setup--byte-compiling-p))
               (eq setup-include-allow-runtime-load 'always))
           (macroexpand-all `(setup ,file ,@body)))
          ((and srcfile (file-exists-p srcfile))
           ;; load during compile
           (let ((byte-compile-warnings nil))
             (or (ignore-errors (require feature nil t)) (load libfile t t)))
           (setup--declare-defuns body)
           (let ((history (load-history-filename-element file))
                 (source (with-temp-buffer
                           (insert-file-contents srcfile)
                           (setup--read-all (current-buffer)))))
             `(let ((beg-time ,(unless setup-silent '(current-time))))
                (unless (featurep ',feature)
                  ;; The author of .emacs considered not responsible
                  ;; for the warnings in included libraries
                  (with-no-warnings
                    ,(macroexpand-all
                      `(progn ,@source)
                      `((eval-when-compile
                          . (lambda (&rest body) `',(eval `(progn ,@body))))
                        (eval-and-compile
                          . (lambda (&rest body) (eval `(progn ,@body)) `(progn ,@body))))))
                  (push ',history load-history)
                  (do-after-load-evaluation ,libfile))
                (condition-case err
                    (progn ,@body
                           ,(unless setup-silent
                              `(message ">> [init] %s: loaded in %d msec" ,file
                                        (let ((now (current-time)))
                                          (+ (* (- (nth 1 now) (nth 1 beg-time)) 1000)
                                             (/ (- (nth 2 now) (nth 2 beg-time)) 1000))))))
                  (error (message "XX [init] %s: %s" ,file (error-message-string err)))))))
          ((and libfile
                (or (not (setup--byte-compiling-p))
                    (if (eq setup-include-allow-runtime-load 'undef)
                        (setq setup-include-allow-runtime-load
                              (y-or-n-p (concat "Some libraries are not includable."
                                                " Load them during runtime ?")))
                      setup-include-allow-runtime-load)))
           `(setup ,file ,@body))
          (t
           (byte-compile-warn "%s not found" file)
           nil))))

;; + autoload libraries

(defmacro setup-lazy (triggers file &rest body)
  "Load FILE and evaluate BODY when a function listed in TRIGGERS
is invoked, if FILE exists."
  (declare (indent defun))
  (cond ((setup--locate-library file)
         (let ((triggers (eval triggers))
               (preparation (when (and body (eq (car body) :prepare))
                              (prog1 (cadr body) (setq body (cddr body))))))
           ;; load during compile
           (when (setup--byte-compiling-p)
             (eval preparation)
             (let ((byte-compile-warnings nil))
               (or (ignore-errors (require (intern file) nil t)) (load file t t)))
             (setup--declare-defuns body))
           `(progn
              ,@(mapcar (lambda (trigger)
                          `(autoload ',trigger ,file nil t))
                        triggers)
              ,(when preparation
                 `(condition-case err
                      ,preparation
                    (error (message "XX [init] %s: %s" ,file (error-message-string err)))))
              (eval-after-load ,file
                ',(macroexpand-all
                   `(condition-case err
                        (progn ,@body
                               ,(unless setup-silent `(message "<< [init] %s: loaded" ,file)))
                      (error (message "XX [init] %s: %s" ,file (error-message-string err)))))))))
        (t
         (byte-compile-warn "%s not found" file)
         nil)))

;; + pre/post-load evaluation

(defmacro setup-after (file &rest body)
  "Eval BODY after FILE is loaded."
  (declare (indent defun))
  (let ((feature (intern file))
        (libfile (setup--locate-library file)))
    (when libfile
      ;; load during compile
      (when (setup--byte-compiling-p)
        (let ((byte-compile-warnings nil))
          (or (ignore-errors (require feature nil t)) (load libfile t t)))
        (setup--declare-defuns body))
      `(eval-after-load ,file
         ',(macroexpand-all
            `(condition-case err ,(if (cadr body) `(progn ,@body) (car body))
               (error (message "XX [init] %s: %s" ,file (error-message-string err)))))))))

(defmacro setup-fallback (file &rest body)
  "Eval BODY only when FILE does not exist."
  (declare (indent defun))
  (unless (setup--locate-library file)
    (when (setup--byte-compiling-p)
      (setup--declare-defuns body))
    `(condition-case err ,(if (cadr body) `(progn ,@body) (car body))
       (error (message "XX [init] %s" (error-message-string err))))))

(defmacro setup-expecting (file &rest body)
  "Eval BODY only when FILE exists."
  (declare (indent defun))
  (cond ((setup--locate-library file)
         (when (eq (car body) :fallback)
           (setq body (cddr body)))
         (when (setup--byte-compiling-p)
           (setup--declare-defuns body))
         `(condition-case err ,(if (cadr body) `(progn ,@body) (car body))
            (error (message "XX [init] %s: %s" ,file (error-message-string err)))))
        ((eq (car body) :fallback)
         (when (setup--byte-compiling-p)
           (setup--declare-defuns (list (cadr body))))
         `(condition-case err ,(cadr body)
            (error (message "XX [init] %s" (error-message-string err)))))))

(defmacro setup-in-idle (file)
  "Load FILE during idle-time."
  (let* ((feature (intern file))
         (libfile (setup--locate-library file)))
    (when libfile
      ;; load during compile
      (when (setup--byte-compiling-p)
        (let ((byte-compile-warnings nil))
          (or (ignore-errors (require feature nil t)) (load libfile t t))))
      `(run-with-idle-timer ,setup-idle-threshold nil
                            (lambda ()
                              ,(if (featurep feature)
                                   `(unless (featurep ',feature)
                                      (load ,libfile t t))
                                 `(load ,libfile t t)))))))

;; + compile-time execution / delayed execution

(defun setup--make-anaphoric-env (value)
  "Internal function for anaphoric macros."
  `((,'\, . (lambda (&rest body) `',(funcall `(lambda (it) ,@body) ',value)))))

(defmacro ! (sexp)
  "Eval SEXP during compile."
  `',(eval sexp))

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

(defmacro !- (&rest body)
  "Eval BODY when Emacs started up with slight delay. This works
like a pseudo asynchronous process."
  (if (not (setup--byte-compiling-p))
      `(progn ,@body)
    (let* ((place (cond ((eq (car body) :prepend)
                         (pop body)
                         'setup--delay-priority-queue)
                        (t
                         'setup--delay-queue)))
           (form (cond (setup-delay-silent
                        `(let ((inhibit-message t)) ,@body))
                       ((cadr body)
                        `(progn ,@body))
                       (t
                        (car body)))))
      `(push ',(macroexpand-all form) ,place))))

;; + other utilities

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
  "Add (lambda () ,@exprs) to HOOK. If EXPRS is just a symbol,
add it without wrapping with `lambda'. HOOK must be already
declared."
  (declare (indent 1))
  `(let ((oldvalue (when (default-boundp ,hook) (default-value ,hook))))
     (if (or (not (listp oldvalue)) (eq (car oldvalue) 'lambda))
         (set-default ,hook (list ,(if (eq (caar exprs) 'quote)
                                       (car exprs)
                                     `(lambda () ,@exprs))
                                  oldvalue))
       (set-default ,hook (cons ,(if (eq (caar exprs) 'quote)
                                     (car exprs)
                                   `(lambda () ,@exprs))
                                oldvalue)))))

(defun setup-byte-compile-file (&optional file cmd)
  "Byte-compile FILE in a clean environment (emacs -q)."
  (interactive)
  (when (and (buffer-modified-p)
             (y-or-n-p (format "Save buffer %s first ? "
                               (file-name-nondirectory buffer-file-name))))
    (save-buffer))
  (let ((tmpfile (make-temp-file "setup"))
        (filename (or file
                      (let* ((absfile (expand-file-name buffer-file-name))
                             (dir (file-name-directory absfile))
                             (file (file-name-nondirectory absfile)))
                        (read-file-name "File: " dir nil t file))))
        (command (or cmd
                     (read-string
                      "Emacs command: "
                      (if (file-exists-p "/Applications/Emacs.app/Contents/MacOS/Emacs")
                          "/Applications/Emacs.app/Contents/MacOS/Emacs"
                        "emacs")))))
    (if (not window-system)
        (shell-command
         (format "%s --batch -eval \" (byte-compile-file \\\"%s\\\")\""
                 command filename))
      (let ((returncode (shell-command
                         (format "%s -q -eval \"
 (progn
   (byte-compile-file \\\"%s\\\")
   (switch-to-buffer \\\"*Messages*\\\")
   (read-only-mode -1)
   (insert \\\"------\n\\\" (with-current-buffer \\\"*Compile-Log*\\\" (buffer-string)))
   (write-region 1 (1+ (buffer-size)) \\\"%s\\\")
   (kill-emacs
     (if (string-match
           (regexp-opt '(\\\"error:\\\" \\\"warning:\\\"))
           (buffer-string))
         1
       0)))\""
                                 command filename tmpfile))))
        (with-current-buffer (get-buffer-create "*Compile-Log*")
          (compilation-mode)
          (let ((buffer-read-only nil))
            (insert-file-contents tmpfile))
          (delete-file tmpfile)
          (goto-char (point-max)))
        (unless (zerop returncode)
          (display-buffer "*Compile-Log*"))))))

;; + debugging utils

(defmacro setup-simulate-absense (file)
  (push file setup--simulated-absent-libraries)
  nil)

;; + (provide)

(provide 'setup)

;;; setup.el ends here
