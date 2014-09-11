;;; setup.el --- helper macros to write faster, portable and robust init script

;; Copyright (C) 2013 zk_phi

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
;; URL: http://hins11.yu-yake.com/
;; Version: 1.0.1

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

;;; Code:

(require 'find-func)
(require 'macroexp)

(defconst setup-version "1.0.1")

;; + customizable vars

(defvar setup-include-allow-runtime-load 'undef
  "When non-nil, setup-include can load libraries in runtime, if
the source file is not found.")

(defvar setup-environ-warning-alist
  '(((system-name)
     . "Init script is not compiled with this system.")
    (window-system
     . "Init script is not compiled with this window system.")
    (user-login-name
     . "Init script is not compiled by this user.")
    (emacs-version
     . "Init script is not compiled with this version of Emacs."))
  "Alist of expressions that must be evaluated to the \"equal\"
value between compile-time and runtime, and warning message shown
when the value differes.")

;; + font-lock keywords for elisp mode

(eval-after-load "lisp-mode"
  '(font-lock-add-keywords
    'emacs-lisp-mode
    '(("(\\(setup\\(?:-\\(?:in\\(?:clude\\|-idle\\)\\|after\\|expecting\\|lazy\\)\\)?\\)\\_>"
       1 font-lock-keyword-face)
      ("(\\(!\\(?:when\\_>\\|if\\_>\\|unless\\_>\\|cond\\_>\\|case\\_>\\|[^\s\t\n]\\)?\\)"
       1 font-lock-keyword-face))))

;; + initialize

(defmacro setup-initialize ()
  "This macro is replaced with an initializing routine during compile.
PUT THIS MACRO AT THE VERY BEGINNING OF YOUR INIT SCRIPT."
  `(progn
     ;; setup stopwatch
     (let ((beg-time (current-time)))
       (add-hook 'after-init-hook
                 `(lambda  ()
                    (message ">> [init] TOTAL: %d msec"
                             (let ((now (current-time)))
                               (+ (* (- (nth 1 now) ,(nth 1 beg-time)) 1000)
                                  (/ (- (nth 2 now) ,(nth 2 beg-time)) 1000)))))))
     ;; check and warn about environ
     (unless (and ,@(mapcar (lambda (pair)
                              `(or (equal ',(eval (car pair)) ,(car pair))
                                   (y-or-n-p
                                    ,(concat (cdr pair) " Really continue ? "))))
                            setup-environ-warning-alist))
       (error "Setup canceled."))))

;; + compile-time execution

(defmacro setup-eval (sexp)
  "Eval during compile."
  `',(eval sexp))

(defmacro setup-if (test then &rest else)
  "Like \"if\" but anaphoric and expanded during compile."
  (declare (indent 2))
  (setq test (eval test))
  (macroexpand-all (if test then `(progn ,@else)) `((it . (lambda () ,test)))))

(defmacro setup-when (test &rest body)
  "Like \"when\" but anaphoric and expanded during compile."
  (declare (indent 1))
  (setq test (eval test))
  (macroexpand-all (when test `(progn ,@body)) `((it . (lambda () ,test)))))

(defmacro setup-unless (test &rest body)
  "Like \"unless\" but anaphoric and expanded during compile."
  (declare (indent 1))
  (setq test (eval test))
  (macroexpand-all (unless test `(progn ,@body)) `((it . (lambda () ,test)))))

(defmacro setup-cond (&rest clauses)
  "Like \"cond\" but anaphoric and expanded during compile."
  (let (val)
    (while (and clauses
                (not (setq val (eval (caar clauses)))))
      (setq clauses (cdr clauses)))
    (macroexpand-all `(progn ,@(cdar clauses)) `((it . (lambda () ,val))))))

(defmacro setup-case (expr &rest clauses)
  "Like \"case\" but anaphoric and expanded during compile."
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
  (macroexpand-all `(progn ,@(cdar clauses)) `((it . (lambda () ,expr)))))

(defalias '! 'setup-eval)
(defalias '!if 'setup-if)
(defalias '!when 'setup-when)
(defalias '!cond 'setup-cond)
(defalias '!case 'setup-case)
(defalias '!unless 'setup-unless)

;; + load and configure libraries

(defmacro setup (file &rest body)
  "Load FILE. Iff succeeded, eval BODY."
  (declare (indent defun))
  (let ((feature (intern file))
        (libfile (locate-library file)))
    (cond (libfile
           ;; load during compile
           (when (and (boundp 'byte-compile-current-file)
                      byte-compile-current-file)
             (or (require feature nil t) (load libfile t t)))
           `(let ((beg-time (current-time)))
              ,(if (featurep feature)
                   `(unless (featurep ',feature)
                      (load ,libfile t t))
                 `(load ,libfile t t))
              (condition-case err
                  (progn ,@body
                         (message ">> [init] %s: succeeded in %d msec" ,file
                                  (let ((now (current-time)))
                                    (+ (* (- (nth 1 now) (nth 1 beg-time)) 1000)
                                       (/ (- (nth 2 now) (nth 2 beg-time)) 1000)))))
                (error (message "XX [init] %s: %s" ,file (error-message-string err))))))
          (t
           (byte-compile-warn "%s not found" file)
           nil))))

(defun setup--read-all (stream)
  (condition-case nil
      (cons (read stream) (setup--read-all stream))
    (error nil)))
(defmacro setup-include (file &rest body)
  "Like \"setup\", but inserts the library source there instead
of loading it during runtime."
  (declare (indent 1))
  (let ((feature (intern file))         ; string->symbol
        (libfile (locate-library file))
        (srcfile (or (ignore-errors (find-library-name file))
                     (expand-file-name file))))
    (cond ((and srcfile (file-exists-p srcfile))
           ;; load during compile
           (when (and (boundp 'byte-compile-current-file)
                      byte-compile-current-file)
             (or (require feature nil t) (load libfile t t)))
           (let ((history (assoc libfile load-history))
                 (source (with-temp-buffer
                           (insert-file-contents srcfile)
                           (setup--read-all (current-buffer)))))
             `(let ((beg-time (current-time)))
                (unless (featurep ',feature)
                  ;; The author of .emacs considered not responsible
                  ;; for the warnings in included libraries
                  (with-no-warnings ,@source)
                  (push ',history load-history)
                  (do-after-load-evaluation ,libfile))
                (condition-case err
                    (progn ,@body
                           (message ">> [init] %s: succeeded in %d msec" ,file
                                    (let ((now (current-time)))
                                      (+ (* (- (nth 1 now) (nth 1 beg-time)) 1000)
                                         (/ (- (nth 2 now) (nth 2 beg-time)) 1000)))))
                  (error (message "XX [init] %s: %s" ,file (error-message-string err)))))))
          ((and libfile
                (or
                 ;; we do not need to ask loading during runtime
                 (not (and (boundp 'byte-compile-current-file)
                           byte-compile-current-file))
                 ;; we do need to ask loading during compile
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
  "Load FILE on TRIGGERS. When loaded, eval BODY."
  (declare (indent defun))
  (cond ((locate-library file)
         (let ((preparation (when (and body (eq (car body) :prepare))
                              (prog1 (cadr body) (setq body (cddr body))))))
           `(progn
              (mapc (lambda (trigger)
                      (autoload trigger ,file nil t))
                    ,triggers)
              ,(when preparation
                 `(condition-case err
                      ,preparation
                    (error (message "XX [init] %s: %s" ,file (error-message-string err)))))
              (eval-after-load ,file
                ',(macroexpand-all
                   `(condition-case err
                        (progn ,@body
                               (message "<< [init] %s: loaded" ,file))
                      (error (message "XX [init] %s: %s" ,file (error-message-string err)))))))))
        (t
         (byte-compile-warn "%s not found" file)
         nil)))

;; + pre/post-load evaluation

(defmacro setup-after (file &rest body)
  "Eval BODY after FILE is loaded."
  (declare (indent defun))
  (when (locate-library file)
    `(eval-after-load ,file
       ',(macroexpand-all
          `(condition-case err (progn ,@body)
             (error (message "XX [init] %s: %s" ,file (error-message-string err))))))))

(defmacro setup-expecting (file &rest body)
  "Eval BODY only when FILE exists."
  (declare (indent defun))
  (cond ((locate-library file)
         (when (eq (car body) :fallback)
           (setq body (cddr body)))
         `(condition-case err (progn ,@body)
            (error (message "XX [init] %s: %s" ,file (error-message-string err)))))
        ((eq (car body) :fallback)
         `(condition-case err ,(cadr body)
            (error (message "XX [init] %s" (error-message-string err)))))))

(defmacro setup-in-idle (file)
  "Load FILE in idle-time."
  (when (locate-library file)
    (let* ((feature (intern file))
           (libfile (locate-library file)))
      ;; load during compile
      (when (and (boundp 'byte-compile-current-file)
                 byte-compile-current-file)
        (or (require feature nil t) (load libfile t t)))
      `(run-with-idle-timer 15 nil
                            (lambda ()
                              ,(if (featurep feature)
                                   `(unless (featurep ',feature)
                                      (load ,libfile t t))
                                 `(load ,libfile t t)))))))

;; + other utilities

(defun setup--list->tuples (lst)
  (when lst
    (cons (cons (car lst) (cadr lst))
          (setup--list->tuples (cddr lst)))))
(defmacro setup-keybinds (keymap &rest binds)
  "Add BINDS to KEYMAP. If KEYMAP is nil, add to the global map
instead. BINDS must be a list of (KEYS DEF KEYS DEF ...) where
KEYS can be one of a string accepted by \"kbd\", an event
accepted by \"define-key\", or a list of above, and DEF can be an
object that \"define-key\" accepts or a list of the
form (\"FILE\" THENCOMMAND :optional ELSECOMMAND])."
  (declare (indent 1))
  `(let ((kmap ,(if (null keymap) `(current-global-map) keymap)))
     ,@(mapcar
        (lambda (bind)
          (let* ((keys (eval (car bind)))
                 (def (eval (cdr bind)))
                 (command (cond ((not (and (listp def) (stringp (car def))))
                                 `',def)
                                ((locate-library (car def))
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
        (setup--list->tuples binds))))

(defmacro setup-hook (hook &rest exprs)
  (declare (indent 1))
  `(add-hook ,hook (lambda () ,@exprs)))

(defun setup-byte-compile-file (&optional file)
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
                        (read-file-name "File: " dir nil t file)))))
    (if (not window-system)
        (shell-command
         (format "emacs --batch -eval \"
 (progn
   (setq byte-compile-dynamic t)
   (byte-compile-file \\\"%s\\\"))\""
                 filename))
      (let ((returncode (shell-command
                         (format "emacs -q -eval \"
 (progn
   (setq byte-compile-dynamic t)
   (byte-compile-file \\\"%s\\\")
   (switch-to-buffer \\\"*Compile-Log*\\\")
   (write-region 1 (1+ (buffer-size)) \\\"%s\\\")
   (kill-emacs
     (if (string-match
           (regexp-opt '(\\\"error:\\\" \\\"warning:\\\"))
           (buffer-string))
         1
       0)))\""
                                 filename tmpfile))))
        (with-current-buffer (get-buffer-create "*Compile-Log*")
          (compilation-mode)
          (let ((buffer-read-only nil))
            (insert-file-contents tmpfile))
          (delete-file tmpfile)
          (goto-char (point-max)))
        (unless (zerop returncode)
          (display-buffer "*Compile-Log*"))))))

;; + (provide)

(provide 'setup)

;;; setup.el ends here
