(defvar setup--simulated-absent-libraries nil)
(defun setup--locate-library (file)
  (and (not (member file setup--simulated-absent-libraries))
       (locate-library file)))

;; API
(defmacro setup-simulate-absense (file)
  (push file setup--simulated-absent-libraries)
  nil)

(provide 'setup-utils)
