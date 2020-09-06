(defvar setup-disable-magic-file-name nil
  "When non-nil, file-name-handler-alist is set nil during
startup for performance.")

(defvar setup-enable-gc-threshold-hacks nil
  "When non-nil, enlarge gc-threshold during startup.")

(defmacro setup-hacks--magic-file-name-after-init ()
  (when setup-disable-magic-file-name
    `(unless file-name-handler-alist
       (setq file-name-handler-alist ',file-name-handler-alist))))

(defmacro setup-hacks--gc-threshold-after-init ()
  (when setup-enable-gc-threshold-hacks
    '(setq gc-cons-threshold  16777216 ; 16mb
           gc-cons-percentage 0.1)))

(defmacro setup-hacks--magic-file-name-initialize ()
  (when setup-disable-magic-file-name
    '(setq file-name-handler-alist nil)))

(defmacro setup-hacks--gc-threshold-initialize ()
  (when setup-enable-gc-threshold-hacks
    '(setq gc-cons-threshold  most-positive-fixnum
           gc-cons-percentage 0.6)))

(provide 'setup-hacks)
