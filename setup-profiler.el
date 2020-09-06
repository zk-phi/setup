(defvar setup-use-profiler nil
  "When non-nil, profile setup time and report after init.")

(defmacro setup-profiler--initialize ()
  (when setup-use-profiler
    '(progn
       (require 'profiler)
       (profiler-start 'cpu))))

(defmacro setup-profiler--after-init ()
  (when setup-use-profiler
    '(progn
       (profiler-report)
       (profiler-stop))))

(provide 'setup-profiler)
