;;; modus-exporter-pkg.el --- Define modus-exporter for package.el
;;; Commentary:
;;; Uses define-package to define "modus-exporter"
;;;
;;; Code:

(define-package "modus-exporter" "0.0.2"
  "An Emacs Lisp library for exporting modus-themes to different formats for other software"
  '(
    ('modus-operandi-theme "0.9.0")
    ('modus-vivendi-theme "0.9.0")))

;;; modus-exporter-pkg.el ends here
