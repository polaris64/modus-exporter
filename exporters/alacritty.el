;;; alacritty.el --- An Alacritty modus-exporter library -*- lexical-binding: t; -*-
;;; Commentary:
;;; This file is part of modus-exporter
;;;
;;; Code:

(defun modus-exporter-export-theme-alacritty (theme-name)
  "Export the modus-(operandi|vivendi) theme for use with Alacritty.

THEME-NAME should be either 'operandi or 'vivendi."

  (let (
    (mappings '(
      ("primary" . (
        ("background" . "bg-main")
        ("foreground" . "fg-main")))
      ("cursor" . (
        ("text"   . "bg-main")
        ("cursor" . "fg-main")))
      ("normal" . (
        ("black"   . "#000000")
        ("red"     . "red")
        ("green"   . "green")
        ("yellow"  . "yellow")
        ("blue"    . "blue")
        ("magenta" . "magenta")
        ("cyan"    . "cyan")
        ("white"   . "#eeeeee")))
      ("bright" . (
        ("black"   . "#555555")
        ("red"     . "red-intense")
        ("green"   . "green-intense")
        ("yellow"  . "yellow-intense")
        ("blue"    . "blue-intense")
        ("magenta" . "magenta-intense")
        ("cyan"    . "cyan-intense")
        ("white"   . "#ffffff")))
      ("dim" . (
        ("black"   . "#222222")
        ("red"     . "red-faint")
        ("green"   . "green-faint")
        ("yellow"  . "yellow-faint")
        ("blue"    . "blue-faint")
        ("magenta" . "magenta-faint")
        ("cyan"    . "cyan-faint")
        ("white"   . "#dddddd"))))))

    ;; Return the entire "colors:" section
    (concat "colors:\n"

      ;; Build a string for each section in mappings
      (mapconcat (lambda (section)
        (concat "  " (car section) ":\n"

          ;; Build a string for each colour in section
          (mapconcat (lambda (colour)

            ;; Build a string for this specific colour
            (concat "    " (car colour) ": "

              ;; Fetch associated colour hex string, replace "#" with "0x" and
              ;; wrap it in single quotes
              (concat "'"
                (replace-regexp-in-string "#" "0x"
                  (modus-exporter-get-colour theme-name (cdr colour)))
                "'")))

              (cdr section)
            "\n")))
        mappings
      "\n"))))

;; Add the exporter function to the list under 'alacritty
(defvar modus-exporter-export-functions '())
(unless (alist-get 'alacritty modus-exporter-export-functions)
        (add-to-list 'modus-exporter-export-functions '(alacritty . modus-exporter-export-theme-alacritty) t))

;;; alacritty.el ends here
