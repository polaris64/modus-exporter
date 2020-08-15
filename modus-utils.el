;;; modus-utils.el --- Utility functions for modus-themes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Simon Pugnet
;;
;; Author: Simon Pugnet <http://github/polaris64>
;; Maintainer: Simon Pugnet <simon@polaris64.net>
;; Created: July 25, 2020
;; Modified: July 25, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/polaris64/modus-utils
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:
(defun p64/get-closest-matching-face-colour (target-colour)
"Return to closest matching face colour to a given TARGET-COLOUR.

TARGET-COLOUR is the RGB hex string (e.g. \"#aabbcc\") to match."
  (apply 'color-rgb-to-hex
         (append (color-name-to-rgb
                  (car (cdr (reduce
                             (lambda (a v)
                               (if (< (car a)
                                      (car v)) a v))
                             (remove-if-not (lambda (v) (car v))
                                            (mapcar (lambda (fname)
                                                      (let* (
                                                             (fg (face-foreground fname))
                                                             (bg (face-background fname))
                                                             (dfg (when fg (color-distance fg target-colour)))
                                                             (dbg (when bg (color-distance bg target-colour))))
                                                        (if (and dfg dbg)
                                                            (if (< dfg dbg) `(,dfg ,fg) `(,dbg ,bg))
                                                          (if dfg `(,dfg ,fg) `(,dbg ,bg)))))
                                                    (face-list)))
                             :initial-value '(65535 . nil)))))
                 '(2))))

(defun p64/export-theme-alacritty ()
  "Export the current theme for use with Alacritty."

  (let (
        (mappings `(
                    ("primary" . (
                                  ("background" . ,(face-background 'default))
                                  ("foreground" . ,(face-foreground 'default))))
                    ("cursor" . (
                                 ("text"   . ,(face-background 'default))
                                 ("cursor" . ,(face-foreground 'default))))
                    ("normal" . (
                                 ("black"   . ,(p64/get-closest-matching-face-colour "#000000"))
                                 ("red"     . ,(p64/get-closest-matching-face-colour "#ee6666"))
                                 ("green"   . ,(p64/get-closest-matching-face-colour "#66ee66"))
                                 ("yellow"  . ,(p64/get-closest-matching-face-colour "#eeee66"))
                                 ("blue"    . ,(p64/get-closest-matching-face-colour "#6666ee"))
                                 ("magenta" . ,(p64/get-closest-matching-face-colour "#ee66ee"))
                                 ("cyan"    . ,(p64/get-closest-matching-face-colour "#66eeee"))
                                 ("white"   . ,(p64/get-closest-matching-face-colour "#eeeeee"))))
                    ("bright" . (
                                 ("black"   . ,(p64/get-closest-matching-face-colour "#666666"))
                                 ("red"     . ,(p64/get-closest-matching-face-colour "#ff6666"))
                                 ("green"   . ,(p64/get-closest-matching-face-colour "#66ff66"))
                                 ("yellow"  . ,(p64/get-closest-matching-face-colour "#ffff66"))
                                 ("blue"    . ,(p64/get-closest-matching-face-colour "#6666ff"))
                                 ("magenta" . ,(p64/get-closest-matching-face-colour "#ff66ff"))
                                 ("cyan"    . ,(p64/get-closest-matching-face-colour "#66ffff"))
                                 ("white"   . ,(p64/get-closest-matching-face-colour "#ffffff"))))
                    ("dim" . (
                                 ("black"   . ,(p64/get-closest-matching-face-colour "#111111"))
                                 ("red"     . ,(p64/get-closest-matching-face-colour "#662222"))
                                 ("green"   . ,(p64/get-closest-matching-face-colour "#226622"))
                                 ("yellow"  . ,(p64/get-closest-matching-face-colour "#666622"))
                                 ("blue"    . ,(p64/get-closest-matching-face-colour "#222266"))
                                 ("magenta" . ,(p64/get-closest-matching-face-colour "#662266"))
                                 ("cyan"    . ,(p64/get-closest-matching-face-colour "#226666"))
                                 ("white"   . ,(p64/get-closest-matching-face-colour "#eeeeee"))))
                    )))

    ;; Return the entire "colors:" section
    (concat "colors:\n"

            ;; Build a string for each section in mappings
            (string-join (mapcar (lambda (section)
                                   (concat "  " (car section) ":\n"

                                           ;; Build a string for each colour in section
                                           (string-join (mapcar (lambda (colour)

                                                                  ;; Build a string for this specific colour
                                                                  (concat "    "
                                                                          (car colour)
                                                                          ": "
                                                                          (concat "'"
                                                                                  (replace-regexp-in-string "#" "0x"
                                                                                                            (cdr colour))
                                                                                  "'")))
                                                                (cdr section))
                                                        "\n")))
                                 mappings)
                         "\n"))))

(defun p64/export-theme (export-format)
"Export the current theme to a given format.

EXPORT-FORMAT should be the name of a supported export format,
such as 'alacritty."
  (catch 'invalid-format
    (let (
      (mappings '(
                  (alacritty . p64/export-theme-alacritty))))
      (let ((export-fn (alist-get export-format mappings)))
        (if (not export-fn) (progn
          (message (concat "Invalid export-format: " (symbol-name export-format)))
          (throw 'invalid-format nil)))
        (funcall export-fn)))))

(defun p64/insert-theme-colours-at-point (export-format)
"Insert colours from the current theme at the current point.

EXPORT-FORMAT should be the name of a supported export format,
such as 'alacritty."
  (interactive "Sexport format: ")
  (let ((exported-string (p64/export-theme export-format)))
    (when exported-string (insert exported-string))))

(defun modus-utils-get-colours (theme-name colour-alist)
"Convert values in an alist to hex colour strings from a theme.

Take an alist and replace all values with the corresponding hex
colour string from the appropriate theme.

THEME-NAME should be either 'operandi or 'vivendi.

COLOUR-ALIST should be an alist containing string keys and
values. The values should refer to colours defined within the
specified theme and will be replaced with hex colour strings. Any
value which is already a hex string will be returned unmodified."

  (defvar modus-operandi-theme-default-colors-alist)
  (defvar modus-vivendi-theme-default-colors-alist)

  (catch 'invalid-theme
    (let ((colour-list
      (cond
        ((eq theme-name 'operandi) modus-operandi-theme-default-colors-alist)
        ((eq theme-name 'vivendi) modus-vivendi-theme-default-colors-alist))))

      (if (not colour-list) (throw 'invalid-theme colour-alist))

      ;; Map each (key . val) pair to (key . colour-hex)
      (mapcar (lambda (list-val)
                `(,(car list-val) ,(alist-get (cdr list-val) colour-list nil nil 'string-equal)))
              colour-alist))))

(defun modus-utils-export-theme-alacritty (theme-name)
"Export the modus-(operandi|vivendi) theme for use with Alacritty.

THEME-NAME should be either 'operandi or 'vivendi.

Requires modus-operandi-theme-default-colors-alist and
modus-vivendi-theme-default-colors-alist in order to fetch
currently defined colour hex strings."

  (defvar modus-operandi-theme-default-colors-alist)
  (defvar modus-vivendi-theme-default-colors-alist)

  (let (
    (mappings '(
      ("primary" . (
        ("background" . "bg-main")
        ("foreground" . "fg-main")))
      ("cursor" . (
        ("text"   . "bg-main")
        ("cursor" . "fg-main")))
      ("normal" . (
        ("black"   . "bg-main")
        ("red"     . "red")
        ("green"   . "green")
        ("yellow"  . "yellow")
        ("blue"    . "blue")
        ("magenta" . "magenta")
        ("cyan"    . "cyan")
        ("white"   . "fg-main")))
      ("bright" . (
        ("black"   . "bg-alt")
        ("red"     . "red-intense")
        ("green"   . "green-intense")
        ("yellow"  . "yellow-intense")
        ("blue"    . "blue-intense")
        ("magenta" . "magenta-intense")
        ("cyan"    . "cyan-intense")
        ("white"   . "fg-alt")))
      ("dim" . (
        ("black"   . "bg-dim")
        ("red"     . "red-faint")
        ("green"   . "green-faint")
        ("yellow"  . "yellow-faint")
        ("blue"    . "blue-faint")
        ("magenta" . "magenta-faint")
        ("cyan"    . "cyan-faint")
        ("white"   . "fg-dim")))))
    (colour-list
     (cond
       ((eq theme-name 'operandi) modus-operandi-theme-default-colors-alist)
       ((eq theme-name 'vivendi) modus-vivendi-theme-default-colors-alist))))

    (catch 'invalid-theme

      (when (not colour-list) (progn
        (message (concat "Invalid theme " (symbol-name theme-name)))
        (throw 'invalid-theme nil)
      ))

      ;; Return the entire "colors:" section
      (concat "colors:\n"

        ;; Build a string for each section in mappings
        (string-join (mapcar (lambda (section)
          (concat "  " (car section) ":\n"

            ;; Build a string for each colour in section
            (string-join (mapcar (lambda (colour)

              ;; Build a string for this specific colour
              (concat "    " (car colour) ": "

                ;; Fetch associated colour hex string, replace "#" with "0x" and
                ;; wrap it in single quotes
                (concat "'"
                  (replace-regexp-in-string "#" "0x"
                    (alist-get (cdr colour) colour-list nil nil 'string-equal))
                  "'")))

                (cdr section))
              "\n")))
          mappings)
        "\n")))))

(defun modus-utils-export-theme (theme-name export-format)
"Export the modus-(operandi|vivendi) theme to a given format.

THEME-NAME should be either 'operandi or 'vivendi.

EXPORT-FORMAT should be the name of a supported export format,
such as 'alacritty."
  (catch 'invalid-format
    (let (
      (mappings '(
                  (alacritty . modus-utils-export-theme-alacritty))))
      (let ((export-fn (alist-get export-format mappings)))
        (if (not export-fn) (progn
          (message (concat "Invalid export-format: " (symbol-name export-format)))
          (throw 'invalid-format nil)))
        (funcall export-fn theme-name)))))

(defun modus-utils-insert-theme-colours-at-point (theme-name export-format)
"Insert colours from a theme at the current point.

THEME-NAME should be either 'operandi or 'vivendi.

EXPORT-FORMAT should be the name of a supported export format,
such as 'alacritty."
  (interactive "Stheme name: \nSexport format: ")
  (let ((exported-string (modus-utils-export-theme theme-name export-format)))
    (when exported-string (insert exported-string))))

(provide 'modus-utils)
;;; modus-utils.el ends here
