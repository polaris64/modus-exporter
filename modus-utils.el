;;; modus-utils.el --- Utility functions for modus-themes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Simon Pugnet
;;
;; Author: Simon Pugnet <https://github.com/polaris64>
;; Maintainer: Simon Pugnet <simon@polaris64.net>
;; Created: July 25, 2020
;; Modified: July 25, 2020
;; Version: 0.0.1
;; Keywords: modus-themes themes
;; Homepage: https://github.com/polaris64/modus-utils
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module provides a set of functions which can be used to export one of
;; the modus-themes (modus-operandi or modus-vivendi) to another format. The
;; purpose of this is to allow the themes to be used in other applications that
;; allow colours to be specified in their configuration files.
;;
;; The two main functions are: -
;;
;;   * modus-utils-export-theme: returns the theme colour configuration in a
;;     specified format as a string.
;;   * modus-utils-insert-theme-colours-at-point: takes the output of the
;;     previous function and inserts it into the current buffer at the current
;;     point.
;;
;; NOTE: these functions require direct access to the theme's colour tables.
;; Therefore in order for these functions to work, the requested theme (operandi
;; or vivendi) must currently be loaded in Emacs.
;;
;; The export formats currently supported are: -
;;
;;  * 'alacritty: YAML to be placed in the user's alacritty.yml file.
;;
;;; Code:

(defun modus-utils-get-colour (theme-name colour-name)
"Convert a colour name to a hex colour string from a theme.

THEME-NAME should be either 'operandi or 'vivendi.

COLOUR-NAME should be a colour defined within the specified theme
and will be replaced with the equivalent hex colour string. If it
is already a hex string it will be returned unmodified."

  (if (string-match "^#[0-9a-fA-F]+" colour-name)

      ;; Return colour-name as-is
      colour-name

    ;; Otherwise fetch the hex string from the specified theme
    (progn

      (defvar modus-operandi-theme-default-colors-alist)
      (defvar modus-vivendi-theme-default-colors-alist)

      (catch 'invalid-theme
        (let ((colour-list
               (cond
                ((eq theme-name 'operandi) modus-operandi-theme-default-colors-alist)
                ((eq theme-name 'vivendi) modus-vivendi-theme-default-colors-alist))))

          (if (not colour-list)
              (progn
                (message (concat "Invalid theme name: " (symbol-name theme-name)))
                (throw 'invalid-theme colour-name)))

          (alist-get colour-name colour-list nil nil 'string-equal))))))

(defun modus-utils-get-colours (theme-name colour-alist)
"Convert values in an alist to hex colour strings from a theme.

Take an alist and replace all values with the corresponding hex
colour string from the appropriate theme.

THEME-NAME should be either 'operandi or 'vivendi.

COLOUR-ALIST should be an alist containing string keys and
values. The values should refer to colours defined within the
specified theme and will be replaced with hex colour strings. Any
value which is already a hex string will be returned unmodified."

  ;; Map each (key . val) pair to (key . colour-hex)
  (mapcar (lambda (list-val)
            `(
              ;; Return key as-is
              ,(car list-val)

              ,(modus-utils-get-colour theme-name (cdr list-val))))
          colour-alist))

(defun modus-utils-export-theme-alacritty (theme-name)
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
                  (modus-utils-get-colour theme-name (cdr colour)))
                "'")))

              (cdr section))
            "\n")))
        mappings)
      "\n"))))

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
