;;; modus-exporter.el --- Functions for exporting modus-themes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Simon Pugnet
;;
;; Author: Simon Pugnet <https://github.com/polaris64>
;; Maintainer: Simon Pugnet <simon@polaris64.net>
;; Created: July 25, 2020
;; Modified: July 25, 2020
;; Version: 0.0.2
;; Keywords: modus-themes themes exporter
;; Homepage: https://github.com/polaris64/modus-exporter
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
;;   * modus-exporter-export-theme: returns the theme colour configuration in a
;;     specified format as a string.
;;   * modus-exporter-insert-theme-colours-at-point: takes the output of the
;;     previous function and inserts it into the current buffer at the current
;;     point.
;;
;; NOTE: these functions require direct access to the theme's colour tables.
;; Therefore in order for these functions to work, the requested theme (operandi
;; or vivendi) must currently be loaded in Emacs.
;;
;; Export formats are defined by "exporters", which are Emacs Lisp files within
;; the ./exporters directory. Each exporter must define a function which exports
;; the specified theme to that format and must add this function to the
;; modus-exporter-export-functions alist (i.e. (exporter-name . function)).
;;
;;; Code:

(defconst modus-exporter-base
  (if load-file-name (file-name-directory load-file-name) (file-name-directory buffer-file-name))
  "Base directory of the modus-exporter library, used for locating exporters.")

;;;###autoload
(defvar modus-exporter-export-functions '()
  "Defines the export formats known to modus-exporter.

Each value is an alist where the key is the name of the export format (e.g.
alacritty) and the value is the function responsible for exporting to that
format.")


(defun modus-exporter-get-colour (theme-name colour-name)
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
      (defvar modus-operandi-theme-override-colors-alist)
      (defvar modus-vivendi-theme-override-colors-alist)

      (catch 'invalid-theme
        (let (
              (colour-list (cond
                            ((eq theme-name 'operandi) modus-operandi-theme-default-colors-alist)
                            ((eq theme-name 'vivendi) modus-vivendi-theme-default-colors-alist)))
              (override-colour-list (cond
                            ((eq theme-name 'operandi) modus-operandi-theme-override-colors-alist)
                            ((eq theme-name 'vivendi) modus-vivendi-theme-override-colors-alist))))

          (if (not colour-list)
              (progn
                (message (concat "Invalid theme name: " (symbol-name theme-name)))
                (throw 'invalid-theme colour-name)))

          (let (
                (default-colour (alist-get colour-name colour-list nil nil 'string-equal))
                (override-colour (if override-colour-list (alist-get colour-name override-colour-list nil nil 'string-equal) nil)))
            (or override-colour default-colour)))))))

(defun modus-exporter-get-colours (theme-name colour-alist)
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

              ,(modus-exporter-get-colour theme-name (cdr list-val))))
          colour-alist))

(defun modus-exporter-export-theme (theme-name export-format)
  "Export the modus-(operandi|vivendi) theme to a given format.

THEME-NAME should be either 'operandi or 'vivendi.

EXPORT-FORMAT should be the name of a supported export format,
such as 'alacritty."
  (catch 'invalid-format
    (let ((export-fn (alist-get export-format modus-exporter-export-functions)))
      (if (not export-fn) (progn
                            (message (concat "Invalid export-format: " (symbol-name export-format)))
                            (throw 'invalid-format nil)))
      (funcall export-fn theme-name))))

;;;###autoload
(defun modus-exporter-insert-theme-colours-at-point (theme-name export-format)
  "Insert colours from a theme at the current point.

When called interactively, prompt for THEME-NAME and
EXPORT-FORMAT using completion.

THEME-NAME should be either 'operandi or 'vivendi.

EXPORT-FORMAT should be the name of a supported export format,
such as 'alacritty."
  (interactive
   (list
    (intern (completing-read "Theme name: " '("operandi" "vivendi") nil t))
    (intern (completing-read "Export format: "
                             (mapcar (lambda (v) (car v)) modus-exporter-export-functions) nil t))))
  (let ((exported-string (modus-exporter-export-theme theme-name export-format)))
    (when exported-string (insert exported-string))))


(defun modus-exporter-load-all-exporters ()
  "Load and evaluate all Emacs Lisp files in ./exporters."
  (let* (
         (base-dir (expand-file-name "exporters" modus-exporter-base))
         (files (directory-files base-dir nil "\.el$")))
    (dolist (elt files)
      (let ((filename (replace-regexp-in-string ".el$" "" (expand-file-name elt base-dir))))
        (load filename)))))

;; Load all exporters when the package is loaded for the first time
(when modus-exporter-base (modus-exporter-load-all-exporters))

(provide 'modus-exporter)
;;; modus-exporter ends here
