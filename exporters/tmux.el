;;; tmux.el --- A Tmux modus-exporter library -*- lexical-binding: t; -*-
;;; Commentary:
;;; This file is part of modus-exporter
;;;
;;; Code:

(require 'modus-exporter)

(defun modus-exporter-export-theme-tmux (theme-name)
  "Export the modus-(operandi|vivendi) theme for use with Tmux.

THEME-NAME should be either 'operandi or 'vivendi."
  (let ((config `(
                  ,(format "pane-active-border-style \"fg=%s\""
                           (modus-exporter-get-colour theme-name "fg-active"))
                  ,(format "pane-border-style \"fg=%s\""
                           (modus-exporter-get-colour theme-name "bg-active"))
                  ,(format "message-style \"bg=%s,fg=%s\""
                           (modus-exporter-get-colour theme-name "blue")
                           (modus-exporter-get-colour theme-name "bg-main"))
                  ,(format "status-style \"bg=%s,fg=%s\""
                           (modus-exporter-get-colour theme-name "green")
                           (modus-exporter-get-colour theme-name "bg-main"))
                  )))
    (mapconcat (lambda (s) (concat "set-option -g " s)) config "\n")))

;; Add the exporter function to the list under 'alacritty
(defvar modus-exporter-export-functions '())
(unless (alist-get 'tmux modus-exporter-export-functions)
        (add-to-list 'modus-exporter-export-functions '(tmux . modus-exporter-export-theme-tmux) t))

;;; tmux.el ends here
