;;; dwarf-fortress.el --- Exporter for the Dwarf Fortress colors.txt configuration file -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;; This file is part of modus-exporter
;;;
;;; Code:

(defun modus-exporter-export-theme-dwarf-fortress (theme-name)
  "Export the modus-(operandi|vivendi) theme for use with Dwarf Fortress.

THEME-NAME should be either 'operandi or 'vivendi."
  (let (
        (mappings `(
                    ("BLACK" . "#000000")
                    ("BLUE" . ,(modus-exporter-get-colour theme-name "blue"))
                    ("GREEN" . ,(modus-exporter-get-colour theme-name "green"))
                    ("CYAN" . ,(modus-exporter-get-colour theme-name "cyan"))
                    ("RED" . ,(modus-exporter-get-colour theme-name "red"))
                    ("MAGENTA" . ,(modus-exporter-get-colour theme-name "magenta"))
                    ("BROWN" . ,(modus-exporter-get-colour theme-name "yellow-subtle-bg"))
                    ("LGRAY" . "#aaaaaa")
                    ("DGRAY" . "#555555")
                    ("LBLUE" . ,(modus-exporter-get-colour theme-name "blue-faint"))
                    ("LGREEN" . ,(modus-exporter-get-colour theme-name "green-faint"))
                    ("LCYAN" . ,(modus-exporter-get-colour theme-name "cyan-faint"))
                    ("LRED" . ,(modus-exporter-get-colour theme-name "red-faint"))
                    ("LMAGENTA" . ,(modus-exporter-get-colour theme-name "magenta-faint"))
                    ("YELLOW" . ,(modus-exporter-get-colour theme-name "yellow"))
                    ("WHITE" . "#ffffff"))))
    (string-join

     ; Map each key to <key>_{R,G,B} with the value of that component in decimal
     (mapcar (lambda (colour)
               (let* (
                      ; Get hex colour as normalised (R G B)
                      (rgb (color-name-to-rgb (cdr colour)))

                      ; Calculate decimal in the range [0, 256) for each
                      ; component
                      (r (int-to-string (round (* 255 (nth 0 rgb)))))
                      (g (int-to-string (round (* 255 (nth 1 rgb)))))
                      (b (int-to-string (round (* 255 (nth 2 rgb))))))

                 ; Build 3-line string for this colour
                 (concat
                  "[" (car colour) "_R:" r "]\n"
                  "[" (car colour) "_G:" g "]\n"
                  "[" (car colour) "_B:" b "]")))
             mappings)
     "\n")))

;; Add the exporter function to the list under 'dwarf-fortress
(defvar modus-exporter-export-functions '())
(unless (alist-get 'dwarf-fortress modus-exporter-export-functions)
        (add-to-list 'modus-exporter-export-functions '(dwarf-fortress . modus-exporter-export-theme-dwarf-fortress) t))

;;; dwarf-fortress.el ends here
