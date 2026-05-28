;;; punch-line-org-clock.el --- org-clock section for punch-line -*- lexical-binding: t; -*-

;; Author: cashmere
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Display the currently clocked org task. Reads only `org-clocking-p'
;; state; never forces org to load.

;;; Code:

(require 'punch-line-glyphs)
(require 'org-clock nil t)

(defcustom punch-show-org-clock-info nil
  "If non-nil, show the active org-clock task in the mode-line."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-org-clock-max-length 30
  "Maximum displayed length of the org task heading."
  :type 'integer
  :group 'punch-line)

(defface punch-line-org-clock-face
  '((t :inherit mode-line-emphasis))
  "Face for the org-clock task in the mode-line."
  :group 'punch-line)

(defun punch-org-clock-info ()
  "Mode-line segment for the active org-clock task, if any."
  (when (and punch-show-org-clock-info
             (featurep 'org-clock)
             (fboundp 'org-clocking-p)
             (org-clocking-p))
    (let* ((heading (or (and (boundp 'org-clock-current-task)
                             org-clock-current-task)
                        ""))
           (trimmed (truncate-string-to-width
                     heading punch-org-clock-max-length nil nil "…")))
      (concat (punch-line-glyph 'clock) " "
              (propertize trimmed 'face 'punch-line-org-clock-face)))))

(provide 'punch-line-org-clock)
;;; punch-line-org-clock.el ends here
