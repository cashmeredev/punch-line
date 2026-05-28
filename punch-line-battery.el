;;; punch-line-battery.el --- A customized mode-line for Emacs, with status and advanced customizations -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson
;; Version: 1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This package offers a customized mode-line for Emacs,
;; configurable colors, and the ability to customize displayed information.

;;; Code:

(require 'battery)
(require 'punch-line-glyphs)
(require 'nerd-icons nil t)

(defgroup punch-battery nil
  "Customization group for punch-line-battery."
  :group 'punch-line)

(defcustom punch-battery-show-percentage t
  "When non-nil, display battery percentage as text after the icon."
  :type 'boolean
  :group 'punch-battery)

(defcustom punch-show-battery-info t
  "If set to t, show battery icons with nerdicons."
  :type 'boolean
  :group 'punch-battery)

(defcustom punch-battery-cache-update-interval 60
  "Interval in seconds for updating the Battery- cache."
  :type 'number
  :group 'punch-line)

(defvar punch-battery-info-cache 'unset
  "Cache for Battery information.

(defvar punch-battery-info-cache-time 0
  "Time of last cache update.")

(defun punch-battery--slurp (path)
  "Read PATH and return its trimmed contents, or nil on error."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (string-trim (buffer-string)))))

(defun punch-battery--linux-sysfs ()
  "Read battery state from /sys/class/power_supply/BAT*.
Returns plist (:percent N :status S) where S is one of
\"Charging\", \"Discharging\", \"Full\", \"AC\", or nil if no battery."
  (when (eq system-type 'gnu/linux)
    (let ((bat-dirs (file-expand-wildcards "/sys/class/power_supply/BAT*")))
      (when bat-dirs
        (let* ((dir (car bat-dirs))
               (cap (punch-battery--slurp (expand-file-name "capacity" dir)))
               (stat (punch-battery--slurp (expand-file-name "status" dir))))
          (when cap
            (list :percent (string-to-number cap)
                  :status (or stat "Unknown"))))))))

(defun punch-battery--from-emacs-battery ()
  "Read battery via Emacs' battery.el, robustly. Returns plist or nil."
  (when (and (bound-and-true-p display-battery-mode)
             (boundp 'battery-status-function)
             battery-status-function)
    (let* ((data (ignore-errors (funcall battery-status-function)))
           (raw-p (and data (cdr (assq ?p data))))
           (raw-b (and data (cdr (assq ?B data)))))
      (when (and raw-p (stringp raw-p))
        (list :percent (truncate (or (ignore-errors (string-to-number raw-p)) 0))
              :status (or raw-b "Unknown"))))))

(defun punch-battery--read ()
  "Read battery state from the most reliable available source."
  (or (punch-battery--linux-sysfs)
      (punch-battery--from-emacs-battery)))

(defun punch-battery-create-info ()
  "Create battery percentage and charging status using glyphs."
  (when punch-show-battery-info
    (when-let* ((state (punch-battery--read))
                (percentage (plist-get state :percent))
                (status (plist-get state :status)))
      (let* ((charging (and (member status '("AC" "charging" "Charging")) t))
             (icon (cond
                    (charging              (punch-line-glyph 'battery-charging))
                    ((>= percentage 87.5)  (punch-line-glyph 'battery-full))
                    ((>= percentage 62.5)  (punch-line-glyph 'battery-high))
                    ((>= percentage 37.5)  (punch-line-glyph 'battery-mid))
                    ((>= percentage 12.5)  (punch-line-glyph 'battery-low))
                    (t                     (punch-line-glyph 'battery-empty))))
             (face (cond
                    (charging 'success)
                    ((<= percentage 10) 'error)
                    ((<= percentage 30) 'warning)
                    (t 'success)))
             (percentage-text (if punch-battery-show-percentage
                                  (format " %d%%" percentage)
                                "")))
        (propertize (format "%s%s" icon percentage-text) 'face face)))))

(defun punch-battery-info ()
  "Return battery information, updating the cache if necessary."
  (let ((current-time (float-time)))
    (when (or (eq punch-battery-info-cache 'unset)
	      (> (- current-time punch-battery-info-cache-time) punch-battery-cache-update-interval))
      (setq punch-battery-info-cache-time current-time)
      (setq punch-battery-info-cache (punch-battery-create-info)))
      punch-battery-info-cache))

(provide 'punch-line-battery)
;;; punch-line-battery.el ends here
