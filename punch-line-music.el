;;; punch-line-music.el --- Customized mode-line with music info -*- lexical-binding: t; -*-
;; Author: Mikael Konradsson
;; Version: 2.1
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; This package provides a customized mode-line component for displaying music information.

;;; Code:

(require 'subr-x)  ; for string-trim
(require 'punch-line-glyphs)
(require 'nerd-icons nil t)

(defgroup punch-line nil
  "Customization group for punch-line."
  :group 'mode-line)

(defcustom punch-line-music-info nil
  "Configuration for music information display in the mode-line.
If nil, music information is not displayed.
If t, music information is displayed with default settings.
If a plist, it can contain the following properties:
  :service - Symbol 'apple or 'spotify to specify the music service."
  :type '(choice (const :tag "Disabled" nil)
                 (const :tag "Enabled (default)" t)
                 (plist :tag "Custom configuration"
                        :options ((:service (choice (const apple) (const spotify))))))
  :group 'punch-line)

(defcustom punch-line-music-max-length 40
  "Maximum length of the music info displayed in the mode-line."
  :type 'number
  :group 'punch-line)

(defvar punch-music-info-cache ""
  "Cache for music information.")

(defvar punch-music-info-last-update 0
  "Timestamp of the last music info update.")

(defvar punch-music-info-update-interval 10
  "Interval in seconds between music info updates.")

(defvar punch-music-info-timer nil
  "Timer for updating music information.")

(defvar punch-line-process-timeout 5
  "Timeout in seconds for music info process.")

(defun punch-line-cleanup-stale-process ()
  "Clean up any stale music info process and its buffer."
  (let* ((buffer-name "*punch-line-music-info*")
         (buffer (get-buffer buffer-name))
         (process (and buffer (get-buffer-process buffer))))
    (when process
      (when (eq (process-status process) 'run)
        (delete-process process))
      (when buffer
        (kill-buffer buffer)))))

(defun punch-line-music--osascript (service)
  "AppleScript command list for SERVICE (\\='apple or \\='spotify) on macOS."
  (let ((app-name (if (eq service 'apple) "Music" "Spotify")))
    (list "osascript" "-e"
          (format "
tell application \"System Events\"
  if exists process \"%s\" then
    tell application \"%s\"
      if player state is playing then
        set track_name to name of current track
        set artist_name to artist of current track
        return track_name & \" • \" & artist_name
      else
        return \"\"
      end if
    end tell
  else
    return \"\"
  end if
end tell" app-name app-name))))

(defun punch-line-music--playerctl (_service)
  "playerctl command on Linux. Returns nil if playerctl is missing."
  (when (executable-find "playerctl")
    (list "sh" "-c"
          "test \"$(playerctl status 2>/dev/null)\" = \"Playing\" && \
playerctl metadata --format '{{title}} • {{artist}}' 2>/dev/null")))

(defun punch-line-music--default-service ()
  "Best-guess default music service for the current platform."
  (pcase system-type
    ('darwin    'apple)
    ('gnu/linux 'playerctl)
    (_          nil)))

(defun punch-line-get-music-service ()
  "Get the configured music service.
Resolves `punch-line-music-info' against platform defaults."
  (cond
   ((null punch-line-music-info) nil)
   ((eq punch-line-music-info t) (punch-line-music--default-service))
   ((and (plist-member punch-line-music-info :service)
         (memq (plist-get punch-line-music-info :service)
               '(apple spotify playerctl)))
    (plist-get punch-line-music-info :service))
   (t (punch-line-music--default-service))))

(defun punch-line-music--command (service)
  "Return command list (PROGRAM . ARGS) for SERVICE, or nil if unsupported."
  (pcase service
    ((or 'apple 'spotify) (punch-line-music--osascript service))
    ('playerctl           (punch-line-music--playerctl service))
    (_                    nil)))

(defun punch-line-update-music-info-async ()
  "Update the cached music info asynchronously with timeout handling."
  (let ((current-time (float-time))
        (service (punch-line-get-music-service)))
    (when (and service
               (or (null punch-music-info-cache)
                   (> (- current-time punch-music-info-last-update)
                      punch-music-info-update-interval)))
      (setq punch-music-info-last-update current-time)
      ;; Cleanup any existing process first
      (punch-line-cleanup-stale-process)
      (condition-case err
          (let* ((buffer-name "*punch-line-music-info*")
                 (cmd (punch-line-music--command service))
                 (process (and cmd (apply #'start-process
                                          "punch-line-music-info"
                                          buffer-name
                                          cmd))))
        (unless process
          (setq punch-music-info-cache "")
          (signal 'user-error '("punch-line-music: no command for service")))
        ;; Set process timeout with proper closure
        (run-with-timer punch-line-process-timeout nil
                       (lambda (proc)
                         (when (and proc
                                    (processp proc)
                                    (process-live-p proc)
                                    (eq (process-status proc) 'run))
                           (delete-process proc)
                           (let ((buf (process-buffer proc)))
                             (when (and buf (buffer-live-p buf))
                               (kill-buffer buf)))))
                       process)
        (set-process-sentinel
         process
         (lambda (proc event)
           (condition-case sentinel-err
               (when (string= event "finished\n")
             ;; Check buffer is alive before trying to read from it
             (let ((proc-buffer (process-buffer proc)))
               (when (and proc-buffer (buffer-live-p proc-buffer))
                 (with-current-buffer proc-buffer
                   (let ((output (buffer-string)))
                     (setq punch-music-info-cache
                           (if (string-empty-p (string-trim output))
                               ""
                             (if (> punch-line-music-max-length 0)
                                 (concat " " (punch-line-icon) " "
                                         (propertize (punch-line-trim-music-info output)
                                                   'face 'punch-line-music-face))
                               (concat (punch-line-icon) " "))))
                     (ignore-errors (force-mode-line-update t)))))))
             (error
              ;; Silently handle any sentinel errors to prevent overnight crashes
              (setq punch-music-info-cache "")))
           ;; Always clean up process buffer
           (let ((proc-buffer (process-buffer proc)))
             (when (and proc-buffer (buffer-live-p proc-buffer))
               (kill-buffer proc-buffer))))))
        (error
         ;; If we can't start the process, just keep the existing cache
         nil)))))

(defun punch-line-trim-music-info (info)
  "Trim the music INFO to a maximum length."
  (let ((max-length punch-line-music-max-length)
        (text (replace-regexp-in-string "\n" "" (string-trim info))))
    (if (> (length text) max-length)
        (concat (substring text 0 max-length) "...")
      text)))

(defun punch-line-icon ()
  "Return the icon for the music service."
  (let ((service (punch-line-get-music-service)))
    (cond
     ((eq service 'apple)     (propertize (punch-line-glyph 'music)   'face 'punch-line-music-apple-face))
     ((eq service 'spotify)   (propertize (punch-line-glyph 'spotify) 'face 'punch-line-music-spotify-face))
     ((eq service 'playerctl) (propertize (punch-line-glyph 'music)   'face 'punch-line-music-face))
     (t ""))))

(defun punch-line-start-music-info-timer ()
  "Start the timer for updating music information."
  (when (and punch-line-music-info (null punch-music-info-timer))
    (setq punch-music-info-timer
          (run-at-time 0 punch-music-info-update-interval #'punch-line-update-music-info-async))))

(defun punch-line-stop-music-info-timer ()
  "Stop the timer and clean up any running process."
  (when punch-music-info-timer
    (cancel-timer punch-music-info-timer)
    (setq punch-music-info-timer nil))
  (punch-line-cleanup-stale-process))

(defun punch-line-get-music-info ()
  "Get the current cached music info with a limited length."
  punch-music-info-cache)

(defun punch-line-music-info ()
  "Return the current music info for display in the mode-line."
  (when (and punch-line-music-info (punch-line-get-music-service))
    (punch-line-get-music-info)))

(add-hook 'kill-emacs-hook #'punch-line-stop-music-info-timer)

(provide 'punch-line-music)
;;; punch-line-music.el ends here
