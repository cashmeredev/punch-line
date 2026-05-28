;;; punch-line-term.el --- Terminal indicator for punch-line -*- lexical-binding: t; -*-

;; Author: Mikael Konradsson, cashmere
;; Version: 1.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Shows a terminal indicator in the mode-line when one or more terminal
;; buffers (vterm, eat, ansi-term, shell, eshell, multi-vterm) are alive
;; but currently hidden. Optionally surfaces the last command run.

;;; Code:

(require 'cl-lib)
(require 'punch-line-glyphs)
(require 'nerd-icons nil t)

(defface punch-term-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for the terminal icon in the punch line."
  :group 'punch-line)

(defcustom punch-show-term-info nil
  "If non-nil, show a hidden-terminal indicator in the mode-line."
  :type 'boolean
  :group 'punch-line)

(defcustom punch-term-buffer-pattern
  "\\`\\*\\(vterm\\|eat\\|terminal\\|shell\\|eshell\\|ansi-term\\)"
  "Regexp matching buffer names that count as terminals."
  :type 'regexp
  :group 'punch-line)

(defcustom punch-term-show-last-command t
  "If non-nil, append the last command run in the most-recent terminal."
  :type 'boolean
  :group 'punch-line)

(defun punch-term--all-terminals ()
  "List live terminal buffers matching `punch-term-buffer-pattern'."
  (cl-loop for buf in (buffer-list)
           when (string-match-p punch-term-buffer-pattern (buffer-name buf))
           collect buf))

(defun punch-term--any-visible-p (buffers)
  "Return non-nil if any of BUFFERS is currently visible in a window."
  (cl-some (lambda (b) (get-buffer-window b 'visible)) buffers))

(defun punch-term--last-command (buffer)
  "Best-effort: return the last shell command in BUFFER.
Heuristic matches starship-like prompt markers; returns nil on miss."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (derived-mode-p 'vterm-mode 'term-mode 'eat-mode)
        (save-excursion
          (goto-char (point-max))
          (let ((case-fold-search nil)
                (limit (max (point-min) (- (point-max) 3000))))
            (catch 'result
              (while (re-search-backward " [↱±]\\s-+\\(.+\\)$" limit t)
                (let* ((full (string-trim (match-string-no-properties 1)))
                       (cleaned (replace-regexp-in-string "[^a-zA-Z ]" "" full))
                       (first (car (split-string cleaned))))
                  (when (and first
                             (not (member first '("cd" "ls" "pwd"))))
                    (throw 'result first)))))))))))

(defun punch-term-info ()
  "Mode-line segment: terminal indicator when terminals are hidden."
  (when punch-show-term-info
    (let* ((terms (punch-term--all-terminals)))
      (when (and terms (not (punch-term--any-visible-p terms)))
        (let* ((icon (punch-line-glyph 'terminal))
               (count (length terms))
               (last-cmd (and punch-term-show-last-command
                              (punch-term--last-command (car terms))))
               (text (cond
                      ((and (= count 1) last-cmd) (format "%s %s" icon last-cmd))
                      ((= count 1) icon)
                      (last-cmd (format "%s %d (%s)" icon count last-cmd))
                      (t (format "%s %d" icon count)))))
          (propertize text 'face 'punch-term-face))))))

(provide 'punch-line-term)
;;; punch-line-term.el ends here
