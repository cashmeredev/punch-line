;;; punch-line-erc.el --- ERC unread channel badge for punch-line -*- lexical-binding: t; -*-

;; Author: cashmere
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Aggregate ERC unread channel count from `erc-modified-channels-alist'.
;; Soft-requires erc-track; renders nothing when erc is not loaded.

;;; Code:

(require 'punch-line-glyphs)
(require 'erc-track nil t)

(defcustom punch-show-erc-info nil
  "If non-nil, show ERC unread channel count in the mode-line."
  :type 'boolean
  :group 'punch-line)

(defface punch-line-erc-face
  '((t :inherit mode-line-emphasis))
  "Face for ERC unread badge."
  :group 'punch-line)

(defface punch-line-erc-mention-face
  '((t :inherit warning))
  "Face for ERC unread badge when any channel has a nick mention."
  :group 'punch-line)

(defun punch-erc--mention-p ()
  "Return non-nil if any tracked channel currently has a nick mention.
Best-effort: relies on `erc-track-faces-priority-list' containing
`erc-current-nick-face' at the front."
  (and (boundp 'erc-modified-channels-alist)
       (cl-some (lambda (entry)
                  (let ((faces (nthcdr 2 entry)))
                    (and faces
                         (memq 'erc-current-nick-face faces))))
                erc-modified-channels-alist)))

(defun punch-erc-info ()
  "Mode-line segment with ERC unread channel count."
  (when (and punch-show-erc-info
             (featurep 'erc-track)
             (boundp 'erc-modified-channels-alist)
             erc-modified-channels-alist)
    (let* ((n (length erc-modified-channels-alist))
           (face (if (punch-erc--mention-p)
                     'punch-line-erc-mention-face
                   'punch-line-erc-face)))
      (propertize (format "%s %d" (punch-line-glyph 'chat) n) 'face face))))

(provide 'punch-line-erc)
;;; punch-line-erc.el ends here
