;;; punch-line-glyphs.el --- GUI/TTY-aware glyph abstraction -*- lexical-binding: t; -*-

;; Author: cashmere
;; Version: 1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Resolves modeline icons to one of three rendering styles:
;;   - nerd     : nerd-icons fontset (best on GUI with patched font)
;;   - unicode  : plain Unicode symbol (works in most modern terminals)
;;   - ascii    : 7-bit fallback (works literally everywhere)
;; Selection is driven by `punch-line-glyph-style' (default 'auto, which
;; picks per frame based on display capabilities).

;;; Code:

(require 'cl-lib)
(require 'nerd-icons nil t)

(defgroup punch-line-glyphs nil
  "Glyph rendering strategy for `punch-line'."
  :group 'punch-line)

(defcustom punch-line-glyph-style 'auto
  "Glyph style.
\\='auto picks per frame: nerd on GUI when nerd-icons is loaded,
unicode when the terminal can display the fallback char, else ascii."
  :type '(choice (const auto) (const nerd) (const unicode) (const ascii))
  :group 'punch-line-glyphs)

(defvar punch-line-glyph-table
  '(;; battery
    (battery-charging . (:fn nerd-icons-faicon :arg "nf-fa-plug"      :u "🔌" :a "[+]"))
    (battery-full     . (:fn nerd-icons-faicon :arg "nf-fa-battery"   :u "🔋" :a "[####]"))
    (battery-high     . (:fn nerd-icons-faicon :arg "nf-fa-battery_3" :u "🔋" :a "[### ]"))
    (battery-mid      . (:fn nerd-icons-faicon :arg "nf-fa-battery_2" :u "🔋" :a "[##  ]"))
    (battery-low      . (:fn nerd-icons-faicon :arg "nf-fa-battery_1" :u "🪫" :a "[#   ]"))
    (battery-empty    . (:fn nerd-icons-faicon :arg "nf-fa-battery_0" :u "🪫" :a "[    ]"))
    ;; diagnostics
    (info             . (:fn nerd-icons-codicon :arg "nf-cod-lightbulb" :u "💡" :a "i"))
    (warning          . (:fn nerd-icons-codicon :arg "nf-cod-warning"   :u "⚠"  :a "!"))
    (error            . (:fn nerd-icons-codicon :arg "nf-cod-error"     :u "✗"  :a "x"))
    (pulse            . (:fn nerd-icons-codicon :arg "nf-cod-pulse"     :u "●"  :a "*"))
    ;; vc
    (git-branch       . (:fn nerd-icons-octicon :arg "nf-oct-git_branch" :u "" :a "git:"))
    (github           . (:fn nerd-icons-octicon :arg "nf-oct-mark_github" :u "⎇" :a "GH"))
    (pencil           . (:fn nerd-icons-octicon :arg "nf-oct-pencil"      :u "✎" :a "*"))
    ;; music
    (music            . (:fn nerd-icons-faicon :arg "nf-fa-music"   :u "♪" :a "M"))
    (spotify          . (:fn nerd-icons-faicon :arg "nf-fa-spotify" :u "♪" :a "S"))
    ;; misc
    (terminal         . (:fn nerd-icons-devicon :arg "nf-dev-terminal" :u "▶" :a ">_"))
    (cpu              . (:fn nerd-icons-octicon :arg "nf-oct-cpu"      :u "⚙" :a "cpu"))
    (memory           . (:fn nerd-icons-faicon  :arg "nf-fa-memory"    :u "▤" :a "mem"))
    (package          . (:fn nerd-icons-codicon :arg "nf-cod-package"  :u "📦" :a "pkg"))
    (fire             . (:fn nerd-icons-faicon  :arg "nf-fa-gripfire"  :u "🔥" :a "@"))
    (clock            . (:fn nerd-icons-faicon  :arg "nf-fa-clock_o"   :u "⏱" :a "@"))
    (chat             . (:fn nerd-icons-faicon  :arg "nf-fa-comments"  :u "💬" :a "#"))
    ;; weather
    (w-sunny          . (:fn nerd-icons-mdicon :arg "nf-md-weather_sunny"         :u "☀"  :a "sun"))
    (w-partly         . (:fn nerd-icons-mdicon :arg "nf-md-weather_partly_cloudy" :u "⛅" :a "pcl"))
    (w-fog            . (:fn nerd-icons-mdicon :arg "nf-md-weather_fog"           :u "🌫" :a "fog"))
    (w-drizzle        . (:fn nerd-icons-mdicon :arg "nf-md-weather_rainy"         :u "🌦" :a "drz"))
    (w-rain           . (:fn nerd-icons-mdicon :arg "nf-md-weather_pouring"       :u "🌧" :a "rai"))
    (w-snow           . (:fn nerd-icons-mdicon :arg "nf-md-weather_snowy"         :u "🌨" :a "sno"))
    (w-snow-heavy     . (:fn nerd-icons-mdicon :arg "nf-md-weather_snowy_heavy"   :u "❄"  :a "SNO"))
    (w-thunder        . (:fn nerd-icons-mdicon :arg "nf-md-weather_lightning"     :u "⛈" :a "thu"))
    (w-cloudy         . (:fn nerd-icons-mdicon :arg "nf-md-weather_cloudy"        :u "☁"  :a "cld")))
  "Map of glyph KEY to plist (:fn :arg :u :a) for nerd/unicode/ascii.")

(defun punch-line-glyph--style-for-frame ()
  "Resolve `punch-line-glyph-style' against the current frame."
  (if (not (eq punch-line-glyph-style 'auto))
      punch-line-glyph-style
    (cond
     ((and (display-graphic-p) (featurep 'nerd-icons)) 'nerd)
     ((char-displayable-p ?⚠) 'unicode)
     (t 'ascii))))

(cl-defun punch-line-glyph (key &key v-adjust)
  "Render glyph KEY according to current style.
V-ADJUST applies only to the nerd style (passed through to nerd-icons)."
  (let* ((spec (alist-get key punch-line-glyph-table))
         (style (punch-line-glyph--style-for-frame)))
    (cond
     ((null spec) "")
     ((eq style 'nerd)
      (let ((fn (plist-get spec :fn))
            (arg (plist-get spec :arg)))
        (if (and (featurep 'nerd-icons) (fboundp fn))
            (if v-adjust
                (funcall fn arg :v-adjust v-adjust)
              (funcall fn arg))
          ;; nerd-icons not actually loadable, fall through to unicode
          (or (plist-get spec :u) (plist-get spec :a) ""))))
     ((eq style 'unicode)
      (or (plist-get spec :u) (plist-get spec :a) ""))
     (t
      (or (plist-get spec :a) "")))))

(provide 'punch-line-glyphs)
;;; punch-line-glyphs.el ends here
