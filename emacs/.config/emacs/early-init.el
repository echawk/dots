;;; early-init --- early init stuff
;;; Commentary:
;; My early-init.el file, this section is here just to get
;; Emacs to stop labelling this as an issue in flymake.

;;; Code:
;; Increases Garbage Collection During Startup

(defvar startup-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold startup-gc-cons-threshold)))

;; Cleanup the inital screen
(setq inhibit-startup-message t
      inhibit-startup-screen t
      frame-inhibit-implied-resize t
      desktop-resore-forces-onscreen nil)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(tool-bar-mode -1)
(menu-bar-mode -1)
;;(tooltip-mode -1)
(global-display-line-numbers-mode 1)  ; Show line numbers
(advice-add #'x-apply-session-resources :override #'ignore) ; Ignore Xresources

;; https://reddit.com/r/emacs/comments/np6ey4/how_packageel_works_with_use_package/
(setq package-enable-at-startup nil
      load-prefer-newer t)

(set-face-attribute 'default nil :height 130)

;; https://github.com/doomemacs/doomemacs/blob/master/lisp/doom.el#L314
;; Below is taken straight from DOOM. Time will tell if I'll keep it.
(unless (daemonp)
  ;; PERF: `file-name-handler-alist' is consulted on each call to `require',
  ;;   `load', or various file/io functions (like `expand-file-name' or
  ;;   `file-remote-p'). You get a noteable boost to startup time by unsetting
  ;;   or simplifying its value.
  (let ((old-value (default-toplevel-value 'file-name-handler-alist)))
    (setq file-name-handler-alist
          ;; HACK: If the bundled elisp for this Emacs install isn't
          ;;   byte-compiled (but is compressed), then leave the gzip file
          ;;   handler there so Emacs won't forget how to read read them.
          ;;
          ;;   calc-loaddefs.el is our heuristic for this because it is built-in
          ;;   to all supported versions of Emacs, and calc.el explicitly loads
          ;;   it uncompiled. This ensures that the only other, possible
          ;;   fallback would be calc-loaddefs.el.gz.
          (if (eval-when-compile
                (locate-file-internal "calc-loaddefs.el" load-path))
              nil
            (list (rassq 'jka-compr-handler old-value))))
    ;; Make sure the new value survives any current let-binding.
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    ;; Remember it so it can be reset where needed.
    (put 'file-name-handler-alist 'initial-value old-value)))

;; (unless noninteractive
;;   ;; PERF: Resizing the Emacs frame (to accommodate fonts that are smaller or
;;   ;;   larger than the system font) appears to impact startup time
;;   ;;   dramatically. The larger the delta in font size, the greater the delay.
;;   ;;   Even trivial deltas can yield a ~1000ms loss, though it varies wildly
;;   ;;   depending on font size.
;;   (setq frame-inhibit-implied-resize t)

;;   ;; PERF,UX: Reduce *Message* noise at startup. An empty scratch buffer (or
;;   ;;   the dashboard) is more than enough, and faster to display.
;;   (setq inhibit-startup-screen t
;;         inhibit-startup-echo-area-message user-login-name)
;;   ;; PERF,UX: Remove "For information about GNU Emacs..." message at startup.
;;   ;;   It's redundant with our dashboard and incurs a premature redraw.
;;   (advice-add #'display-startup-echo-area-message :override #'ignore)
;;   ;; PERF: Suppress the vanilla startup screen completely. We've disabled it
;;   ;;   with `inhibit-startup-screen', but it would still initialize anyway.
;;   ;;   This involves some file IO and/or bitmap work (depending on the frame
;;   ;;   type) that we can no-op for a free 50-100ms boost in startup time.
;;   (advice-add #'display-startup-screen :override #'ignore))

(defun me/is-night-p ()
  "Return t if the current time is between 8PM and 6AM, nil otherwise."
  (let ((curr-hour (string-to-number
                    (format-time-string "%H" (current-time)))))
    (or (<= curr-hour 6)
        (>= curr-hour (+ 12 8)))))

(defun me/set-theme ()
  "Set the theme based on the time of day."
  (load-theme
   (if (me/is-night-p)
       'modus-vivendi
     'modus-operandi)))

(defun me/set-font ()
  "Set Emacs's font for different systems."
  (pcase system-type
    (darwin
     (if (member "Victor Mono" (font-family-list))
         (progn
           (set-face-attribute 'default nil :font "Victor Mono-14")
           (set-face-attribute 'font-lock-comment-face nil :slant 'italic :font "Victor Mono-14"))
       (message "'Victor Mono' is not installed. Run: brew install --cask font-victor-mono")))))

;; Set the font.
(me/set-font)

;; https://github.com/haji-ali/elcron
;; Every 1/4 hour, reset the theme.
(run-with-timer 0 (* 15 60) 'me/set-theme)

(provide 'early-init)
;;; early-init.el ends here
