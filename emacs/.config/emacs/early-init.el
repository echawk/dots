;; Increases Garbage Collection During Startup
(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;; Cleanup the inital screen
(setq inhibit-startup-message t
      inhibit-startup-screen t
      frame-inhibit-implied-resize t
      desktop-resore-forces-onscreen nil)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(tool-bar-mode -1)
;;(tooltip-mode -1)
(global-display-line-numbers-mode 1)  ; Show line numbers
(advice-add #'x-apply-session-resources :override #'ignore) ; Ignore Xresources

;; https://reddit.com/r/emacs/comments/np6ey4/how_packageel_works_with_use_package/
(setq straight-check-for-modifications nil
      straight-vc-git-default-clone-depth 1
      package-enable-at-startup nil)

(set-face-attribute 'default nil :height 140)
;; Set the theme based on the time.
;; If it's before 6AM or after 8PM, switch to vivendi.
(let ((curr-hour (string-to-number
                  (format-time-string "%H" (current-time)))))
  (setq is-night-p (or (<= curr-hour 6)
                       (>= curr-hour (+ 12 8)))))

(setq theme (if is-night-p
                'modus-vivendi
              'modus-operandi))
(load-theme theme)
