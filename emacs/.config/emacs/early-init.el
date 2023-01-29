; Increases Garbage Collection During Startup
(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

; Cleanup the inital screen
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; Disable the scrollbar
(set-fringe-mode 10) ; give some breathing room
(tool-bar-mode -1)   ; Disable the toolbar
(menu-bar-mode -1)   ; Disable the menubar
(tooltip-mode -1)    ; Disable tooltips
(global-display-line-numbers-mode 1)  ; Show line numbers
(setq frame-inhibit-implied-resize t)
(setq desktop-resore-forces-onscreen nil)
(advice-add #'x-apply-session-resources :override #'ignore) ; Ignore Xresources
(setq straight-check-for-modifications nil)
;(setq straight-vc-git-default-clone-depth 1)

; https://reddit.com/r/emacs/comments/np6ey4/how_packageel_works_with_use_package/
(setq package-enable-at-startup nil)

(set-face-attribute 'default nil :height 140)
;; Set the theme based on the time.
;; If it's before 6AM or after 8PM, switch to vivendi.
(setq theme (let ((curr-hour (string-to-number
                              (format-time-string "%H" (current-time)))))
              (if (or (<= curr-hour 6)
                      (>= curr-hour (+ 12 8)))
                  'modus-vivendi
                'modus-operandi)))
(load-theme theme)
