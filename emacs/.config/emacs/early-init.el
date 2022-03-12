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

; https://reddit.com/r/emacs/comments/np6ey4/how_packageel_works_with_use_package/
(setq package-enable-at-startup nil)
