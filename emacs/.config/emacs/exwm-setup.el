;;; config for exwm

(defun me/make-exwm-script ()
  "Create `exwm` script in `HOME/.local/bin/`."
  (let ((exwm-script (concat (getenv "HOME") "/.local/bin/exwm")))
    (write-region
     (concat
      "#!/bin/sh"
      "\n"
      "EMACS_IS_EXWM=1 emacs -mm --debug-init")
     nil
     exwm-script)
    (shell-command (concat "chmod +x " exwm-script))))

;; TODO: Integrate these both into exwm. (IE: don't depend on these pkgs)
(use-package exwm-mff
  :after exwm
  :hook (exwm-mode . exwm-mff-mode))
(use-package exwm-modeline
  :after exwm
  :hook (exwm-mode . exwm-modeline-mode))

;; https://github.com/johanwiden/exwm-setup
;; https://wiki.archlinux.org/title/EXWM
;; https://github.com/ch11ng/exwm/wiki/Configuration-Example

(use-package exwm
  :if (and (or (eq system-type 'gnu/linux)
               (eq system-type 'berkeley-unix))
           (getenv "EMACS_IS_EXWM"))
  :hook ((exwm-update-class . (lambda () (exwm-workspace-rename-buffer exwm-class-name))))
  :init
  (unless (file-exists-p (executable-find "exwm"))
    (me/make-exwm-script))
  :config

  ;; Make sure the battery is shown.
  (setq battery-mode-line-format " [%b%p%%|%t] ")
  (display-battery-mode)

  ;; Have the current time show up in the modeline.
  (setq display-time-default-load-average nil)
  (display-time-mode)

  (set-frame-parameter nil 'alpha-background 80)
  (add-to-list 'default-frame-alist '(alpha-background . 80))

  ;; https://gitea.petton.fr/DamienCassou/desktop-environment
  (defun volume-raise        () (interactive) (call-process-shell-command "volctrl i"))
  (defun volume-lower        () (interactive) (call-process-shell-command "volctrl d"))
  (defun volume-toggle-mute  () (interactive) (call-process-shell-command "volctrl m"))
  (defun brightness-inc      () (interactive) (call-process-shell-command "bri i"))
  (defun brightness-dec      () (interactive) (call-process-shell-command "bri d"))

  ;; Set the media keys. Maybe move this out?
  (global-set-key (kbd "<XF86AudioRaiseVolume>")  'volume-raise)
  (global-set-key (kbd "<XF86AudioLowerVolume>")  'volume-lower)
  (global-set-key (kbd "<XF86AudioMute>")         'volume-toggle-mute)
  (global-set-key (kbd "<XF86MonBrightnessUp>")   'brightness-inc)
  (global-set-key (kbd "<XF86MonBrightnessDown>") 'brightness-dec)

  ;; Might as well have a systemtray, since it can be handy at times.
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; Ctrl+q will send the next key directly.
  (define-key exwm-mode-map [\?C-q] 'exwm-input-send-next-key)

  (exwm-init)

  ;; Garbage collect every 15 minutes when running exwm.
  (let ((mins-15 (* 15 60)))
    (run-with-timer mins-15 mins-15 'garbage-collect))

  :custom
  (exwm-workspace-number 4)
  (exwm-input-global-keys
   `((,(kbd "s-r") . exwm-reset)
     (,(kbd "s-w") . exwm-workspace-switch)

     (,(kbd "s-f") . exwm-layout-toggle-fullscreen)

     (,(kbd "s-p") . (lambda (cmd)
                       (interactive (list (read-shell-command "λ ")))
                       (start-process-shell-command cmd nil cmd)))

     (,(kbd "s-,") . next-buffer)
     (,(kbd "s-.") . previous-buffer)

     (,(kbd "s-d") . delete-window)

     ;; TODO: Consider making the window key binds available to non-exwm Emacs.
     ;; FIXME: ^^^ I'd like to use these binds everywhere.
     (,(kbd "s-v") . split-window-right)
     (,(kbd "s-s") . split-window-below)

     (,(kbd "s-h") . windmove-left)
     (,(kbd "s-l") . windmove-right)
     (,(kbd "s-j") . windmove-down)
     (,(kbd "s-k") . windmove-up)

     (,(kbd "s-H") . windmove-swap-states-left)
     (,(kbd "s-L") . windmove-swap-states-right)
     (,(kbd "s-J") . windmove-swap-states-down)
     (,(kbd "s-K") . windmove-swap-states-up)

     (,(kbd "s-Q") . kill-this-buffer)

     ;; This handy little block makes it possible to switch to
     ;; the different workspaces by using s-<num>
     ,@(mapcar (lambda (i)
                 `(,(kbd (format "s-%d" i)) .
                   (lambda ()
                     (interactive)
                     (exwm-workspace-switch-create ,i))))
               (number-sequence 0 9))

     ,@(mapcar (lambda (i)
                 (let ((keys '(")" "!" "@" "#" "$" "%" "^" "&" "*" "(")))
                   `(,(kbd (concat "s-" (nth i keys))) .
                     (lambda ()
                       (interactive)
                       (exwm-workspace-move-window ,i)))))
               (number-sequence 0 9)))))
