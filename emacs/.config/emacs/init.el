; Increases Garbage Collection During Startup
(setq startup/gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;(load-theme 'leuven)
(load-theme 'wombat)
(set-face-attribute 'default nil :height 140)

; Cleanup the inital screen
(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; Disable the scrollbar
(set-fringe-mode 10) ; give some breathing room
(tool-bar-mode -1)   ; Disable the toolbar
(menu-bar-mode -1)   ; Disable the menubar
(tooltip-mode -1)    ; Disable tooltips
(global-display-line-numbers-mode 1) ; Show line numbers
(setq column-number-mode t) ; Show column number too
(fset 'yes-or-no-p 'y-or-n-p) ; Don't ask to spell out 'yes'

; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

; Be more like vim when scrolling
(setq scroll-step 1
      scroll-conservatively 10000)
(setq next-screen-context-lines 5)
(setq line-move-visual nil)

; basic functionality
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

; package stuff
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;(use-package rainbow-delimiters
;  :hook (prog-mode . rainbow-delimiters-mode))


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

; Better scheme editing.
(use-package geiser)
(use-package geiser-chez
  :after geiser)

; Simple LSP mode for emacs.
(use-package eglot)

; OCaml integration.
(use-package tuareg)

; Only init imaxima if the elisp files exist.
(when (file-exists-p "/usr/share/emacs/site-lisp/maxima")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/maxima")
  (autoload 'maxima-mode "maxima" "Maxima mode" t)
  (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
  (autoload 'maxima "maxima" "Maxima interaction" t)
  (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
  (setq imaxima-use-maxima-mode-flag t)
  ; Sets the font for the LaTeX output.
  (setq imaxima-fnt-size "LARGE")
  (setq imaxima-latex-preamble "\\usepackage{concrete}")
  (add-to-list 'auto-mode-alist '("\\.ma[cx]\\'" . maxima-mode)))

(when (file-exists-p "/usr/share/emacs/site-lisp/slime")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
  (require 'slime-autoloads)
  (setq inferior-lisp-program "sbcl"))

; Keep custom variables from polluting this file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
