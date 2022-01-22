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

; Indentation
(setq default-tab-width 4)
(setq tab-width 4)
(setq default-fill-column 80)
(setq fill-column 80)
(setq-default evil-indent-convert-tabs nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default evil-shift-round nil)

; Don't use file backups.
(setq backup-inhibited t)
(setq auto-save-default nil)

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
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;(use-package rainbow-delimiters
;  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-prog-extra
  :hook ((prog-mode . hl-prog-extra-mode))
  :commands (hl-prog-extra-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

; Better scheme editing.
(use-package geiser
  :defer)
(use-package geiser-chez
  :after geiser)

; Simple LSP mode for emacs.
(use-package eglot
  :defer
  :hook ((tuareg-mode . eglot-ensure)
         (go-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
    '((latex-mode tex-mode context-mode texinfo-mode bibtex-mode) . ("texlab"))))


; In-buffer autocompletion.
(use-package company
  :defer
  ; First hook is for any time we enable eglot, we want company mode running too.
  :hook (eglot--managed-mode . company-mode)
  :bind (:map company-active-map
          ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.0))

; OCaml integration.
(use-package tuareg
  :defer)

; Autoformatting for OCaml.
(use-package ocamlformat
  :custom (ocamlformat-enable 'enable-outside-detected-project)
  :hook (before-save . ocamlformat-before-save))

; Go integration.
(use-package go-mode
  :hook (before-save . gofmt-before-save))

; Have <file>.P, be recognized as prolog source files.
(add-to-list 'auto-mode-alist '("\\.P\\'" . prolog-mode))

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

; Use sly instead of slime
(use-package sly
  :init
  (setq inferior-lisp-program "sbcl"))

; Keep custom variables from polluting this file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

; Prevent the scratch buffer from being killed.
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

; Auto kill dead buffers.
; See: https://www.emacswiki.org/emacs/KillingBuffers#h5o-14
(require 'midnight)

(setq clean-buffer-list-delay-special 900)

(defvar clean-buffer-list-timer nil
  "Stores clean-buffer-list timer if there is one. You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")

; Run clean-buffer-list every 2 hours.
(setq clean-buffer-list-timer (run-at-time t 7200 'clean-buffer-list))

; Kill everything, clean-buffer-list is intelligent & won't kill unsaved buffers.
(setq clean-buffer-list-kill-regexps '("^.*$"))

; Keep these buffers untouched & prevent append multiple times.
(defvar clean-buffer-list-kill-never-buffer-names-init
  clean-buffer-list-kill-never-buffer-names
  "Init value for clean-buffer-list-kill-never-buffer-names")
(setq clean-buffer-list-kill-never-buffer-names
      (append
       '("*Messages*" "*scratch*")
       clean-buffer-list-kill-never-buffer-names-init))

; Prevent append multiple times.
(defvar clean-buffer-list-kill-never-regexps-init
  clean-buffer-list-kill-never-regexps
  "Init value for clean-buffer-list-kill-never-regexps")

; Append to *-init instead of itself
(setq clean-buffer-list-kill-never-regexps
      (append '("^\\*EMMS Playlist\\*.*$")
      clean-buffer-list-kill-never-regexps-init))
