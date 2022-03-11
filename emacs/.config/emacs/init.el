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

; Native Compilation.
(setq package-native-compile (if (>= emacs-major-version 28) t nil))
;(setq comp-async-report-warnings-errors nil)

; package stuff
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(defun package-bootstrap ()
  "Bootstrap package.el"
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t))

(defun straight-bootstrap ()
  "Bootstrap straight.el"
  (defvar bootstrap-version)
  (let ((bootstrap-file
          (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-existsp bootstrap-file)
      (with-current-buffer
        (url-retrieve-synchronously
          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
          'slient 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t))

(setq use-straight nil)
(if use-straight
  (straight-bootstrap)
  (package-bootstrap))

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
  :custom ((doom-modeline-height 15))
  :config
  (doom-modeline-mode 1))

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

; Git frontend.
(use-package magit :defer)
(use-package forge :defer)

; Simple LSP mode for emacs.
(use-package eglot
  :defer
  :hook ((tuareg-mode . eglot-ensure)
         (caml-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (elixir-mode . eglot-ensure)
         (erlang-mode . eglot-ensure)
         (LaTeX-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
    '((latex-mode tex-mode context-mode texinfo-mode bibtex-mode) . ("texlab")))
  (add-to-list 'eglot-server-programs
    '(elixir-mode . ("elixir-ls"))))


; In-buffer autocompletion.
(use-package company
  :defer
  ; First hook is for any time we enable eglot, we want company mode running too.
  :hook ((eglot--managed-mode . company-mode)
         (lisp-interaction-mode . company-mode))
  :bind (:map company-active-map
          ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.0))

; Language independent syntax highlighting via tree-sitter.
(use-package tree-sitter
  :defer
  :hook ((go-mode . tree-sitter-hl-mode)
         (shell-mode . tree-sitter-hl-mode)
         (python-mode . tree-sitter-hl-mode)
         (caml-mode . tree-sitter-hl-mode)))
(use-package tree-sitter-langs :after tree-sitter)

; OCaml integration.
;(use-package tuareg
;  :defer
;  :custom
;  (setq tuareg-in-indent 0))
;  (setq tuareg-default-indent 0)
;  (setq tuareg-with-indent 0)
;  (setq tuareg-match-clause-indent 0)
;  (setq tuareg-match-when-indent 0)

; tuareg's default indentation behavior is pretty bad. See:
; https://github.com/ocaml/tuareg/issues/179
;(use-package ocp-indent
;  :defer
;  :hook ((tuareg-mode-hook . ocp-setup-indent)
;         (caml-mode . ocp-setup-indent)))

(use-package caml :defer
  :hook (before-save . ocamlformat)
  :interpreter (("ocaml" . caml-mode)
                ("ocamlrun" . caml-mode))
  :bind (:map caml-mode-map
         ("C-c C-b" . caml-eval-buffer))
  :init
  (add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . caml-mode))
  (autoload 'caml-mode "caml" "Major mode for editing OCaml code." t)
  (autoload 'run-caml "inf-caml" "Run an inferior OCaml process." t)
  (autoload 'camldebug "camldebug" "Run ocamldebug on program." t))

; Autoformatting for OCaml.
(use-package ocamlformat
  :defer
  :custom (ocamlformat-enable 'enable-outside-detected-project))

; Go integration.
(use-package go-mode
  :defer
  :hook (before-save . gofmt-before-save))

; ESS for R & Data Sci.
(use-package ess :defer)

; Have <file>.P, be recognized as prolog source files.
(add-to-list 'auto-mode-alist '("\\.P\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

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

; Better LaTeX editing.
(use-package auctex
  :hook ((LaTeX-mode-hook . visual-line-mode)
         (LaTeX-mode-hook . LaTeX-math-mode))
  :config
  (setq TeX-auto-save nil)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

; Markdown support.
(use-package markdown-mode :defer)
(use-package markdown-preview-mode
  :defer
  :config
  (setq markdown-command "lowdown -s -Thtml"))

; Better scheme editing.
(use-package geiser :defer)
(use-package geiser-chez
  :after geiser)
(use-package geiser-chicken
  :after geiser)
(use-package macrostep :defer)
(use-package macrostep-geiser :defer)
(use-package srfi :defer)

; Use sly instead of slime
(use-package sly
  :defer
  :config
  (setq inferior-lisp-program "sbcl"))

; Erlang editing.
(use-package edts
  :defer
  :hook (erlang-mode . edts-mode))

; Elixir editing.
(use-package elixir-mode :defer)
(use-package inf-elixir :defer)

; Use an ancient haskell mode since it has support for hugs.
; https://github.com/haskell/haskell-mode#fb0168b346465479d7af48eb5edbb246c0c9f6a7
; To remove warnings about (require 'cl), run:
; $ sed -i "s/require 'cl)/require 'cl-lib)/" *.el
(let ((haskell-mode-d (concat user-emacs-directory "haskell-mode")))
  (when (file-exists-p haskell-mode-d)
    (add-to-list 'load-path haskell-mode-d)
    (require 'haskell-mode-autoloads)))

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

; Print startup time on emacs startup.
(defun display-startup-time ()
      (message "Emacs loaded in %s with %d garbage collections."
        (format "%.2f seconds"
          (float-time
            (time-subtract after-init-time before-init-time))) gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time)
