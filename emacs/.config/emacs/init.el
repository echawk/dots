(load-theme 'wombat)

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

; Don't ask for confirmation when opening large files.
(setq large-file-warning-threshold nil)

; Native Compilation.
(setq is-emacs-28 (>= emacs-major-version 28))
(setq package-native-compile is-emacs-28)
(setq native-comp-async-report-warnings-errors (not is-emacs-28))

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
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
        (url-retrieve-synchronously
          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
          'slient 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package 'use-package)
  (setq straight-use-package-by-default t))

(setq use-straight nil)
(if use-straight (straight-bootstrap) (package-bootstrap))

(use-package try :defer)
; Better 'M-x package-list-packages'
(use-package paradox
  :defer
  :config
  (paradox-enable))

(use-package async :defer)

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

(use-package which-key
  :defer
  :config
  (which-key-mode))

(use-package doom-themes :defer)
(use-package all-the-icons :defer)
(use-package doom-modeline
  :custom ((doom-modeline-height 15))
  :config
  (doom-modeline-mode 1))

;(use-package rainbow-delimiters
;  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-prog-extra
  :defer
  :hook ((prog-mode . hl-prog-extra-mode))
  :commands (hl-prog-extra-mode))

(use-package editorconfig
  :defer
  :config
  (editorconfig-mode 1))

(use-package undo-fu :defer)

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-lion
  :after evil
  :config
  (evil-lion-mode))

; Git frontend.
(use-package magit :defer)
(use-package forge :defer)

; Simple LSP mode for emacs.
(use-package eglot
  :defer
  :hook ((tuareg-mode . eglot-ensure)
         (caml-mode   . eglot-ensure)
         (go-mode     . eglot-ensure)
         (c-mode      . eglot-ensure)
         (zig-mode    . eglot-ensure)
         (elixir-mode . eglot-ensure)
         (erlang-mode . eglot-ensure)
         (LaTeX-mode  . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
    '((latex-mode tex-mode context-mode texinfo-mode bibtex-mode) . ("texlab")))
  (add-to-list 'eglot-server-programs
    '(elixir-mode . ("elixir-ls"))))


; In-buffer autocompletion.
(use-package company
  :defer
  :hook ((eglot--managed-mode   . company-mode)
         (lisp-interaction-mode . company-mode)
         (sly-mode              . company-mode))
  :bind (:map company-active-map
          ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.0))

; Language independent syntax highlighting via tree-sitter.
(use-package tree-sitter
  :defer
  :hook ((go-mode     . tree-sitter-hl-mode)
         (shell-mode  . tree-sitter-hl-mode)
         (python-mode . tree-sitter-hl-mode)
         (caml-mode   . tree-sitter-hl-mode)))
(use-package tree-sitter-langs :after tree-sitter)

(use-package apheleia
  :defer
  :init
  (apheleia-global-mode +1)
  :config
  (add-to-list 'apheleia-formatters '(zigfmt    . ("zig" "fmt" filepath)))
  (add-to-list 'apheleia-mode-alist '(caml-mode . ocamlformat))
  (add-to-list 'apheleia-mode-alist '(zig-mode  . zigfmt)))

(defun buffer-as-string ()
  "Returns the current buffer as a string."
  (buffer-substring-no-properties (point-min) (point-max)))

(use-package caml
  :defer
  :mode ("\\.ml[iylp]?$" . caml-mode)
  :interpreter (("ocaml"    . caml-mode)
                ("ocamlrun" . caml-mode))
  :bind (:map caml-mode-map
         ("C-c C-b" . 'custom-caml-eval-buffer)
         ("C-c C-r" . caml-eval-region))
  :config
  (autoload 'caml-mode "caml" "Major mode for editing OCaml code." t)
  (autoload 'run-caml "inf-caml" "Run an inferior OCaml process." t)
  (autoload 'camldebug "camldebug" "Run ocamldebug on program." t)
  (defun custom-caml-eval-buffer ()
    "Send the current buffer to OCaml and evaluate it."
    (interactive)
    (let ((buff (concat
                  (replace-regexp-in-string
                    "[ \t\n]*\\(;;[ \t\n]*\\)?\\'"
                    ""
                    (buffer-as-string))
                  ";;")))
      (with-current-buffer "*inferior-caml*"
        (goto-char (point-max))
        (comint-send-string "*inferior-caml*" buff)
        (let ((pos (point)))
          (comint-send-input)
          (save-excursion
            (goto-char pos)
            (insert buff))))
      (display-buffer "*inferior-caml*"))))

; SML
(use-package sml-mode
  :defer
  :init
  (setq sml-program-name "hamlet"))
(use-package sml-basis :defer)

; Go integration.
(use-package go-mode :defer)

; D integration.
(use-package d-mode :defer)

; Zig integration.
(use-package zig-mode :defer)

; ESS for R & Data Sci.
(use-package ess :defer)

; Have <file>.(P|pl), be recognized as prolog source files.
(dolist (item '("\\.P\\'" "\\.pl\\'") nil)
  (add-to-list 'auto-mode-alist (cons item 'prolog-mode)))

; Read EPUBs in Emacs!
(use-package nov
  :defer
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . visual-line-mode))

(use-package eww
  :defer
  :init
  (setq browse-url-browser-function 'eww-browse-url)
  :hook (eww-mode . visual-line-mode))

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
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . LaTeX-math-mode))
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
(use-package geiser
  :defer
  :init
  (setq geiser-active-implementations '(mit)))
(use-package geiser-mit     :after geiser)
(use-package macrostep        :defer)
(use-package macrostep-geiser :defer)
(use-package srfi :defer)

; Use sly instead of slime
(use-package sly
  :defer
  :hook ((sly-mode . (lambda ()
                       (unless (sly-connected-p) (save-excursion (sly))))))
  :init
  (setq inferior-lisp-program "sbcl"))
(use-package sly-macrostep :after sly)

; Erlang editing.
(use-package erlang :defer)

; Elixir editing.
(use-package elixir-mode :defer)
(use-package inf-elixir :defer)

(use-package lua-mode
  :defer
  :config
  (setq lua-default-application "luajit"))
(use-package fennel-mode :defer)

(use-package haskell-mode
  :defer
  :hook ((haskell-mode . haskell-indent-mode)
         (haskell-mode . interactive-haskell-mode)))

(use-package idris-mode
  :defer
  :config
  (setq idris-interpreter-path "idris2"))

; Keep custom variables from polluting this file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

; Prevent the scratch buffer from being killed.
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))

; Print startup time on emacs startup.
(defun display-startup-time ()
      (message "Emacs loaded in %s with %d garbage collections."
        (format "%.2f seconds"
          (float-time
            (time-subtract after-init-time before-init-time))) gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time)
