(load-theme 'modus-operandi)

;; package stuff
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(defun package-bootstrap ()
  "Function to bootstrap package.el."
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  ;; Only install 'use-package' if emacs version is below 29.
  (if (not (>= emacs-major-version 29))
      (unless (package-installed-p 'use-package)
        (package-install 'use-package)))
  (require 'use-package)
  (setq use-package-always-ensure t))

(defun straight-bootstrap ()
  "Function to bootstrap straight.el."
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
(if use-straight
    (straight-bootstrap)
  (package-bootstrap))

(use-package emacs
  :config
  ;; Enable flyspell in all programming modes.
  (add-hook 'prog-mode-hook #'flyspell-prog-mode)
  ;; Don't ask to spell out 'yes'
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; Indentation
  (setq default-tab-width 4
        tab-width 4
        default-fill-column 80
        fill-column 80

        ;; Show column number too
        column-number-mode t

        ;; Be more like vim when scrolling
        scroll-step 1
        scroll-conservatively 10000
        next-screen-context-lines 5
        line-move-visual nil

        ;; Don't use file backups.
        ;; Don't ask for confirmation when opening large files.
        backup-inhibited t
        auto-save-default nil
        large-file-warning-threshold nil

        ;; Native Compilation.
        package-native-compile (>= emacs-major-version 28)
        native-comp-async-report-warnings-errors (not (>= emacs-major-version 28))

        ;; utf-8
        default-buffer-file-coding-system 'utf-8

        ;; Keep custom variables from polluting this file.
        custom-file (concat user-emacs-directory "custom.el")

        ;; Automatically visit symlink sources.
        find-file-visit-truename t
        vc-follow-symlinks t

        ;; Don't prompt when trying to kill a buffer with a live process.
        kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions))

  ;; utf-8
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; Prevent the scratch buffer from being killed.
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))

  ;; Don't error if we can't load the custom file
  (load custom-file 'noerror)
  (setq-default evil-indent-convert-tabs nil
                indent-tabs-mode nil
                tab-width 4
                evil-shift-round nil)
  :bind
  (("<escape>" . keyboard-escape-quit)))

;; Better 'M-x package-list-packages'
(use-package paradox
  :defer
  :custom
  (paradox-execute-asynchronously t)
  :config
  (paradox-enable))

(use-package async :defer)

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package which-key
  :config
  (which-key-mode))

(use-package helpful
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h C-d") #'helpful-at-point))

(use-package editorconfig
  :defer
  :config
  (editorconfig-mode 1))

(use-package undo-fu :defer)

(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-undo-system 'undo-fu)
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

(use-package doom-themes :defer)
(use-package all-the-icons :defer)
(use-package doom-modeline
  :custom ((doom-modeline-height 15))
  :hook (after-init . doom-modeline-mode))

(use-package rainbow-delimiters
  :defer
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package rainbow-mode :defer)
(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))
;;:config
;;(global-diff-hl-mode))

(use-package hl-prog-extra
  :defer
  :hook ((prog-mode . hl-prog-extra-mode))
  :commands (hl-prog-extra-mode))

;; Git frontend.
(use-package magit :defer)
(use-package forge :defer)

;; Simple LSP mode for emacs.
(use-package eglot
  :defer
  :hook ((tuareg-mode  . eglot-ensure)
         (caml-mode    . eglot-ensure)
         (go-mode      . eglot-ensure)
         (c-mode       . eglot-ensure)
         (crystal-mode . eglot-ensure)
         (vala-mode    . eglot-ensure)
         (zig-mode     . eglot-ensure)
         (elixir-mode  . eglot-ensure)
         (erlang-mode  . eglot-ensure)
         (LaTeX-mode   . eglot-ensure)
         (futhark-mode . eglot-ensure)
         (sh-mode      . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '((latex-mode
                  tex-mode
                  context-mode
                  texinfo-mode
                  bibtex-mode) . ("texlab")))
  (add-to-list 'eglot-server-programs
               '(vala-mode . ("vala-language-server")))
  (add-to-list 'eglot-server-programs
               '(crystal-mode . ("crystalline")))
  (add-to-list 'eglot-server-programs
               '(elixir-mode . ("elixir-ls"))))

;; In-buffer auto-completion.
(use-package corfu
  ;; Enable info popups whenever we use corfu.
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-auto t)  ;; Enable auto-completion.
  (corfu-cycle t) ;; Enable cycling.

  (corfu-auto-prefix 2)  ;; Set the minimum prefix for completion.
  (corfu-auto-delay 0.0) ;; Disable delay for completions.
  (corfu-quit-at-boundary 'separator)
  (corfu-echo-documentation 0.25) ;; Echo documentation.

  (corfu-popupinfo-delay '(0.25 . 0.1)) 
  (corfu-popupinfo-hide nil) ;; Don't hide the popup when candidates switch.
  ;; https://github.com/minad/corfu#tab-and-go-completion
  :bind
  (:map corfu-map
        ("TAB"     . corfu-next)
        ([tab]     . corfu-next)
        ("S-TAB"   . corfu-previous)
        ([backtab] . corfu-previous))

  :init
  (global-corfu-mode))

;; Nice icons for corfu completions, depends on librsvg.
(use-package kind-icon
  :if (and (display-graphic-p) (image-type-available-p 'svg))
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package apheleia
  :defer
  :init
  (apheleia-global-mode +1)
  :config
  (dolist (formatter-cmd '((dfmt      . ("dfmt" "--indent_size" "2"
                                         "--soft_max_line_length" "80"
                                         "--indent_style" "space"
                                         "--brace_style" "otbs"
                                         filepath))
                           (zigfmt    . ("zig" "fmt" filepath))
                           (hindent   . ("hindent"))
                           (shfmt     . ("shfmt" "-i" "4" "-ci" "-kp" "-sr")))
                         nil)
    (add-to-list #'apheleia-formatters formatter-cmd))

  ;; Set custom formatters for various modes.
  (dolist (mode-formatter '((caml-mode       . ocamlformat)
                            (d-mode          . dfmt)
                            (haskell-mode    . hindent)
                            (emacs-lisp-mode . lisp-indent)
                            (zig-mode        . zigfmt))
                          nil)
    (add-to-list #'apheleia-mode-alist mode-formatter)))

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

;; SML
(use-package sml-mode
  :defer
  :init
  (setq sml-program-name "hamlet"))
(use-package sml-basis :defer)

(use-package go-mode :defer)
(use-package d-mode :defer)
(use-package crystal-mode :defer)
(use-package vala-mode :defer)
(use-package zig-mode :defer)

(use-package ess
  :defer
  :mode ("\\.jl$" . ess-julia-mode))
(use-package julia-mode :defer)

(use-package prolog
  :ensure nil
  :defer
  ;; Have <file>.(P|pl), be recognized as prolog source files.
  ;; Have <file>.m be recognized as mercury source file.
  :mode (("\\.P\\'"  . prolog-mode)
         ("\\.pl\\'" . prolog-mode)
         ("\\.m$"    . mercury-mode))
  :init
  ;; Prolog configuration.
  (let ((pl (concat user-emacs-directory "prolog.el")))
    (unless (file-exists-p pl)
      (url-copy-file "https://bruda.ca/_media/emacs/prolog.el" pl))
    (load pl))
  :custom
  (prolog-system 'swi)
  (prolog-program-name "swipl"))

(use-package org
  :defer
  :hook (org-mode . flyspell-mode)
  :config
  ;; Add in shortcuts for code blocks and whatnot.
  (require 'org-tempo)

  ;; Will automatically load a language if it is needed when in org-mode.
  ;; While somewhat cryptic, it beats having to explicitly enable every language.
  ;; NOTE: This isn't my own function, its from stackexchange.
  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load a language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages
                     (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages
                                     org-babel-load-languages))
      ad-do-it)))

;; Fix weird tab behavior in default org-mode. Preferable to "C-c '".
(use-package poly-org
  :after org)

(use-package focus :defer)
(use-package writeroom-mode
  :config
  ;; TODO: make this way prettier - macro?
  ;; Arguments: function, enable value, disable value
  ;; (defun writeroom-toggle-theme (arg)
  ;;   (cond
  ;;    ((= arg 1)  (load-theme 'modus-operandi))
  ;;    ((= arg -1) (load-theme 'modus-operandi))))
  (defun writeroom-toggle-line-numbers (arg)
    (cond
     ((= arg 1)  (display-line-numbers-mode 0))
     ((= arg -1) (display-line-numbers-mode 1))))
  (defun writeroom-toggle-focus-mode (arg)
    (cond
     ((= arg 1)  (focus-mode 1))
     ((= arg -1) (focus-mode 0))))
  ;; TODO: see if I can set a custom sans serif font as well.
  (defun writeroom-toggle-variable-pitch (arg)
    (cond
     ((= arg 1)  (variable-pitch-mode 1))
     ((= arg -1) (variable-pitch-mode 0))))
  ;; Add all of our functions to the local effects list.
  (dolist (func '(writeroom-toggle-line-numbers
                  writeroom-toggle-focus-mode
                  writeroom-toggle-variable-pitch)
                nil)
    (add-to-list #'writeroom-local-effects func)))

;; Read EPUBs in Emacs!
(use-package nov
  :defer
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . visual-line-mode))

(use-package vterm
  :defer
  :hook (vterm-mode . (lambda () (display-line-numbers-mode 0))))

(use-package pdf-tools
  :defer
  :init
  (pdf-loader-install :no-query))

(use-package eww
  :defer
  ;; Uncomment below for emacs to behave as the default web browser.
  ;; :init
  ;; (setq browse-url-browser-function 'eww-browse-url)
  :hook (eww-mode . visual-line-mode))

(use-package ement :defer)

(use-package define-word :defer)

(use-package maxima
  :ensure nil
  :if (file-exists-p "/usr/share/emacs/site-lisp/maxima")
  :commands (maxima-mode maxima imaxima imath-mode)
  :defer
  :mode ("\\.ma[cx]\\'" . maxima-mode)
  :custom
  (imaxima-use-maxima-mode-flag t)
  (imaxima-fnt-size "LARGE")
  (imaxima-latex-preamble "\\usepackage{concrete}") ;; Sets the font for the LaTeX output.
  :config
  ;; Needed to fix commands section above. TODO: see if I can remove this.
  (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
  (autoload 'imath-mode "imath" "Imath mode for math formula input" t))

;; Init singular if files exist.
(when (file-exists-p "/usr/share/singular/emacs")
  (add-to-list 'load-path "/usr/share/singular/emacs/")
  (autoload 'singular "singular" "Singular mode" t))

(use-package frimacs :defer)

;; Better LaTeX editing.
(use-package auctex
  :defer
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . flyspell-mode)
         (LaTeX-mode . auctex-cluttex-mode))
  :init
  ;; Make pdf-tools the default viewer for auctex.
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t

        TeX-auto-save nil
        TeX-parse-self t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :config
  (setq-default TeX-master nil))
(use-package auctex-cluttex
  :after auctex)

;; Markdown support.
(use-package markdown-mode
  :defer
  :hook (markdown-mode . flyspell-mode)
  :config
  (setq markdown-command "lowdown -s -Thtml"))
(use-package markdown-preview-mode :defer)

;; Better scheme editing.
(use-package geiser
  :defer
  :init
  (setq geiser-active-implementations '(chez)))
(use-package geiser-chez     :after geiser)
(use-package macrostep        :defer)
(use-package macrostep-geiser :defer)
(use-package srfi :defer)

;; Use sly instead of slime
(use-package sly
  :defer
  :hook ((sly-mode . (lambda ()
                       (unless (sly-connected-p) (save-excursion (sly))))))
  :custom
  (inferior-lisp-program "sbcl"))
(use-package sly-macrostep :after sly)

(use-package clojure-mode :defer)
(use-package inf-clojure :defer)

(use-package erlang :defer)
(use-package lfe-mode :defer)

(use-package elixir-mode :defer)
(use-package inf-elixir :defer)

(use-package lua-mode
  :defer
  :config
  (setq lua-default-application "luajit"))
(use-package fennel-mode :defer)

(use-package haskell-mode
  :defer
  :hook ((haskell-mode . haskell-indentation-mode)
         (haskell-mode . interactive-haskell-mode)))
(use-package dante
  :after haskell-mode
  :commands 'dante-mode
  :hook ((haskell-mode . flycheck-mode)
         (haskell-mode . dante-mode)))

(use-package idris-mode
  :defer
  :custom
  (idris-interpreter-path "idris2"))

(use-package futhark-mode :defer)

(use-package shen-elisp :defer)
(use-package shen-mode
  :defer
  :mode ("\\.shen$" . shen-mode)
  :config
  (setq inferior-shen-program "shen-ccl"))

;; Enable prettify-symbols-mode in some languages
(dolist (hook '(sly-mode-hook
                emacs-lisp-mode-hook
                geiser-mode-hook
                shen-mode-hook
                clojure-mode-hook)
              nil)
  (add-hook hook #'prettify-symbols-mode))

;; Print startup time on emacs startup.
(defun display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time))) gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time)
