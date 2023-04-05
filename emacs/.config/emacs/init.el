(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(defun package-bootstrap ()
  "Function to bootstrap package.el."
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  ;; Only install 'use-package' if emacs version is below 29,
  ;; and if use-package is not already installed.
  (if (and (< emacs-major-version 29)
           (not (package-installed-p 'use-package)))
      (package-install 'use-package))
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

(let ((use-straight nil))
  (if use-straight
      (straight-bootstrap)
    (package-bootstrap)))

(use-package emacs
  ;; Enable flyspell in all programming modes.
  :hook (prog-mode . flyspell-prog-mode)
  :custom
  ;; Indentation
  (default-tab-width 4)
  (tab-width 4)
  (default-fill-column 80)
  (fill-column 80)

  ;; Show column number too
  (column-number-mode t)

  ;; Be more like vim when scrolling
  (scroll-step 1)
  (scroll-conservatively 10000)
  (next-screen-context-lines 5)
  (line-move-visual nil)

  ;; Don't use file backups.
  ;; Don't ask for confirmation when opening large files.
  (backup-inhibited t)
  (auto-save-default nil)
  (large-file-warning-threshold nil)

  ;; Native Compilation.
  (package-native-compile (>= emacs-major-version 28))
  (native-comp-async-report-warnings-errors (not (>= emacs-major-version 28)))

  ;; utf-8
  (default-buffer-file-coding-system 'utf-8)

  ;; Keep custom variables from polluting this file.
  (custom-file (concat user-emacs-directory "custom.el"))

  ;; Automatically visit symlink sources.
  (find-file-visit-truename t)
  (vc-follow-symlinks t)

  (use-dialog-box nil)

  ;; Don't prompt when trying to kill a buffer with a live process.
  (kill-buffer-query-functions
   (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))
  :config
  ;; Automatically update buffers when contents change on disk.
  (global-auto-revert-mode)
  ;; Don't ask to spell out 'yes'
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; utf-8
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; Delete trailing whitespace before saving buffers.
  (add-hook 'before-save-hook #'(lambda () (delete-trailing-whitespace (point-min) (point-max))))

  ;; Print startup time & num of gcs done.
  (add-hook 'emacs-startup-hook #'(lambda ()
                                    (message "Emacs loaded in %s with %d garbage collections."
                                             (format "%.2f seconds"
                                                     (float-time
                                                      (time-subtract after-init-time before-init-time))) gcs-done)))

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
  :defer
  :commands (helpful-callable helpful-variable helpful-key helpful-at-point)
  :init
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
  :hook ((caml-mode    . eglot-ensure)
         (c-mode       . eglot-ensure)
         (crystal-mode . eglot-ensure)
         (elixir-mode  . eglot-ensure)
         (erlang-mode  . eglot-ensure)
         (futhark-mode . eglot-ensure)
         (go-mode      . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (LaTeX-mode   . eglot-ensure)
         (sh-mode      . eglot-ensure)
         (tuareg-mode  . eglot-ensure)
         (vala-mode    . eglot-ensure)
         (zig-mode     . eglot-ensure))
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

(use-package treesit
  ;; Need to make sure we don't try to install this from package.el
  :ensure nil
  :commands (treesit-install-language-grammar treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
        '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (c-sharp    . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
          (clojure    . ("https://github.com/sogaiu/tree-sitter-clojure"))
          (elixir     . ("https://github.com/elixir-lang/tree-sitter-elixir"))
          (erlang     . ("https://github.com/AbstractMachinesLab/tree-sitter-erlang"))
          (fennel     . ("https://github.com/TravonteD/tree-sitter-fennel"))
          (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
          (haskell    . ("https://github.com/tree-sitter/tree-sitter-haskell"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (julia      . ("https://github.com/tree-sitter/tree-sitter-julia"))
          (latex      . ("https://github.com/latex-lsp/tree-sitter-latex"))
          (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make       . ("https://github.com/alemuller/tree-sitter-make"))
          (ocaml      . ("https://github.com/tree-sitter/tree-sitter-ocaml" "master" "ocaml/src"))
          (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
          (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
          (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
          (zig        . ("https://github.com/GrayJack/tree-sitter-zig"))))
  :config
  ;; https://www.nathanfurnal.xyz/posts/building-tree-sitter-langs-emacs/#native-emacs-solution
  (defun treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
        (treesit-install-language-grammar lang)
        (message "`%s' parser was installed." lang)
        (sit-for 0.75)))))

;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :if (>= emacs-major-version 29)
  ;; :defer
  :config
  (global-treesit-auto-mode))

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
                            (elisp-mode      . lisp-indent)
                            (emacs-lisp-mode . lisp-indent)
                            (geiser-mode     . lisp-indent)
                            (haskell-mode    . hindent)
                            (scheme-mode     . lisp-indent)
                            (sh-mode         . shfmt)
                            (zig-mode        . zigfmt))
                          nil)
    (add-to-list #'apheleia-mode-alist mode-formatter)))


(use-package tuareg
  :defer
  :hook (tuareg-mode . (lambda () (electric-indent-mode 0)))
  :mode ("\\.ml[iylp]?$" . tuareg-mode)
  :custom
  (tuareg-indent-align-with-first-arg t)
  (tuareg-match-patterns-aligned t))

;; SML
(use-package sml-mode
  :defer
  :custom
  (sml-program-name "hamlet"))
(use-package sml-basis :defer)

(use-package crystal-mode :defer)
(use-package d-mode :defer)
(use-package fsharp-mode :defer)
(use-package go-mode :defer)
(use-package hy-mode :defer)
(use-package nim-mode :defer)
(use-package vala-mode :defer)
(use-package vimrc-mode :defer)
(use-package zig-mode :defer)

(use-package ess
  :defer
  :mode ("\\.jl$" . ess-julia-mode))
(use-package julia-mode :defer)

(use-package prolog
  :ensure nil
  :load-path "prolog"
  :defer
  ;; Have <file>.(P|pl), be recognized as prolog source files.
  ;; Have <file>.m be recognized as mercury source file.
  :mode (("\\.P\\'"  . prolog-mode)
         ("\\.pl\\'" . prolog-mode)
         ("\\.m$"    . mercury-mode))
  :init
  (let ((pl-dir (concat user-emacs-directory "prolog/")))
    (unless (file-exists-p pl-dir)
      (make-directory pl-dir)
      (url-copy-file "https://bruda.ca/_media/emacs/prolog.el"
                     (concat pl-dir "prolog.el"))))
  :custom
  (prolog-system 'swi)
  (prolog-program-name "swipl"))

;; Custom snobol mode.
(use-package snobol-mode
  :ensure nil
  :load-path "snobol-mode"
  :defer
  :mode ("\\.sno" . snobol-mode))

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
  :defer
  :commands (writeroom-mode)
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
  :hook ((pdf-view-mode . (lambda () (display-line-numbers-mode 0)))
         (pdf-view-mode . (lambda () (if is-night-p (pdf-view-midnight-minor-mode)))))
  :init
  (pdf-loader-install :no-query))

(use-package eww
  :defer
  ;; Uncomment below for emacs to behave as the default web browser.
  ;; :init
  ;; (setq browse-url-browser-function 'eww-browse-url)
  :hook (eww-mode . visual-line-mode))

(use-package google-translate :defer)

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
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :custom
  ;; Make pdf-tools the default viewer for auctex.
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (TeX-source-correlate-start-server t)

  (TeX-auto-save nil)
  (TeX-parse-self t)
  :config
  (setq-default TeX-master nil))
(use-package auctex-cluttex
  :after auctex)

;; Markdown support.
(use-package markdown-mode
  :defer
  :hook (markdown-mode . flyspell-mode)
  :custom
  (markdown-command "lowdown -s -Thtml"))
(use-package markdown-preview-mode :defer)

;; Better scheme editing.
(use-package geiser
  :defer
  :custom
  (geiser-active-implementations '(chez)))
(use-package geiser-chez     :after geiser)
(use-package macrostep
  :defer
  :bind
  (:map macrostep-keymap
        ("C-c C-e" . macrostep-expand)
        ("C-c C-u" . macrostep-collapse)
        ("C-c C-q" . macrostep-collapse-all)))
(use-package macrostep-geiser :defer)
;; TODO: override browse-url and have it use eww for srfi.
(use-package srfi
  :defer
  ;; Add some keybinds that make this easier to use from evil-mode.
  :bind
  (:map srfi-mode-map
        ("C-<return>" . srfi-browse-document-url)
        ("C-c C-k"    . srfi-keyword)
        ("C-c C-l"    . srfi-browse-landing-page-url)
        ("C-c C-r"    . srfi-browse-repository-url)
        ("C-c C-s"    . sfri-search)))

;; Use sly instead of slime
(use-package sly
  :defer
  :hook ((sly-mode . (lambda ()
                       (unless (sly-connected-p) (save-excursion (sly))))))
  :bind
  (:map sly-mode-map
        ("C-c C-i" . sly-interrupt)
        ("C-c C-b" . sly-eval-buffer))
  :custom
  (inferior-lisp-program "sbcl"))
(use-package sly-macrostep :after sly)

(use-package clojure-mode :defer)
(use-package inf-clojure :defer)

;; Depends on clojure-mdoe
(use-package carp-mode
  :ensure nil
  :if (file-exists-p (concat user-emacs-directory "carp-emacs/"))
  :commands (carp-mode run-carp)
  :load-path "carp-emacs"
  :defer
  :mode ("\\.carp$" . carp-mode)
  :config
  (require 'inf-carp-mode))

(use-package erlang :defer)
(use-package lfe-mode :defer)

(use-package elixir-mode :defer)
(use-package inf-elixir :defer)

(use-package lua-mode
  :defer
  :custom
  (lua-default-application "luajit"))
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
  :custom
  (inferior-shen-program "shen-scheme"))

;; Enable prettify-symbols-mode in some languages
(dolist (hook '(sly-mode-hook
                emacs-lisp-mode-hook
                geiser-mode-hook
                shen-mode-hook
                clojure-mode-hook)
              nil)
  (add-hook hook #'prettify-symbols-mode))

(use-package gnu-apl-mode
  :defer
  :hook ((gnu-apl-mode             . (lambda () (set-input-method "APL-Z")))
         (gnu-apl-interactive-mode . (lambda () (set-input-method "APL-Z")))))

(use-package forth-mode
  :defer
  :init
  (setq forth-executable "gforth"))


