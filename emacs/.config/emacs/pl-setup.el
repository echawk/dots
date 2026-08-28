;;; setup for editing code

(use-package scratch-plus
  :hook (prog-mode . scratch-plus-minor-mode)
  :config (scratch-plus-mode))

(defmacro me/add-to-eglot-server-programs (modes-lsp-cmd)
  "Add modes in MODES-LSP-CMD to eglot-server-programs if the LSP-CMD exists."
  `(dolist (modes-cmd ,modes-lsp-cmd)
     (let ((modes (car   modes-cmd))
           (cmd   (nth 1 modes-cmd)))
       (add-to-list 'eglot-server-programs
                    `(,modes . ,cmd)))))

(use-package eglot
  :defer
  :config
  ;; Don't log anything in the events buffer, offers a decent speed improvement.
  (setq eglot-events-buffer-size 0)
  ;; Don't log *anything*. If there are problems, it's easy enough to comment
  ;; this line out.
  (fset #'jsonrpc--log-event #'ignore)
  (me/add-to-eglot-server-programs
   '((crystal-mode  ("crystalline"))
     (d-mode        ("serve-d"))
     (elixir-mode   ("elixir-ls"))
     (racket-hash-lang-mode
      ("racket" "-l" "racket-langserver"))
     ((python-mode python-ts-mode)
      ("pyrefly" "lsp"))
     ;; TODO: integrate julia into this
     ;; https://github.com/julia-vscode/LanguageServer.jl
     ((latex-mode
       tex-mode
       context-mode
       texinfo-mode
       bibtex-mode)
      ("texlab"))
     (vala-mode     ("vala-language-server")))))


(use-package eglot-plus
  :defer
  :ensure nil
  :config
  (eglot-plus-enable-eglot-everywhere)
  (eglot-plus-enable-quickload-file))


(use-package consult-eglot :after eglot)


;; LSP/DAP/formatter/linter manager ported from nvim.
;; (use-package mason
;;   :defer
;;   :config
;;   ;; FIXME: integrate with eglot-plus.
;;   (defun mason-install-if-uninstalled (lsp)
;;     (mason-ensure
;;      (lambda ()
;;        (unless (mason-installed-p lsp)
;;          (ignore-errors (mason-install lsp))))))
  
;;   (mason-setup
;;     (thread-last
;;       eglot-server-programs
;;       (mapcar #'cdr)
;;       (cl-remove-if #'compiled-function-p)
;;       (mapcar #'car)
;;       (mapcar #'mason-install-if-uninstalled))))

(use-package eldoc-mouse
  :defer
  :hook eldoc-mode)

;; I almost never want this to popup
;; (use-package eldoc-box
;;   :defer
;;   :hook ((eldoc-mode         . eldoc-box-hover-mode)))

;;; formatting

(use-package apheleia
  ;;:init
  ;;(apheleia-global-mode +1)
  :config
  (dolist (formatter-cmd '((dfmt      . ("dfmt" "--indent_size" "2"
                                         "--soft_max_line_length" "80"
                                         "--indent_style" "space"
                                         "--brace_style" "otbs"
                                         filepath))
                           (zigfmt    . ("zig" "fmt" filepath))
                           (hindent   . ("hindent"))
                           (smlfmt    . ("smlfmt" "--force"))
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
                            (shen-mode       . lisp-indent)
                            (sml-mode        . smlfmt)
                            (zig-mode        . zigfmt))
                          nil)
    (add-to-list #'apheleia-mode-alist mode-formatter))

  ;; TODO: impelement a buffer local variable that can be used to
  ;; override all of these checks.
  (defun me/get-formatter-backend ()
    "Returns an appropriate backend for formatting the current buffer.

Will return one of the following symbols:
- apheleia
- eglot
- treesit
- indent

"
    (interactive)
    (or (when
            (let* ((aph-func (cdr (assoc major-mode apheleia-mode-alist)))
                   (aph-func-int (cdr (assoc aph-func apheleia-formatters))))
              (and
               apheleia-mode
               aph-func
               (pcase aph-func-int
                 ((pred atom)  (fboundp aph-func-int))
                 ((pred listp) (executable-find (car aph-func-int))))))
          'apheleia)
        (when (and (fboundp #'eglot-managed-p)
                   (eglot-managed-p)
                   (not
                    (member :documentFormattingProvider
                            eglot-ignored-server-capabilities)))
          'eglot)
        (when (and (treesit-available-p)
                   (string-match "*-ts-*" (symbol-name major-mode)))
          'treesit)
        'indent))

  (defun me/format-buffer (&optional formatter)
    "My custom format-buffer command. Integrates w/ Apheleia, Eglot & -TS- modes.

The heirarchy is as follows:
- `apheleia-format-buffer'
- `eglot-format-buffer'
- `treesit-indent-region'
- `indent-region'

Additionally, any of these functions can be requested explicity, by providing
FORMATTER as the optional argument. FORMATTER is expected to be one of the
following symbols:
- \\='apheleia
- \\='eglot
- \\='treesit
- \\='indent

It will also remove any trailing whitespace from the end of any line from
the file.
"
    (interactive)
    (setq formatter (if formatter formatter (me/get-formatter-backend)))
    (cl-flet
        ((apheleia-fmt ()
           (apheleia-format-buffer (cdr (assoc major-mode apheleia-mode-alist))))
         (eglot-fmt ()
           (eglot-format          (point-min) (point-max)))
         (treesit-fmt ()
           (treesit-indent-region (point-min) (point-max)))
         (indent-fmt ()
           (indent-region         (point-min) (point-max))))
      (pcase formatter
        ('apheleia (apheleia-fmt))
        ('eglot    (eglot-fmt))
        ('treesit  (treesit-fmt))
        ('indent   (indent-fmt))))
    (delete-trailing-whitespace (point-min) (point-max)))

  ;; FIXME: seems to have strange behavior at times...
  (advice-add
   #'apheleia-format-after-save
   :around
   (lambda (orig &rest args)
     "Use my custom format-buffer command if applicable"
     (let ((formatter (me/get-formatter-backend)))
       (cond
        ((boundp 'me/apheleia-preferred-backend)
         (me/format-buffer me/apheleia-preferred-backend))
        ((not (eq 'apheleia formatter))
         (me/format-buffer formatter))
        (t (apply orig args)))))))

;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install t)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

;; (use-package flycheck-xcode
;;   :ensure t
;;   :commands (flycheck-xcode-setup)
;;   :init
;;   (mapc
;;    (lambda (x)
;;      (add-hook x #'flycheck-xcode-setup))
;;    '(c-mode-hook c++-mode-hook objc-mode-hook swift-mode-hook)))

;; (use-package flymake-x)

(use-package symbol-overlay
  :defer
  :hook (prog-mode . symbol-overlay-mode))

(use-package casual-symbol-overlay
  :after symbol-overlay
  :bind (:map prog-mode-map
              ("C-c o" . casual-symbol-overlay-tmenu)))



;; TODO: configure this package more.
;; Specifically the keybinds to slurp/barf sexps, since the default bindings
;; are a little strange imo.
(use-package paredit
  :defer
  :hook
  ((emacs-lisp-mode
    scheme-mode
    geiser-mode
    racket-mode
    racket-hash-lang-mode
    lisp-mode)
   . enable-paredit-mode))

(use-package lisp-docstring-toggle
  :ensure nil
  :hook
  ((emacs-lisp-mode lisp-mode scheme-mode) . lisp-docstring-toggle-setup)
  :init
  (unless (package-installed-p (intern "lisp-docstring-toggle"))
    (package-vc-install
     '(lisp-docstring-toggle :vc-backend Git
                             :url "https://github.com/gggion/lisp-docstring-toggle"))))


;;; programming lang setup


(me/setup-auto-mode "\\.bqn"   bqn-mode)
(me/setup-auto-mode "\\.cr"    crystal-mode)
;;(use-package ob-crystal)
(me/setup-auto-mode "\\.d"     d-mode)
(me/setup-auto-mode "\\.fs"    fsharp-mode)
;;(use-package ob-fsharp)
(me/setup-auto-mode "\\.fut"   futhark-mode)
(me/setup-auto-mode "\\.meson" meson-mode)
(me/setup-auto-mode "\\.nim"   nim-mode)
(me/setup-auto-mode "\\.go"    go-mode)
(me/setup-auto-mode "\\.hy"    hy-mode)
(me/setup-auto-mode "\\.fnl"   fennel-mode)
(me/setup-auto-mode "\\.vala"  vala-mode)
(me/setup-auto-mode "\\.vim"   vimrc-mode)
(me/setup-auto-mode "\\.zig"   zig-mode)
(me/setup-auto-mode "\\.ua"    uiua-mode)
(me/setup-auto-mode "\\.nasm"  nasm-mode)
(me/setup-auto-mode
 "\\.clj" clojure-mode (use-package inf-clojure :after clojure-mode))
(me/setup-auto-mode
 "\\.exs?" elixir-mode (use-package inf-elixir :after elixir-mode))
;;(use-package ob-elixir)
(me/setup-auto-mode "\\.lfe" lfe-mode)
;;(use-package ob-lfe)
(me/setup-auto-mode
 "\\.sml" sml-mode
 (progn
   (setq sml-program-name "hamlet")
   (use-package sml-basis :after sml-mode)))
;;(use-package ob-sml)
(me/setup-auto-mode "\\.lua" lua-mode)
(me/setup-auto-mode "\\.4th" forth-mode (setq forth-executable "gforth"))
(me/setup-auto-mode "\\.idr" idris-mode (setq idris-interpreter-path "idris2"))
(me/setup-auto-mode
 "\\.shen" shen-mode (setq inferior-shen-program "shen-sbcl"))
(me/setup-auto-mode
 "\\.ij[rstp]$" j-mode (setq j-console-cmd "/usr/lib/j9/bin/jconsole"))
(me/setup-auto-mode
 "\\.rb" ruby-mode
 (progn
   (let ((gem-bindir (concat (getenv "GEM_PATH") "/bin/")))
     (when (file-exists-p gem-bindir)
       (setq exec-path (cons gem-bindir exec-path))))
   (use-package inf-ruby :defer
     :after ruby-mode
     :hook (ruby-mode . inf-ruby-minor-mode))))
(me/setup-auto-mode "\\.applescript" applescript-mode)
(me/setup-auto-mode "\\.erl" erlang-mode :package erlang)
(me/setup-auto-mode
 "\\.jl$"
 ess-julia-mode
 :package ess
 (progn
   (use-package julia-mode :defer)))
;; NOTE: look into this package.
;; (use-package ess-view-data
;;   :defer t)
(me/setup-auto-mode
 "\\.R$"
 ess-r-mode
 :package ess
 (progn
   (defun me/is-r-pacakge-installed-p (pkgname)
     (zerop
      (shell-command
       (format "Rscript -e 'if(requireNamespace(\"%s\", quietly=TRUE)) q(status=0) else q(status=1)'" pkgname))))

   ;;(Me/is-cran-pacakge-installed-p "languageserver")
   (defun me/install-r-cran-pacakge (pkgname)
     (shell-command
      (format "Rscript -e 'options(repos = \"https://cran.r-project.org\")' -e 'install.packages(\"%s\")'" pkgname)))

   (defun me/install-r-github-package (user/repo)
     (unless (me/is-r-pacakge-installed-p "remotes")
       (me/install-r-cran-pacakge "remotes"))
     (shell-command
      (format "Rscript -e 'if(requireNamespace(\"remotes\")) remotes::install_github(\"%s\")' else q(status=1)" user/repo)))

   (unless (me/is-r-pacakge-installed-p "languageserver")
     ;;(me/install-r-github-package "REditorSupport/languageserver")
     (me/install-r-cran-pacakge "langaugeserver"))))
(me/setup-auto-mode
 "\\.ml[iylp]?$"
 tuareg-mode
 :package tuareg
 ;; Ensure that `ocamllsp` is in Emacs' $PATH.
 (let ((opam-bindir (concat (getenv "HOME") "/.opam/default/bin/")))
   (when (file-exists-p opam-bindir)
     (add-to-list 'exec-path opam-bindir)))
 (setq tuareg-indent-align-with-first-arg t)
 (setq tuareg-match-patterns-aligned t)
 (use-package utop
   :after tuareg
   :hook ((tuareg-mode . utop-minor-mode))
   :custom
   (utop-command "opam exec -- dune utop . -- -emacs")))
;; (me/setup-auto-mode "\\.ml[iylp]?$" neocaml-mode :package neocaml)
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '((neocaml-mode neocaml-interface-mode) . ("ocamllsp"))))
;; FIXME: figure out how to get proof general from deciding that it will
;; automatically reload the windows **without** my input. I really really really
;; have had enough of package authors thinking that they know best and decide
;; to refresh my emacs instance and open up buffers where there shouldn't be
;; any.......
(me/setup-auto-mode
 "\\.v$"
 coq-mode
 :package proof-general
 (use-package company-coq :after proof-general
   ;; FIXME: enable company-coq mode when we enter coq-mode - not sure why
   ;; below doesn't work.
   ;;:hook ((coq-mode . company-coq-mode))
   :config
   (add-to-list completion-at-point-functions
                (cape-company-to-capf #'company-coq))))
(me/setup-auto-mode
 "\\.hs" haskell-mode
 (progn
   (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
   (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
   ;; FIXME: check to see if I can use flymake instead.
   (use-package dante
     :after haskell-mode
     :commands 'dante-mode
     :hook ((haskell-mode . flymake-mode)
            (haskell-mode . dante-mode))
     :config
     (defalias 'flymake-hlint
       (flymake-flycheck-diagnostic-function-for 'haskell-hlint))
     (add-to-list 'flymake-diagnostic-functions 'flymake-hlint)

     ;; Generate a capf backend...
     ;; Way to do as a list?
     ;; (dolist (company-backend (list #'dante-company))
     ;;   (add-to-list completion-at-point-functions
     ;;                (cape-company-to-capf #'company-backend)))
     (add-to-list completion-at-point-functions
                  (cape-company-to-capf #'dante-company)))))
(me/setup-auto-mode
 "\\.apl"
 gnu-apl-mode
 (add-hook 'gnu-apl-mode-hook             #'(lambda () (set-input-method "APL-Z")))
 (add-hook 'gnu-apl-interactive-mode-hook #'(lambda () (set-input-method "APL-Z"))))
(me/setup-auto-mode
 "\\.tex$"
 LaTeX-mode
 :package auctex
 (dolist (mode (list #'visual-line-mode #'LaTeX-math-mode #'jinx-mode))
   (add-hook 'LaTeX-mode-hook mode))

 (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

 ;; https://tectonic-typesetting.github.io/book/latest/howto/auctex-setup/index.html
 ;; (add-to-list 'TeX-engine-alist '(default
 ;;                                  "Tectonic"
 ;;                                  "tectonic -X compile -f plain %T"
 ;;                                  "tectonic -X watch"
 ;;                                  nil))
 ;; (setq LaTeX-command-style '(("" "%(latenx)")))
 ;; (let ((tex-list (assoc "TeX" TeX-command-list))
 ;;       (latex-list (assoc "LaTeX" TeX-command-list)))
 ;;   (setf (cadr tex-list) "%(tex)"
 ;;         (cadr latex-list) "%l"))

 (setq
  ;; Make pdf-tools the default viewer for auctex.
  TeX-view-program-selection '((output-pdf "PDF Tools"))
  TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
  TeX-source-correlate-mode t
  TeX-source-correlate-start-server t
  TeX-engine 'xetex
  TeX-process-asynchronous t
  TeX-check-TeX nil
  TeX-auto-save nil
  TeX-parse-self t)

 (setq-default TeX-master nil))

;; Better LaTeX editing.

;; (use-package auctex-cluttex
;;   :after auctex)
;;(add-hook 'LaTeX-mode-hook #'auctex-cluttex-mode)
(me/setup-auto-mode
 "\\.md$"
 markdown-mode
 (add-hook 'markdown-mode-hook #'jinx-mode)
 (setq markdown-command "lowdown -s -Thtml")
 (use-package markdown-preview-mode :defer))
(me/eval-form-on-first-command-run
 imaxima
 (use-package maxima
   :ensure nil
   :if (file-exists-p "/usr/share/emacs/site-lisp/maxima")
   :commands (maxima-mode maxima imaxima imath-mode)
   :defer
   :mode ("\\.ma[cx]\\'" . maxima-mode)
   :config
   (setq imaxima-use-maxima-mode-flag t)
   (setq imaxima-fnt-size "LARGE")
   (setq imaxima-latex-preamble "\\usepackage{concrete}") ;; Sets the font for the LaTeX output.
   ;; Needed to fix commands section above. TODO: see if I can remove this.
   (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)
   (autoload 'imath-mode "imath" "Imath mode for math formula input" t)))
;;; python setup
;;(use-package pyenv)
;; elpy
;; auto-virtualenv                20250608.1633  available    melpa    Automatically activate Python virtualenvs based on project directory
;; auto-virtualenvwrapper         20230317.1313  available    melpa    Lightweight auto activate python virtualenvs
(use-package auto-virtualenv
  :hook (python-ts-mode . (lambda ()
                            (setq auto-virtualenv-verbose t)
                            (auto-virtualenv-setup))))

;; Custom snobol mode.
;; (use-package snobol-mode
;;   :ensure nil
;;   :init
;;   (unless (package-installed-p (intern "snobol-mode"))
;;     (package-vc-install "https://github.com/echawk/snobol-mode"
;;                         :last-release))
;;   :mode ("\\.sno" . snobol-mode))

;; Better scheme editing.
;; (use-package geiser :defer
;;   :defer
;;   :custom
;;   (geiser-active-implementations '(guile3 racket)))
;; (use-package geiser-guile
;;   :after geiser
;;   :custom (geiser-guile-binary "guile3"))

;; (use-package macrostep-geiser
;;   :after geiser-mode
;;   :hook ((geiser-mode . macrostep-geiser-setup)))

;; https://eshelyaron.com/sweep.html
;; (use-package prolog
;;   :ensure nil
;;   :load-path "prolog"
;;   :defer
;;   ;; Have <file>.(P|pl), be recognized as prolog source files.
;;   ;; Have <file>.m be recognized as mercury source file.
;;   :mode (("\\.P\\'"  . prolog-mode)
;;          ("\\.pl\\'" . prolog-mode)
;;          ("\\.m$"    . mercury-mode))
;;   :init
;;   (let ((pl-dir (concat user-emacs-directory "prolog/")))
;;     (unless (file-exists-p pl-dir)
;;       (make-directory pl-dir)
;;       (url-copy-file "https://bruda.ca/_media/emacs/prolog.el"
;;                      (concat pl-dir "prolog.el"))))
;;   :config
;;   (setq prolog-system 'swi)
;;   ;; Custom code to allow for switching between prolog implementations.
;;   (let* ((custom-prologs '((trealla "tpl")
;;                            (scryer "scryer-prolog")))
;;          (new-prolog-program-name
;;           (seq-remove
;;            (lambda (p)
;;              (or (equal (car p) t)
;;                  (equal (cadr p) nil)))
;;            (seq-filter (lambda (p) (symbolp (car p)))
;;                        (append prolog-program-name custom-prologs)))))
;;     (dolist (prolog-pair new-prolog-program-name)
;;       (if (executable-find (cadr prolog-pair))
;;           (let* ((prolog-sys (car prolog-pair))
;;                  (prolog-exe (cadr prolog-pair))
;;                  (prolog-sym-str (symbol-name prolog-sys))
;;                  (func-name
;;                   (intern
;;                    (seq-concatenate
;;                     'string
;;                     "run-" prolog-sym-str "-prolog"))))
;;             (eval
;;              `(defun ,func-name ()
;;                 (interactive)
;;                 (let ((prolog-system ',prolog-sys)
;;                       (prolog-program-name ,prolog-exe))
;;                   (run-prolog t)))))))))

;; (use-package prolog-mode
;;   :ensure nil
;;   :mode (("\\.P\\'"  . prolog-mode)
;;          ("\\.pl\\'" . prolog-mode)
;;          ("\\.m$"    . mercury-mode)))


;; Refactoring mode:
;; https://github.com/Wilfred/emacs-refactor

;;(use-package emr :defer)

;; https://github.com/phantomics/april/tree/master
;; anaphora is a dependency of jpt-apl-mode.
;; (me/emacs-N-progn
;;  30
;;  (use-package anaphora :defer)
;;  (use-package jpt-apl-mode
;;    :vc (:url "https://github.com/jthing/apl-mode"
;;              :rev :newest)
;;    :defer))

;; (use-package agda
;;   :ensure nil
;;   :defer
;;   :if (executable-find "agda-mode")
;;   :commands (agda2-mode)
;;   :init
;;   (load-file (shell-command-to-string "agda-mode locate")))
;; (setq completion-at-point-functions
;;       (append
;;        completion-at-point-functions
;;        (mapcar #'cape-company-to-capf
;;                (list #'dante-company))))

;; Emacs lisp editing stuff.
;; https://github.com/emacs-elsa/Elsa
;; https://github.com/p3r7/awesome-elisp
;; https://github.com/gonewest818/elisp-lint
;; https://alphapapa.github.io/emacs-package-dev-handbook/
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
;; (use-package elisp-demap :defer)



;;; end pl setup

;; --- begin lisp setup ---

(me/eval-form-on-first-command-run
 macrostep-expand
 (use-package macrostep))
(me/eval-form-on-first-command-run
 srfi
 (use-package srfi
   :config
   ;; I'm kinda over trying to mess w/ Emacs advice combinators to make
   ;; this more reliable (cl-flet sucks). This works for now though.
   (advice-add
    #'srfi-browse-document-url
    :override
    (lambda (number) (eww-browse-url (srfi--document-url number))))
   ;; Add some keybinds that make this easier to use from evil-mode.
   :bind
   (:map srfi-mode-map
         ("C-<return>" . srfi-browse-document-url)
         ("C-c C-k"    . srfi-keyword)
         ("C-c C-l"    . srfi-browse-landing-page-url)
         ("C-c C-r"    . srfi-browse-repository-url)
         ("C-c C-s"    . srfi-search))))

(use-package elisp-slime-nav :defer)
(setq me/lisp-interface 'sly)
(setq me/supported-lisps '(sbcl ecl ccl clasp clisp abcl))
(setq inferior-lisp-program "sbcl")
(use-package sly
  :defer
  :if (eq me/lisp-interface 'sly)
  :mode (("\\.lisp\\'"  . lisp-mode)
         ("\\.lsp\\'"   . lisp-mode))
  :bind
  (:map sly-mode-map
        ("C-c C-i" . sly-interrupt)
        ("C-c C-b" . sly-eval-buffer))
  :config
  ;; Dynamic command generation for SLY
  (dolist (inf-lisp me/supported-lisps)
    (when (executable-find (format "%s" inf-lisp))
      (let* ((inf-lisp-str (symbol-name inf-lisp))
             (func-name (intern (concat "sly-" inf-lisp-str))))
        (eval
         `(defun ,func-name ()
            ,(format "Start %s and connect via SLY." inf-lisp-str)
            (interactive)
            (let ((inferior-lisp-program ,inf-lisp-str))
              (sly))))))))

(use-package sly-macrostep :after sly)

(dolist (hook '(sly-mode-hook
                emacs-lisp-mode-hook
                geiser-mode-hook
                clojure-mode-hook)
              nil)
  (add-hook hook #'prettify-symbols-mode))

(use-package racket-mode
  ;;:defer
  :mode "\\.rkt"
  :hook
  ((racket-xp-mode        . (lambda ()
                              (remove-hook 'pre-display-functions
                                           #'racket-xp-pre-redisplay
                                           t)))
   ((racket-mode
     racket-hash-lang-mode)
    . (lambda ()
        ;; Enable racket-xp which allow for goto-source, etc.
        (require 'racket-xp)
        (racket-xp-mode)

        ;; Enable unicode input/mostly for \Gamma & friends...
        (when (boundp #'agda2-mode)
          (require 'agda2-mode)
          (set-input-method "Agda")))))
  :init
  (defun me/racket-pkg-is-installed-p (pkg-name)
    (thread-last
      pkg-name
      (format "sh -c 'raco pkg show %s | tail -n 1'")
      (shell-command-to-string)
      (string-match-p (rx "[none]"))
      (not)))

  (defun me/racket-install-package (pkg-name)
    (unless
        (zerop
         (shell-command
          (format "sh -c 'yes Y | raco pkg install %s'" pkg-name)))
      (message
       (format "me/racket-install-package: unable to install '%s'" pkg-name))))

  (add-to-list +eglot-plus-install-lsp-servers+
               `((racket-mode racket-hash-lang-mode)
                 ,(lambda () (me/racket-pkg-is-installed-p "racket-langserver"))
                 ,(lambda () (me/racket-install-package "racket-langserver")))))

;; --- end lisp setup ---

(me/eval-form-on-first-command-run
 rmsbolt
 (use-package rmsbolt))

(me/eval-form-on-first-command-run
 realgud:gdb
 (use-package realgud))

(me/eval-form-on-first-command-run
 suggest
 (use-package suggest))

(me/eval-form-on-first-command-run
 package-lint-current-buffer
 (use-package package-lint))

;; (use-package xjupyter
;;   :ensure nil
;;   :init
;;   (unless (package-installed-p (intern "xjupyter"))
;;     (package-vc-install
;;      '(xjupyter :vc-backend Git
;;                 :url "https://github.com/commercial-emacs/xjupyter"))))

