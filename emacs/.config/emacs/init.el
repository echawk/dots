;;; init --- my Emacs init file

;;; Commentary:

;; This is my personal Emacs configuration.  It may be of use to you,
;; as I have configured quite a number of packages.  However, there is
;; an emphasis on lightweight or builtin packages in Emacs.  Also, by
;; default, every package is explicitly deferred unless if it is
;; absolutely required at init time.  Because of this, my init time hovers
;; around 0.5 to 1 second.  This could be reduced if you don't use evil.

(defun me/package-bootstrap ()
  "Function to bootstrap package.el."
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  ;; Only install 'use-package' if Emacs version is below 29,
  ;; and if use-package is not already installed.
  (if (and (< emacs-major-version 29)
           (not (package-installed-p 'use-package)))
      (package-install 'use-package))
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-compute-statistics t))

(defun me/quelpa-bootstrap ()
  "Function to bootstrap quelpa."
  (me/package-bootstrap)
  (use-package use-package-ensure
    :config
    (setq use-package-ensure-function 'quelpa))

  (unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents
       "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))
  (quelpa
   '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git"))
  (require 'quelpa-use-package))

(let ((pkg-system 'package-el))
  (pcase pkg-system
    ('package-el (me/package-bootstrap))
    ('quelpa (me/quelpa-bootstrap))))

(setq me/delete-trailing-whitespace nil)

(defmacro me/emacs-N-progn (N &rest body)
  (when (>= emacs-major-version N)
    `(progn ,@body)))



(setq me/modal-system nil)
(use-package emacs
  :hook ((prog-mode . display-fill-column-indicator-mode)
         (before-save . (lambda ()
                          ;; Somewhat better way of deleting whitespace
                          (when (or (not vc-mode) me/delete-trailing-whitespace)
                            (delete-trailing-whitespace (point-min) (point-max)))))
         (emacs-startup . (lambda ()
                            (message "Emacs loaded in %s with %d garbage collections."
                                     (format "%.2f seconds"
                                             (float-time
                                              (time-subtract after-init-time before-init-time))) gcs-done)))
         (after-save . executable-make-buffer-file-executable-if-script-p))

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
  (backup-inhibited t)
  ;; Don't ask for confirmation when opening large files.
  (large-file-warning-threshold nil)

  ;; I almost NEVER want to see the warnings buffer.
  (warning-minimum-level :emergency)
  ;; Now if only I could also disable the *Compile-Log* buffer...
  (compilation-window-height 0)
  ;; Prevent any buffer from making a new window
  (display-buffer-alist '(("\\.*" . (display-buffer-no-window))))

  ;; Better buffer naming.
  (uniquify-buffer-name-style 'post-forward)

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

  ;; Don't wrap lines
  (truncate-lines t)


  ;; Make minibuffer less stuttery.
  (read-minibuffer-restore-windows nil)

  ;; Don't prompt when trying to kill a buffer with a live process.
  (kill-buffer-query-functions
   (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))

  ;; I really really cannot stand how eldoc thinks its the most important
  ;; package that likes to take over buffers that I am referring to.
  (eldoc-echo-area-prefer-doc-buffer nil)
  (eldoc-idle-delay 0)
  :config
  ;; OS specific settings.
  (cond
   ((eq system-type 'darwin)
    (progn
      (setq mac-command-modifier 'control)
      ;; Emacs on macOS has now crash too many times with no reason for me to
      ;; feel comfortable disabling this.
      (setq auto-save-default t
            auto-save-timeout 30)
      ;; Only use this package on macOS. Otherwise things on Linux and BSD
      ;; behave very very incorrectly. Linux does not have the weird shell
      ;; issues that macOS has so this package hurts *far* more than it helps.
      (use-package exec-path-from-shell
        :config
        (dolist (var '("PATH"
                       "HOMEBREW_PREFIX" "HOMEBREW_CELLAR" "HOMEBREW_REPOSITORY"
                       "VIRTUAL_ENV" "PYTHONHOME"
                       "CPPFLAGS" "LDFLAGS"
                       "SSH_AUTH_SOCK" "SSH_AGENT_PID"))
          (add-to-list 'exec-path-from-shell-variables var))
        (exec-path-from-shell-initialize))))
   (t
    (progn
      (setq auto-save-default nil))
    )
   )

  ;; Section to disable eldoc since I almost never want it around.
  ;; If I do, then I still have means of enabling it.

  (defvar me/eldoc-enabled-p nil)

  (defun me/eldoc-advice (orig &rest args)
    (when me/eldoc-enabled-p
      (apply orig args)))

  (advice-add 'eldoc-mode :around #'me/eldoc-advice)

  (defun me/toggle-eldoc (&optional arg)
    (interactive "P")
    (setq me/eldoc-enabled-p (if arg nil (not me/eldoc-enabled-p))))

  ;; Automatically update buffers when contents change on disk.
  (global-auto-revert-mode)
  ;; Always highlight the current line.
  (global-hl-line-mode)
  ;; Don't ask to spell out 'yes'
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; utf-8
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)

  ;; ibuffer is a better version of list-buffers.
  ;; http://xahlee.info/emacs/emacs/emacs_buffer_management.html
  (defalias 'list-buffers 'ibuffer)

  ;; Prevent the scratch buffer from being killed.
  (with-current-buffer "*scratch*"
    (emacs-lock-mode 'kill))

  ;; Don't error if we can't load the custom file
  (load custom-file 'noerror)
  (setq-default indent-tabs-mode nil
                tab-width 4))

(use-package async :defer)

;; Faster scrolling for emacs
(me/emacs-N-progn
 29
 (use-package ultra-scroll
   :ensure nil
   :init
   (unless (package-installed-p (intern "ultra-scroll"))
     (package-vc-install
      '(ultra-scroll :vc-backend Git
                     :url "https://github.com/jdtsmith/ultra-scroll")))

   (setq scroll-conservatively 101
         scroll-margin 0)
   :config
   (ultra-scroll-mode 1)))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode))

(use-package ctrlf
  :config
  (ctrlf-mode)
  :custom
  (ctrlf-default-search-style 'regexp)
  (ctrlf-alternate-search-style 'fuzzy-regexp))

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

;; https://macowners.club/posts/from-ivy-to-vertico/
;; https://github.com/minad/consult
;; NOTE: need to investigate this package further, there is definitely more
;; to configure here...
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind
  (("C-x b" . consult-buffer)
   ("M-g i" . consult-imenu)))

;; https://github.com/oantolin/embark
(use-package embark
  :init

  ;; Prefer a more minimal setup w/ using only the minibuffer & vertico.
  (setq embark-indicators
        '(embark-minimal-indicator  ; default is embark-mixed-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))
  (setq embark-prompter #'embark-completing-read-prompter)

  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-;" . embark-act)
   ("C-:" . embark-dwim))
  (:map embark-general-map
        ("Dw" . define-word-at-point)
        ("Cp" . kill-ring-save)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package which-key
  :config
  (which-key-mode)
  (when (eq me/modal-system 'god)
    (which-key-enable-god-mode-support)))

(use-package helpful
  :defer
  :commands (helpful-callable helpful-variable helpful-key helpful-at-point)
  :init
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h C-d") #'helpful-at-point))

;; TODO: experiment with not using vim, and trying out Vanilla emacs binds...

(pcase me/modal-system
  ('evil
   (progn
     (use-package evil
       :config
       (evil-mode 1)
       :custom
       (evil-want-integration t)
       (evil-want-keybinding nil)
       (evil-want-C-u-scroll t)
       (evil-want-minibuffer t)
       (evil-undo-system 'undo-redo)
       (evil-shift-round nil)
       (evil-indent-convert-tabs nil))

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
       (evil-lion-mode))))

  ('viper
   (use-package viper
     :ensure nil
     :init
     (setq viper-inhibit-startup-message t
           viper-expert-level 3
           viper-mode t)
     :config
     (viper-mode)))

  ('god
   (progn
     (use-package god-mode
       :init
       (global-set-key (kbd "<escape>") #'god-local-mode)
       :bind
       (:map god-local-mode-map
             ("i" . god-local-mode))))))

;; TODO: add a keybind to bring up vundo menu.
(use-package vundo
  :defer
  :commands (vundo))

(use-package beframe
  :defer
  :if (not (getenv "EMACS_IS_EXWM"))
  :custom
  (beframe-global-buffers '("*scratch*" "*Messages*" "*Backtrace*"))
  ;; :bind
  ;; (("C-x b" . beframe-switch-buffer))
  :init
  (beframe-mode 1)
  :config
  ;; Consult integration.
  ;; https://protesilaos.com/emacs/beframe#h:1c2d3d64-aa7b-4585-a418-ccedbb548b38
  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defun me/beframe-buffer-names-sorted (&optional frame)
      "Return the list of buffers from `beframe-buffer-names' sorted by
visibility.
With optional argument FRAME, return the list of buffers of FRAME."
      (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

    (defvar beframe-consult-source
      `(
        :name     "Frame-specific buffers (current frame)"
        :narrow   ?F
        :category buffer
        :face     beframe-buffer
        :history  beframe-history
        :items    ,#'me/beframe-buffer-names-sorted
        :action   ,#'switch-to-buffer
        :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe-consult-source))
  )

;; (use-package doom-themes :defer)
(use-package all-the-icons :defer)

(use-package all-the-icons-dired
  :defer
  :hook (dired-mode . all-the-icons-dired-mode))

;; TODO: Break this out into a package.
;; TODO: also make a macro to help simplify the creation of
;; the functions - it should suffice to simply provide the quoted
;; code and to have that automagically be inserted into an anonymous function.
;; Semi-Configurable modeline.
(defmacro me/modeline-sexps-to-str (lst-of-sexps)
  `(let* ((lst ,lst-of-sexps)
          (fns (mapcar (lambda (expr) `(lambda () (propertize (eval ,expr)))) lst))
          (res ""))
     (dotimes (i (length lst))
       (setq res (concat res (when (>= i 1) " ") (funcall (nth i fns)))))
     res))

(setq me/modeline-left-side-sexps
      '((pcase me/modal-system
          ('god
           (if (bound-and-true-p god-local-mode) " ✝️" " 🍦"))
          ('evil
           (if (and (boundp 'evil-mode) evil-mode) (symbol-name evil-state) ""))
          (_ ""))
        ;; Need to do a check for the *-ts-mode modes.
        (let ((icon  (all-the-icons-icon-for-mode major-mode :height 1.0 :v-adjust -0.1)))
          (if (not (eq major-mode icon))
              icon
            (if buffer-file-name
                (all-the-icons-icon-for-file buffer-file-name :height 1.0 :v-adjust -0.1)
              "")))
        (replace-regexp-in-string "-mode$" "" (format "%s" major-mode))

        ;; TODO: incorporate (vc-state buffer-file-name (vc-backend buffer-file-name))
        (if (and vc-mode buffer-file-name)
            (concat "git:" (propertize (substring vc-mode (+ (if (eq (vc-backend buffer-file-name) 'Hg) 2 3) 2))))
          "")
        (propertize "%l:%c" 'face 'bold)
        "- "))

(setq me/modeline-right-side-sexps
      '())

;;mode-line-client
;;mode-line-right-align-edge
(defun me/modeline ()
  "Create a modeline."
  (interactive)
  (setq-default
   mode-line-format
   '("%e" (:eval (me/modeline-sexps-to-str me/modeline-left-side-sexps))
     mode-line-buffer-identification
     mode-line-misc-info
     mode-line-format-right-align
     "%e" (:eval (me/modeline-sexps-to-str me/modeline-right-side-sexps)))))
(me/modeline)

(use-package rainbow-mode
  :defer
  :hook (prog-mode . rainbow-mode))
(use-package rainbow-delimiters
  :defer
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package hl-prog-extra
  :defer
  :hook ((prog-mode . hl-prog-extra-mode))
  :commands (hl-prog-extra-mode))

;; Git frontend.
(use-package magit :defer)
(use-package forge :defer)


;;(make-variable-buffer-local
;;(setq me/apheleia-preferred-backend (me/get-formatter-backend))


(defmacro me/add-to-eglot-server-programs (modes-lsp-cmd)
  "Add modes in MODES-LSP-CMD to eglot-server-programs if the LSP-CMD exists."
  `(dolist (modes-cmd ,modes-lsp-cmd)
     (let ((modes (car   modes-cmd))
           (cmd   (nth 1 modes-cmd)))
       (if (executable-find cmd)
           (add-to-list 'eglot-server-programs
                        `(,modes . (,cmd)))))))

(defun me/eglot-enable-everything ()
  "Little function to enable eglot in every mode that it knows about."
  (mapcar
   (lambda (mode-str)
     (add-hook
      (intern (seq-concatenate 'string mode-str "-hook")) #'eglot-ensure))
   (seq-filter
    (lambda (str) (string-match-p ".*-mode$" str))
    (mapcar (lambda (sym) (if (symbolp sym) (symbol-name sym) sym))
            (flatten-list (mapcar #'car eglot-server-programs))))))

(setq me/eglot-quickload-file (concat user-emacs-directory "eglot-quickload.el"))

(defun me/make-eglot-quickload-file ()
  (require 'eglot)
  (with-temp-file me/eglot-quickload-file
    (prin1
     `(setq eglot-server-programs ',eglot-server-programs)
     (current-buffer))))

;; Simple LSP mode for emacs.
(use-package eglot
  :defer
  :custom
  (eglot-events-buffer-size 0)
  :init
  ;; Refresh the file every two weeks.
  (when (or
         (not (file-exists-p me/eglot-quickload-file))
         (<= 14
             (- (time-to-days (current-time))
                (time-to-days (nth 5 (file-attributes me/eglot-quickload-file))))))
    (me/make-eglot-quickload-file))
  (load-file me/eglot-quickload-file)
  (me/eglot-enable-everything)
  :config
  ;; Don't log *anything*. If there are problems, it's easy enough to comment
  ;; this line out.
  (fset #'jsonrpc--log-event #'ignore)
  (me/add-to-eglot-server-programs
   '((crystal-mode  "crystalline")
     (d-mode        "serve-d")
     (elixir-mode   "elixir-ls")
     ;; TODO: integrate julia into this
     ;; https://github.com/julia-vscode/LanguageServer.jl
     ((latex-mode
       tex-mode
       context-mode
       texinfo-mode
       bibtex-mode) "texlab")
     (vala-mode     "vala-language-server"))))

(use-package consult-eglot :after eglot)

;; I almost never want this to popup
;; (use-package eldoc-box
;;   :defer
;;   :hook ((eldoc-mode         . eldoc-box-hover-mode)))

;; NOTE: Can't defer this package because it won't add in the custom formatters.


(use-package apheleia
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
    (setq formatter
          (if (not (eq nil formatter)) formatter (me/get-formatter-backend)))
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
          (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
          (julia      . ("https://github.com/tree-sitter/tree-sitter-julia"))
          (latex      . ("https://github.com/latex-lsp/tree-sitter-latex"))
          (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
          (make       . ("https://github.com/alemuller/tree-sitter-make"))
          (php        . ("https://github.com/tree-sitter/tree-sitter-php"))
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

;; FIXME: see if i can replace this package by just using
;; the auto mode remap list

;; FIXME: I would like to write some Emacs lisp code to automatically move
;; over my hooks from non-tree-sitter modes to the tree-sitter modes so that
;; way there is little headache when it comes to actually using this
;; https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :if (>= emacs-major-version 29)
  ;; :defer
  :config
  (global-treesit-auto-mode))

;; In-buffer auto-completion.
(use-package corfu
  :custom
  (corfu-auto t)  ;; Enable auto-completion.
  (corfu-cycle t) ;; Enable cycling.

  (corfu-auto-prefix 2)  ;; Set the minimum prefix for completion.
  (corfu-auto-delay 0.0) ;; Disable delay for completions.
  (corfu-quit-no-match    'separator)
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

;; Some extra in-buffer autocompletion - add ability to complete words
;; as well as files.
;; TODO: add misc company backends as a capf too.
;; TODO: need to add a check/wrapper for cape-dict, since it is only
;; useful in some circumstances, like writing prose in LaTeX mode or
;; markdown mode - it'd be interesting to have it be dynamically enabled
;; in programming modes while I am writing comments as well.
(use-package cape
  :hook ((LaTeX-mode . (lambda () (add-to-list 'completion-at-point-functions #'cape-tex))))
  :init
  (let* ((dict-file (concat user-emacs-directory "cape-dict"))
         ;; FIXME: make this work cross platform...
         (hunspell-dict "/usr/share/hunspell/en_US.dic")
         (hunspell-exists-p  (file-exists-p hunspell-dict))
         (dict-file-exists-p (file-exists-p dict-file)))
    (unless (or hunspell-exists-p dict-file-exists-p)
      (unless dict-file-exists-p
        ;; Generate the dictionary file.
        (shell-command
         (concat
          "sed 's;/.*$;;'"
          " " hunspell-dict " "
          " | "
          "grep -v '^[0-9]'"
          " | "
          "grep -E '....'"
          " > "
          dict-file)))
      (setq cape-dict-file dict-file)))

  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-emoji))


;; Nice icons for corfu completions, depends on librsvg.
(use-package kind-icon
  :if (and (display-graphic-p) (image-type-available-p 'svg))
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; Collaborative editing in Emacs.
(use-package crdt :defer)

(use-package jinx
  :hook ((emacs-startup . global-jinx-mode)
         (prog-mode     . (lambda () (jinx-mode -1))))
  :bind ([remap ispell-word] . jinx-correct))

(use-package flymake
  :defer
  :hook ((text-mode . flymake-mode))
  :config
  (require 'flymake-proc))

;; https://github.com/purcell/flymake-flycheck
(use-package flymake-flycheck
  :defer
  :after flymake)

;; https://github.com/emacs-languagetool/flymake-languagetool
;; https://github.com/emacs-languagetool
;; https://valentjn.github.io/ltex/index.html
;; https://old.reddit.com/r/emacs/comments/1b1s7wk/grammarly_in_emacs/

(me/emacs-N-progn
 30
 (defun me/make-vale-config ()
   "Write a simple vale config to `.vale.ini' in `default-directory'."
   (interactive)
   (let ((vale-cfg (concat default-directory ".vale.ini")))
     (write-region
      "StylesPath = styles

MinAlertLevel = suggestion

Packages = proselint, write-good, alex, Readability, Joblint

[*]
BasedOnStyles = Vale, proselint, write-good, alex, Readability, Joblint"
      nil
      vale-cfg))
   (shell-command "vale sync")
   (when flymake-mode (flymake-vale-maybe-load)))

 (use-package flymake-vale
   :if (executable-find "vale")
   :defer
   :vc (:url "https://github.com/tpeacock19/flymake-vale"
             :rev :newest)
   :hook (flymake-mode . flymake-vale-load)))

(use-package symbol-overlay
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
  ((emacs-lisp-mode       . enable-paredit-mode)
   (geiser-mode           . enable-paredit-mode)
   (racket-mode           . enable-paredit-mode)
   (racket-hash-lang-mode . enable-paredit-mode)
   (lisp-mode             . enable-paredit-mode)))

(use-package tuareg
  :defer
  :mode ("\\.ml[iylp]?$" . tuareg-mode)
  :init
  ;; Ensure that `ocamllsp` is in Emacs' $PATH.
  (let ((opam-bindir (concat (getenv "HOME") "/.opam/default/bin/")))
    (when (file-exists-p opam-bindir)
      (add-to-list 'exec-path opam-bindir)))
  :custom
  (tuareg-indent-align-with-first-arg t)
  (tuareg-match-patterns-aligned t))

(use-package utop
  :after tuareg
  :hook ((tuareg-mode . utop-minor-mode))
  :custom
  (utop-command "opam exec -- dune utop . -- -emacs"))


;; TODO: consider rewriting this macro as a use-package extension

;; it should be possible to have this be a keyword, similar to :defer,
;; which, will actually install the package either when the file extension
;; given by :mode is encountered, or when a certain mode is requested.


;; IE: only install/require the code whenever I begin to edit a file
;; with the associated file extension.
(defmacro me/setup-auto-mode (file-extension major-mode &rest args)
  "FIXME: write this docstring"
  ;; NOTE: only use this for modes that you don't plan on configuring at all.

  ;; Default values for the use-package decl.
  (let ((pkg  major-mode)
        (mm   major-mode)
        (fe   file-extension)
        (body args))
    ;; Means we have args that we need to parse.
    (when args
      (if (eq (nth 0 args) :package)
          (progn
            (setq pkg  (nth 1 args))
            (setq body (seq-drop args 2)))))
    (let ((setup-fn-name
           (intern
            (concat
             "setup-"
             (symbol-name mm)
             "-"
             (symbol-name (gensym))))))
      `(progn
         (defun ,setup-fn-name ()
           "AUTOGENERATED from `me/setup-auto-mode` macro."
           (unless (fboundp ',mm)
             (use-package ,pkg)
             ,@body)
           (,mm))
         (add-to-list
          'auto-mode-alist
          '(,fe . ,setup-fn-name))))
    )
  )

(me/setup-auto-mode "\\.bqn"   bqn-mode)
(me/setup-auto-mode "\\.cr"    crystal-mode)
(me/setup-auto-mode "\\.d"     d-mode)
(me/setup-auto-mode "\\.fs"    fsharp-mode)
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
(me/setup-auto-mode
 "\\.clj" clojure-mode (use-package inf-clojure :after clojure-mode))
(me/setup-auto-mode
 "\\.exs?" elixir-mode (use-package inf-elixir :after elixir-mode))
(me/setup-auto-mode "\\.lfe" lfe-mode)
(me/setup-auto-mode
 "\\.sml" sml-mode
 (progn
   (setq sml-program-name "hamlet")
   (use-package sml-basis :after sml-mode)))
(me/setup-auto-mode "\\.lua" lua-mode)

(me/setup-auto-mode "\\.4th" forth-mode (setq forth-executable "gforth"))
(me/setup-auto-mode "\\.idr" idris-mode (setq idris-interpreter-path "idris2"))

(me/setup-auto-mode
 "\\.shen" shen-mode (setq inferior-shen-program "shen-sbcl"))

(me/setup-auto-mode
 "\\.ij[rstp]$" j-mode (setq j-console-cmd "/usr/lib/j9/bin/jconsole"))

;; .rb
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
;; .rkt
;; https://www.racket-mode.com/
(me/setup-auto-mode
 "\\.rkt"
 racket-hash-lang-mode
 :package racket-mode
 (progn
   (add-hook 'racket-hash-lang-mode-hook #'racket-xp-mode)
   (add-hook 'racket-hash-lang-mode-hook
             #'(lambda ()
                 (when (boundp #'agda2-mode)
                   (require 'agda2-mode)
                   (set-input-method "Agda")))) ))

(me/setup-auto-mode
 "\\.jl$"
 ess-julia-mode
 :package ess
 (progn
   (use-package julia-mode :defer)))

;; https://eshelyaron.com/sweep.html
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
  :config
  ;; Custom code to allow for switching between prolog implementations.
  (let* ((custom-prologs '((trealla "tpl")
                           (scryer "scryer-prolog")))
         (new-prolog-program-name
          (seq-remove
           (lambda (p)
             (or (equal (car p) t)
                 (equal (cadr p) nil)))
           (seq-filter (lambda (p) (symbolp (car p)))
                       (append prolog-program-name custom-prologs)))))
    (dolist (prolog-pair new-prolog-program-name)
      (if (executable-find (cadr prolog-pair))
          (let* ((prolog-sys (car prolog-pair))
                 (prolog-exe (cadr prolog-pair))
                 (prolog-sym-str (symbol-name prolog-sys))
                 (func-name
                  (intern
                   (seq-concatenate
                    'string
                    "run-" prolog-sym-str "-prolog"))))
            (eval
             `(defun ,func-name ()
                (interactive)
                (let ((prolog-system ',prolog-sys)
                      (prolog-program-name ,prolog-exe))
                  (run-prolog t)))))))))

;; Custom snobol mode.
(use-package snobol-mode
  :ensure nil
  :init
  (unless (package-installed-p (intern "snobol-mode"))
    (package-vc-install "https://github.com/echawk/snobol-mode"
                        :last-release))
  :mode ("\\.sno" . snobol-mode))

;; Better scheme editing.
;; (use-package geiser :defer
;;   :defer
;;   :custom
;;   (geiser-active-implementations '(guile3 racket)))
;; (use-package geiser-guile
;;   :after geiser
;;   :custom (geiser-guile-binary "guile3"))

;; FIXME: rm these keybinds
(use-package macrostep :defer
  :bind
  (:map macrostep-keymap
        ("C-c C-e" . macrostep-expand)
        ("C-c C-u" . macrostep-collapse)
        ("C-c C-q" . macrostep-collapse-all)))
(use-package macrostep-geiser
  :after geiser-mode
  :hook ((geiser-mode . macrostep-geiser-setup)))
;; https://scripter.co/emacs-lisp-advice-combinators/
(use-package srfi :defer
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
        ("C-c C-s"    . srfi-search)))

;; Use sly instead of slime
(use-package elisp-slime-nav :after sly)
(use-package sly :defer
  :bind
  (:map sly-mode-map
        ("C-c C-i" . sly-interrupt)
        ("C-c C-b" . sly-eval-buffer))
  :init
  ;; This little bit of magic code will make a custom sly-<lisp> command
  ;; for each of the available lisps.
  (dolist (inf-lisp '(sbcl ecl ccl clasp clisp abcl))
    (if (executable-find (format "%s" inf-lisp))
        (let* ((inf-lisp-str (symbol-name inf-lisp))
               (func-name
                (intern
                 (seq-concatenate 'string (symbol-name 'sly-) inf-lisp-str)))
               (doc-str
                (seq-concatenate
                 'string "Start " inf-lisp-str " and connect to it.")))
          (eval
           `(defun ,func-name ()
              ,doc-str
              (interactive)
              (let ((inferior-lisp-program ,inf-lisp-str))
                (sly)))))))
  :custom
  (inferior-lisp-program "sbcl"))
(use-package sly-macrostep :after sly)

(use-package agda
  :ensure nil
  :defer
  :if (executable-find "agda-mode")
  :commands (agda2-mode)
  :init
  (load-file (shell-command-to-string "agda-mode locate")))
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


;; (setq completion-at-point-functions
;;       (append
;;        completion-at-point-functions
;;        (mapcar #'cape-company-to-capf
;;                (list #'dante-company))))

;; Enable prettify-symbols-mode in some languages
(dolist (hook '(sly-mode-hook
                emacs-lisp-mode-hook
                geiser-mode-hook
                clojure-mode-hook)
              nil)
  (add-hook hook #'prettify-symbols-mode))

;; https://github.com/phantomics/april/tree/master
;; anaphora is a dependency of jpt-apl-mode.
(me/emacs-N-progn
 30
 (use-package anaphora :defer)
 (use-package jpt-apl-mode
   :vc (:url "https://github.com/jthing/apl-mode"
             :rev :newest)
   :defer))

(use-package gnu-apl-mode
  :defer
  :hook ((gnu-apl-mode             . (lambda () (set-input-method "APL-Z")))
         (gnu-apl-interactive-mode . (lambda () (set-input-method "APL-Z")))))

;; Refactoring mode:
;; https://github.com/Wilfred/emacs-refactor

(use-package emr :defer)
(me/emacs-N-progn
 30
 (use-package etrace
   :vc (:url "https://github.com/aspiers/etrace"
             :rev :newest)
   :defer))

;; Emacs lisp editing stuff.
;; https://github.com/emacs-elsa/Elsa
;; https://github.com/p3r7/awesome-elisp
;; https://github.com/gonewest818/elisp-lint
;; https://alphapapa.github.io/emacs-package-dev-handbook/
;; https://www.masteringemacs.org/article/understanding-minibuffer-completion
;; (use-package elisp-demap :defer)
(use-package suggest :defer)
(use-package package-lint :defer)


;; Speed reading in Emacs.
(me/emacs-N-progn
 30
 (use-package spray
   :defer
   :vc (:url "https://github.com/emacsmirror/spray"
             :rev :newest)
   :commands (spray-mode)))

(use-package org
  :defer
  :hook (org-mode . jinx-mode)
  :custom
  ;;(org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-confirm-babel-evaluate nil)
  :config
  ;; Add in shortcuts for code blocks and whatnot.
  (require 'org-tempo)

  ;; Will automatically load a language if it is needed when in org-mode.
  (advice-add
   #'org-babel-execute-src-block
   :around
   (lambda (orig &rest args)
     "Load a language if needed."
     (let ((language (org-element-property :language (org-element-at-point))))
       (unless (cdr (assoc (intern language) org-babel-load-languages))
         (add-to-list 'org-babel-load-languages
                      (cons (intern language) t))
         (org-babel-do-load-languages 'org-babel-load-languages
                                      org-babel-load-languages)))
     (apply orig args))))

;; Fix weird tab behavior in default org-mode. Preferable to "C-c '".
(use-package poly-org
  :after org)

;; Speech-to-text in Emacs.
(me/emacs-N-progn
 30
 (use-package whisper
   :defer
   :vc (:url "https://github.com/natrys/whisper.el"
             :rev :newest)
   :bind ("C-c w" . whisper-run)
   :config
   (setq
    whisper-install-directory (concat user-emacs-directory "whisper-el/")
    whisper-model "base"
    whisper-language "en"
    whisper-translate nil
    whisper-recording-timeout 600
    whisper--ffmpeg-input-format "alsa"
    whisper--ffmpeg-input-device "hw:5,0")))

;; Read EPUBs in Emacs!
(use-package nov
  :defer
  :mode ("\\.epub\\'" . nov-mode)
  :hook (nov-mode . visual-line-mode))

(use-package vterm
  :defer
  :hook (vterm-mode . (lambda () (display-line-numbers-mode 0))))

(use-package eat
  :after eshell
  :config
  (eat-eshell-mode))

(use-package pdf-tools
  :defer
  :hook ((pdf-view-mode . (lambda () (display-line-numbers-mode 0)))
         (pdf-view-mode . (lambda () (if (me/is-night-p) (pdf-view-midnight-minor-mode)))))
  :init
  (pdf-loader-install :no-query))


(use-package eradio
  :defer
  :custom
  (eradio-player '("mpv" "--no-video" "--no-terminal"))
  (eradio-channels '(("def con - soma fm"      . "https://somafm.com/defcon256.pls")
                     ("the trip - soma fm"     . "https://somafm.com/thetrip.pls")
                     ("dubstep - soma fm"      . "https://somafm.com/dubstep.pls")
                     ("doomed - soma fm"       . "https://somafm.com/doomed.pls")
                     ("darkzone - soma fm"     . "https://somafm.com/darkzone.pls")
                     ("groove salad - soma fm" . "https://somafm.com/groovesalad.pls")
                     ("bossa - soma fm"        . "https://somafm.com/bossa.pls")
                     ("isl - soma fm "         . "https://somafm.com/illstreet.pls"))))

;; NOTE: They keys get bound, but they are captured by evil-mode.
(use-package yeetube
  :defer
  :config
  (defun me/open-yt-under-point ()
    (interactive)
    (let ((url (thing-at-point 'url)))
      (if (string-match "youtube.com" url)
          (yeetube-search url))))

  ;; Simple bit of advice to allow for youtube links to be automatically
  ;; searched for via yeetube.
  (advice-add
   #'browse-url
   :around
   (lambda (orig &rest args)
     (if (string-match "youtube.com" (car args))
         (yeetube-search (car args))
       (apply orig args))))

  :bind
  (:map yeetube-mode-map
        ("RET"     . yeetube-play)
        ("d"       . yeetube-download-video)
        ("/"       . yeetube-search)))

(use-package media-thumbnail
  :defer
  :hook (dired-mode . media-thumbnail-dired-mode))

(use-package eww
  :defer
  ;; Uncomment below for emacs to behave as the default web browser.
  ;; :init
  ;; (setq browse-url-browser-function 'eww-browse-url)
  :hook (eww-mode . visual-line-mode))

(use-package elpher
  :defer
  :hook (elpher-mode . visual-line-mode))

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


;; Better LaTeX editing.
(use-package auctex
  :defer
  :hook ((LaTeX-mode . visual-line-mode)
         (LaTeX-mode . LaTeX-math-mode)
         (LaTeX-mode . jinx-mode)
         (LaTeX-mode . auctex-cluttex-mode))
  :init
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :custom
  ;; Make pdf-tools the default viewer for auctex.
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))

  (TeX-source-correlate-mode t)
  (TeX-source-correlate-start-server t)

  (TeX-engine 'xetex)
  (TeX-auto-save nil)
  (TeX-parse-self t)
  :config
  (setq-default TeX-master nil))
(use-package auctex-cluttex
  :after auctex)

;; Markdown support.
(use-package markdown-mode
  :defer
  :hook (markdown-mode . jinx-mode)
  :custom
  (markdown-command "lowdown -s -Thtml"))
(use-package markdown-preview-mode :defer)

(use-package rmsbolt :defer)

(use-package realgud :defer)

;; http://yummymelon.com/devnull/announcing-casual-an-opinionated-porcelain-for-emacs-calc.html
;; https://legends2k.github.io/note/emacs_calc/
;; (use-package casual-calc
;;   :after calc
;;   :config
;;   (define-key calc-mode-map (kbd "C-c o") #'casual-calc-tmenu))

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
  :if (and (getenv "EMACS_IS_EXWM")
           (or (eq system-type 'gnu/linux)
               (eq system-type 'berkeley-unix)))
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
               (number-sequence 0 9))))
  )

(defun me/mu4e-have-dependencies ()
  "Return t if all dependencies for mu4e are installed, nil otherwise."
  (and
   (executable-find "msmtp")
   (executable-find "mbsync")))

(use-package mu4e
  :defer
  :ensure nil
  :if (me/mu4e-have-dependencies)
  :config

  ;; Allow selecting files with dired.
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Dired.html
  ;; Keybind is 'C-c RET C-a' once files are marked.
  (add-hook 'dired-mode-hook #'turn-on-gnus-dired-mode)

  (setq mu4e-setup-use-msmtp-p t)
  (setq mu4e-setup-mbsync-use-master-slave-p t)
  (load-file (concat user-emacs-directory "mu4e-setup.el"))

  (setq mu4e-setup-email-profiles-list
        (list

         (mu4e-setup-email-profile
          :email-address "ethan.hawk@valpo.edu"
          :imap-address "imap.gmail.com"
          :imap-port "993"
          :smtp-address "smtp.gmail.com"
          :smtp-port "587"
          :password-command "cat /home/ethan/.config/lsps/gmail")

         (mu4e-setup-email-profile
          :email-address "masterdragoon17@hotmail.com"
          :imap-address "outlook.office365.com"
          :imap-port    "993"
          :smtp-address "smtp-mail.outlook.com"
          :smtp-port "587"
          :password-command "cat /home/ethan/.config/lsps/masterdragoon17")))

  (mu4e-setup-configure)

  (setq mail-user-agent               'mu4e-user-agent
        user-full-name                "Ethan Hawk"
        mu4e-compose-context-policy   'ask-if-none
        mu4e-context-policy           'pick-first
        mu4e-update-interval          (* 3 60)
        mu4e-completing-read-function #'completing-read
        mu4e-read-option-use-builtin  nil)

  :commands (mu4e))
