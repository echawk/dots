;;; packages to enhance the user experience in emacs


;; https://github.com/meedstrom/el-job
;; https://github.com/tarsius/emacs-ffi/

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
  (ultra-scroll-mode 1))

(use-package current-window-only)

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

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function       #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :bind
  (("C-x b" . consult-buffer)
   ("M-g i" . consult-imenu)))

(use-package embark
  :init
  ;; Prefer a more minimal setup w/ using only the minibuffer & vertico.
  (setq embark-indicators
        '(embark-minimal-indicator      ; default is embark-mixed-indicator
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
        ("Ie" . iedit-mode)
        ("Cp" . kill-ring-save)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic partial-completion))
  (completion-category-overrides '((file (styles partial-completion)))))


(use-package iedit)
;;(use-package multiple-cursors :defer)
;;(use-package selected :defer)

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

(me/eval-form-on-first-command-run
 define-word
 (use-package define-word))
(me/eval-form-on-first-command-run
 google-translate-buffer
 (use-package google-translate))


