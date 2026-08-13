;;; ui-setup --- packages for ui changes in emacs.

(use-package page-break-lines
  :config
  (global-page-break-lines-mode))

(use-package which-key
  :config
  (which-key-mode)
  (when (eq me/modal-system 'god)
    (which-key-enable-god-mode-support)))

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

    (add-to-list 'consult-buffer-sources 'beframe-consult-source)))

(use-package all-the-icons
  :defer
  :hook (dired-mode . (lambda ()
                        (use-package all-the-icons-dired :ensure t)
                        (all-the-icons-dired-mode))))

;; (use-package sidebuf :defer)

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

;;; --- begin modeline section ---

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

;;; --- end modeline section ---

(use-package sideline
  :defer
  :hook ((prog-mode . sideline-mode))
  :custom
  (sideline-backends-right '(sideline-flymake)))

(use-package sideline-flymake
  :defer
  :after sideline
  :custom
  (sideline-flymake-display-mode 'line)
  (sideline-flymake-max-lines 1))


;; The kirigami package offers a unified interface for text folding across a
;; diverse set of major and minor modes in Emacs, including `outline-mode',
;; `outline-minor-mode', `outline-indent-mode', `org-mode', `markdown-mode',
;; `vdiff-mode', `vdiff-3way-mode', `hs-minor-mode', `hide-ifdef-mode',
;; `origami-mode', `yafolding-mode', `folding-mode', and `treesit-fold-mode'.

;; With Kirigami, folding key bindings only need to be configured once. After
;; that, the same keys work consistently across all supported major and minor
;; modes, providing a unified and predictable folding experience. The available
;; commands include:

;; - `kirigami-open-fold': Open the fold at point.
;; - `kirigami-open-fold-rec': Open the fold at point recursively.
;; - `kirigami-close-fold': Close the fold at point.
;; - `kirigami-open-folds': Open all folds in the buffer.
;; - `kirigami-close-folds': Close all folds in the buffer.
;; - `kirigami-toggle-fold': Toggle the fold at point.

;; (In addition to unified interface, the kirigami package enhances folding
;; behavior in outline-mode, outline-minor-mode, markdown-mode, and
;; org-mode. It ensures that deep folds open reliably and allows folds to be
;; closed even when the cursor is positioned inside the content.)


;; Installation from MELPA
;; -----------------------
;; (use-package kirigami
;;   :ensure t)

(use-package media-thumbnail
  :defer
  :hook (dired-mode . media-thumbnail-dired-mode))

(use-package electric-list-directory
  :defer
  :bind ("C-x C-d" . electric-list-directory))


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


;; corfu-candidate-overlay        20240322.1814  available    melpa    Show first candidate in an overlay while typing
;; corfu-prescient                20250816.19    available    melpa    Prescient.el + Corfu
;; corfu-terminal                 0.7            available    nongnu   Corfu popup on terminal

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


(use-package flymake
  :defer
  :hook ((text-mode . flymake-mode))
  :config
  (require 'flymake-proc))

(use-package flymake-flycheck
  :defer
  :after flymake)

(me/eval-form-on-first-command-run
 vundo
 (use-package vundo))

(use-package helpful
  :defer
  :commands (helpful-callable helpful-variable helpful-key helpful-at-point)
  :init
  (dolist (bind.func `(("C-h f"   . ,#'helpful-callable)
                       ("C-h v"   . ,#'helpful-variable)
                       ("C-h k"   . ,#'helpful-key)
                       ("C-h C-d" . ,#'helpful-at-point)))
    (pcase bind.func
      (`(,bind . ,func) (global-set-key (kbd bind) func)))))
