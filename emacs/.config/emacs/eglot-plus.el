
(require 'cl-lib)
(require 'pcase)
(require 'eglot)


;; Enable Eglot everywhere it can be.

(defun eglot-plus-enable-eglot-everywhere ()
  "Little function to enable eglot in every mode that it knows about."
  (interactive)
  (mapcar
   (lambda (mode-str)
     (add-hook
      (intern (seq-concatenate 'string mode-str "-hook")) #'eglot-ensure))
   (seq-filter
    (lambda (str) (string-match-p ".*-mode$" str))
    (mapcar (lambda (sym) (if (symbolp sym) (symbol-name sym) sym))
            (flatten-list (mapcar #'car eglot-server-programs))))))

;; Eglot quickload file/allows for faster init times.

(defvar eglot-plus-quickload-file
  (concat user-emacs-directory "eglot-plus-quickload.el"))

(defun eglot-plus-make-quickload-file ()
  (require 'eglot)
  (with-temp-file eglot-plus-quickload-file
    (prin1
     `(setq eglot-server-programs ',eglot-server-programs)
     (current-buffer))))

(defun eglot-plus-quickload-file-out-of-date-p ()
  (or
   (not (file-exists-p eglot-plus-quickload-file))
   (<= 14
       (- (time-to-days (current-time))
          (time-to-days (nth 5 (file-attributes eglot-plus-quickload-file)))))))


(defun eglot-plus-enable-quickload-file ()
  (interactive)
  (when (eglot-plus-quickload-file-out-of-date-p)
    (eglot-plus-make-quickload-file))
  (load-file eglot-plus-quickload-file))

;; Automatically install eglot lsp servers.

(defvar +eglot-plus-install-lsp-servers+
  `(((python-mode python-ts-mode)
     ,(lambda () (zerop (shell-command "pip show pyrefly")))
     ,(lambda () (shell-command "pip install pyrefly")))
    ((ruby-mode ruby-ts-mode)
     ,(lambda () (zerop (shell-command "gem list -i solargraph")))
     ,(lambda () (shell-command "gem install solargraph")))
    ((rust-mode rust-ts-mode)
     ,(lambda () (executable-find "rust-analyzer"))
     ,(lambda () (shell-command "cargo install ra_ap_rust-analyzer")))
    ((go-mode go-ts-mode)
     ,(lambda () (executable-find "gopls"))
     ,(lambda () (shell-command "go install golang.org/x/tools/gopls@latest")))
    ((LaTeX-mode)
     ,(lambda () (executable-find "texlab"))
     ,(lambda () (shell-command "cargo install texlab")))
    ((haskell-mode)
     ,(lambda ()
        (string-match-p
         "haskell-language-server"
         (shell-command-to-string "ghc-pkg list --simple-output")))
     ,(lambda () (shell-command "cabal install haskell-language-server")))
    ((caml-mode tuareg-mode)
     ,(lambda ()
        (string-match-p
         "ocaml-lsp-server"
         (shell-command-to-string "opam list")))
     ,(lambda () (shell-command "opam install ocaml-lsp-server")))))

(defvar +eglot-plus-install-lsp-servers-modes+
  (flatten-list (mapcar #'car +eglot-plus-install-lsp-servers+)))

;; FIXME: add way for the user to disable the automatic installation of
;; lang servers based off of adding values to a list; or could do the opposite
;; semantics, but I'd rather things be lazy.

(defun eglot-plus-maybe-install-lsp (mode)
  (pcase (cl-find-if
          (lambda (e)
            (memq mode (car e))
            +eglot-plus-install-lsp-servers+))
    (`(,modes-lst ,server-installed-thnk-p ,install-server-thnk)
     (unless (funcall server-installed-thnk-p)
       (funcall install-server-thnk)))))

(defun eglot-plus-enable-auto-lsp-server-installation ()
  (interactive)
  (advice-add
   #'eglot--connect
   :around
   (lambda (orig &rest args)
     (pcase args
       (`(,major-modes ,project ,class-name ,saved-initargs ,language-ids)
        (when (eq 1 (length major-modes))
          (eglot-plus-maybe-install-lsp (car major-modes)))))
     (apply orig args))))

(provide 'eglot-plus)
