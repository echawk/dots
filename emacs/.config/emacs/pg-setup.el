(me/eval-form-on-first-command-run
 vterm
 (use-package vterm
   :hook (vterm-mode . (lambda () (display-line-numbers-mode 0)))))

(me/setup-auto-mode
 "\\.pdf"
 pdf-view-mode
 :package pdf-tools
 (pdf-loader-install :no-query)
 (add-hook 'pdf-view-mode-hook #'(lambda () (display-line-numbers-mode 0)))
 (add-hook 'pdf-view-mode-hook #'(lambda () (when (me/is-night-p) (pdf-view-midnight-minor-mode)))))

(me/setup-auto-mode
 "\\.epub"
 nov-mode
 :package nov
 (add-hook 'nov-mode-hook #'visual-line-mode))

(me/eval-form-on-first-command-run
 eradio-play
 (use-package eradio
   :custom
   (eradio-player '("mpv" "--no-video" "--no-terminal"))
   (eradio-channels '(("def con - soma fm"      . "https://somafm.com/defcon256.pls")
                      ("the trip - soma fm"     . "https://somafm.com/thetrip.pls")
                      ("dubstep - soma fm"      . "https://somafm.com/dubstep.pls")
                      ("doomed - soma fm"       . "https://somafm.com/doomed.pls")
                      ("darkzone - soma fm"     . "https://somafm.com/darkzone.pls")
                      ("groove salad - soma fm" . "https://somafm.com/groovesalad.pls")
                      ("bossa - soma fm"        . "https://somafm.com/bossa.pls")
                      ("isl - soma fm "         . "https://somafm.com/illstreet.pls")))))


(me/eval-form-on-first-command-run
  spray-mode
  (use-package spray
    :vc (:url "https://github.com/emacsmirror/spray"
              :rev :newest)
    :commands (spray-mode)))


;; it should be possible to have this be a keyword, similar to :defer,
;; which, will actually install the package either when the file extension
;; given by :mode is encountered, or when a certain mode is requested.
;; Git frontend.

(progn
  (use-package magit
    :hook (magit-mode . (lambda ()
                          (use-package gptel-magit :after gptel)
                          (use-package magit-prime
                            :config
                            (magit-prime-mode)))))
  (use-package forge)
  (use-package magit-filenotify
    :hook
    (magit-status-mode . magit-filenotify-mode))
  (use-package magit-gh)
  (use-package magit-gh-pulls))

;;(use-package ement :defer)

(me/eval-form-on-first-command-run
 elpher
 (use-package elpher
   :hook (elpher-mode . visual-line-mode)))


;; NOTE: They keys get bound, but they are captured by evil-mode.
;; (use-package yeetube
;;   :defer
;;   :config
;;   (defun me/open-yt-under-point ()
;;     (interactive)
;;     (let ((url (thing-at-point 'url)))
;;       (if (string-match "youtube.com" url)
;;           (yeetube-search url))))

;;   ;; Simple bit of advice to allow for youtube links to be automatically
;;   ;; searched for via yeetube.
;;   (advice-add
;;    #'browse-url
;;    :around
;;    (lambda (orig &rest args)
;;      (if (string-match "youtube.com" (car args))
;;          (yeetube-search (car args))
;;        (apply orig args))))

;;   :bind
;;   (:map yeetube-mode-map
;;         ("RET"     . yeetube-play)
;;         ("d"       . yeetube-download-video)
;;         ("/"       . yeetube-search)))


;; (me/emacs-N-progn
;;  30
;;  (use-package tsort)
;;  (use-package kiss
;;    :ensure nil
;;    :init
;;    (unless (package-installed-p (intern "kiss"))
;;      (package-vc-install
;;       '(kiss :vc-backend Git
;;              :url "https://github.com/echawk/kiss.el")))))

;; Speech-to-text in Emacs.
;; (me/emacs-N-progn
;;  30
;;  (me/eval-form-on-first-command-run
;;   whisper-run
;;   (use-package whisper
;;     :vc (:url "https://github.com/natrys/whisper.el"
;;               :rev :newest)
;;     :bind ("C-c w" . whisper-run) ;; FIXME: move this out of here?
;;     :config
;;     (setq
;;      whisper-install-directory (concat user-emacs-directory "whisper-el/")
;;      whisper-model "base"
;;      whisper-language "en"
;;      whisper-translate nil
;;      whisper-recording-timeout 600
;;      whisper--ffmpeg-input-format "alsa"
;;      whisper--ffmpeg-input-device "hw:5,0"))))

;; http://yummymelon.com/devnull/announcing-casual-an-opinionated-porcelain-for-emacs-calc.html
;; https://legends2k.github.io/note/emacs_calc/
;; (use-package casual-calc
;;   :after calc
;;   :config
;;   (define-key calc-mode-map (kbd "C-c o") #'casual-calc-tmenu))


(use-package jinx
  :defer
  :hook ((emacs-startup . global-jinx-mode)
         (prog-mode     . (lambda () (jinx-mode -1))))
  :bind ([remap ispell-word] . jinx-correct))

;; Collaborative editing in Emacs.
(me/eval-form-on-first-command-run crdt-version (use-package crdt))
