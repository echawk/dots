(defun me/mu4e-have-dependencies ()
  "Return t if all dependencies for mu4e are installed, nil otherwise."
  (and
   (executable-find "msmtp")
   (executable-find "mbsync")))

;; FIXME: integrate this blogpost into mu4e-setup.el & this config.
;; https://lambdaland.org/posts/2023-05-03_email_with_outlook/

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
          :password-command "cat /home/ethan/.config/lsps/masterdragoon17")

         ;; Potential davmail setup below - if i ever get around to setting
         ;; it up. Maybe over winter break 2024???
         ;; (mu4e-setup-email-profile
         ;;  :email-address "ethhawk@iu.edu"
         ;;  :imap-address "127.0.0.1"
         ;;  :imap-port "1143"
         ;;  :smtp-address "127.0.0.1"
         ;;  :smtp-port "1025"
         ;;  :smtp-type "plain"
         ;;  :password-commadn "cat /home/ethan/.config/lsps/gradoutlook")
         ))

  (mu4e-setup-configure)

  (setq mail-user-agent               'mu4e-user-agent
        user-full-name                "Ethan Hawk"
        mu4e-compose-context-policy   'ask-if-none
        mu4e-context-policy           'pick-first
        mu4e-update-interval          (* 3 60)
        mu4e-completing-read-function #'completing-read
        mu4e-read-option-use-builtin  nil)

  :commands (mu4e))

;;(use-package vm)

