;;; Commentary:

;; This file is essentially a re-implementation of mutt-wizard, but for
;; mu4e. There are many missing features when compared to mutt-wizard,
;; such as POP support, but this config is really just to make it easier
;; to up email.

(setq mu4e-setup-maildir      (concat (getenv "HOME") "/.local/share/mail"))
(setq mu4e-setup-dir          (concat user-emacs-directory "mu4e-setup/"))

(setq mu4e-setup-config-file  (concat mu4e-setup-dir "mu4e-config.el"))
(setq mu4e-setup-mbsync-file  (concat mu4e-setup-dir "mbsyncrc"))
(setq mu4e-setup-msmtp-file   (concat mu4e-setup-dir "msmtp-config"))

(setq mu4e-setup-mutt-wizard-repo "https://github.com/LukeSmithxyz/mutt-wizard")

(setq mu4e-setup-mbsync-cmd
      (concat "mbsync -a -c " mu4e-setup-mbsync-file))
(setq mu4e-setup-msmtp-cmd
      (list "msmtp"
            (list "--read-envelope-from"
                  (concat " -C " mu4e-setup-msmtp-file))))

(setq mu4e-setup-default-process-environment
      (append
       process-environment
       (list
        (concat "maildir="  mu4e-setup-maildir)
        (concat "msmtplog=" mu4e-setup-dir "msmtp.log")
        ;; FIXME: allow this to be configured...
        "master=Master"
        "slave=Slave"
        (concat
         "sslcert="
         (car
          (seq-filter
           #'file-exists-p
           '("/etc/ssl/cert.pem"
             "/etc/ssl/certs/ca-certificates.crt"
             "/etc/pki/tls/certs/ca-bundle.crt"
             "/etc/ssl/ca-bundle.pem"
             "/etc/pki/tls/cacert.pem"
             "/etc/pki/ca-trust/extracted/pem/tls-ca-bundle.pem"
             "/usr/local/share/ca-certificates/"))))
        ;; FIXME: have this be detected
        "imapssl=IMAPS"
        "maxmes=0")))

(defun mu4e-setup--append-to-file (str filename)
  (with-temp-buffer
    (insert str)
    (write-region (point-min) (point-max) filename t)))

(defun mu4e-setup-download-mutt-wizard ()
  "Clone mutt-wizard to ~/.config/emacs/mu4e-setup/mutt-wizard."
  (interactive)

  ;; Ensure dir exists.
  (unless (file-exists-p mu4e-setup-dir)
    (make-directory      mu4e-setup-dir))

  ;; Clone mutt-wizard repo (contains useful crowd sourced info)
  (let ((default-directory mu4e-setup-dir))
    (if (not (file-exists-p (concat mu4e-setup-dir "mutt-wizard")))
        (shell-command
         (concat "git clone " mu4e-setup-mutt-wizard-repo))
      (shell-command "cd mutt-wizard; git pull"))))

(require 'eieio)

(defclass mu4e-setup-email-profile ()
  ((email-address
    :initarg :email-address :initform "" :type string
    :documentation "The email address.")
   (imap-address
    :initarg :imap-address :initform "" :type string
    :documentation "The imap address for the email.")
   (imap-port
    :initarg :imap-port :initform "" :type string
    :documentation "The port for the imap address")
   (smtp-address
    :initarg :smtp-address :initform "" :type string
    :documentation "The smtp address for the email.")
   (smtp-port
    :initarg :smtp-port :initform "" :type string
    :documentation "The port for the smtp address.")
   (password-command
    :initarg :password-command :initform "" :type string
    :documentation "A command that when ran returns the account's passowrd.")))

(cl-defmethod mu4e-setup--email-profile-setup ((obj mu4e-setup-email-profile))
  (with-slots
      ((email-address :email-address)
       (imap-address  :imap-address)
       (smtp-address  :smtp-address)
       (imap-port     :imap-port)
       (smtp-port     :smtp-port)

       (password-command :password-command))
      obj

    ;; Assert that everything is filled out in the object.
    (cl-assert
     (not
      (or
       (string-empty-p email-address)
       (string-empty-p imap-address)
       (string-empty-p smtp-address)
       (string-empty-p imap-port)
       (string-empty-p smtp-port)
       (string-empty-p password-command))))

    (let ((process-environment
           (append
            mu4e-setup-default-process-environment
            ;; append this list to process-environment, and execute my commands
            ;; in there.
            (list
             (concat "fulladdr=" email-address)
             (concat "imap="     imap-address)
             (concat "iport="    imap-port)
             (concat "smtp="     smtp-address)
             (concat "sport="    smtp-port)
             ;;FIXME: this *technically* isn't correct
             (concat "login="    email-address)))))

      ;; mbsync config
      (mu4e-setup--append-to-file
       (shell-command-to-string
        (concat
         "sed '/PassCmd/s/.*/PassCmd \"" password-command "\"/'"
         " < " mu4e-setup-dir "/mutt-wizard/share/mbsync-temp"
         " | "
         "envsubst"))
       mu4e-setup-mbsync-file)

      ;; msmtp config
      (mu4e-setup--append-to-file
       (shell-command-to-string
        (concat
         "sed '/passwordeval/s/.*/passwordeval \"" password-command "\"/'"
         " < " mu4e-setup-dir "/mutt-wizard/share/msmtp-temp"
         " | "
         "envsubst"))
       mu4e-setup-msmtp-file)

      ;; Ensure this directory exists before we run mbsync.
      (shell-command (concat "mkdir -p " mu4e-setup-maildir "/" email-address)))))

(cl-defmethod mu4e-setup--email-profile-add-to-mu4e-contexts
  ((obj mu4e-setup-email-profile))
  (with-slots
      ((email-address :email-address)
       (smtp-address  :smtp-address)
       (smtp-port  :smtp-port))
      obj

    (add-to-list
     mu4e-contexts
     (make-mu4e-context
      :name email-address
      :match-func
      (lambda (msg)
        (when msg
          (string-prefix-p (concat "/" email-address)

                           (mu4e-message-field msg :maildir))))

      ;; Now to save the generated context into the contexts file
      ;; FIXME: add check for gmail... since it's "special"
      :vars
      '((user-mail-address     . email-address)

        (smtpmail-smtp-server  . smtp-address)
        (smtpmail-smtp-service . smtp-port)
        (smtpmail-stream-type  . ssl)

        (mu4e-drafts-folder    . (concat "/" email-address "/Drafts"))
        (mu4e-sent-folder      . (concat "/" email-address "/Sent"))
        (mu4e-trash-folder     . (concat "/" email-address "/Trash")))))))


(defun mu4e-setup--configure-email-profiles (email-profiles-list)
  ;; Check for required commands.
  (dolist (cmd '("envsubst" "mu"))
    (unless (executable-find cmd)
      (error
       (concat "\"`" cmd "` needs to be present in $PATH."))))

  (unless (boundp email-profiles-list)
    (error "mu4e-setup-email-profiles isn't bound!"))

  (dolist (email-profile email-profiles-list)
    (mu4e-setup--email-profile-setup email-profile))

  ;; Sync w/ mbsync
  (shell-command mu4e-setup-mbsync-cmd)

  ;; Remove old mu cache.
  (let ((mu-cache-dir (concat (getenv "HOME") "/.cache/mu/")))
    (when (file-exists-p mu-cache-dir)
      (shell-command (concat "rm -rf " mu-cache-dir))))

  ;; Initialize mu.
  (shell-command
   (string-join
    (append
     (list
      "mu" "init"
      (concat "--maildir=" mu4e-setup-maildir))

     ;; Will need to add --my-address for *every* configured email address
     (mapcar
      (lambda (obj)
        (concat "--my-address=" (slot-value obj :email-address)))
      email-profiles-list))
    " ")))

(defun mu4e-setup--setup-config-file (email-profiles-list)
  (shell-command (concat "rm " mu4e-setup-config-file))
  (shell-command (concat "touch " mu4e-setup-config-file))
  (mu4e-setup--append-to-file
   (prin1-to-string
    (prin1
     `(progn
        ;; Default configuration for mu4e here - can be overridden by
        ;; your own config - just set the variables to a different value.
        (setq message-send-mail-function 'message-send-mail-with-sendmail
              message-sendmail-f-is-evil 't
              message-sendmail-extra-arguments ,(cdr mu4e-setup-msmtp-cmd)
              sendmail-program                 ,(car mu4e-setup-msmtp-cmd))

        (setq mu4e-change-filenames-when-moving 't
              mu4e-get-mail-command             ,mu4e-setup-mbsync-cmd
              mu4e-maildir                      ,mu4e-setup-maildir)

        ;; This is important to save (as a different symbol), since we can
        ;; compare the two lists to see if we have any new email profiles to
        ;; setup.
        (setq mu4e-setup-current-email-profiles-list
              ,email-profiles-list)

        ;; Ensure that each of the email profiles is a context in mu4e.
        (mapcar #'mu4e-setup--email-profile-add-to-mu4e-contexts
                mu4e-setup-current-email-profiles-list))))
   mu4e-setup-config-file))

(defun mu4e-setup-configure ()
  "Configure mu4e to use the emails listed in `mu4e-setup-email-profiles-list'."
  (if (file-exists-p mu4e-setup-config-file)
      (progn
        (load-file mu4e-setup-config-file)
        ;; TODO: check to make sure that our old email list is the same as
        ;; our current email list
        ;; (equal mu4e-setup-current-email-profiles-list mu4e-setup-email-profiles-list)
        ;; If not, then
        ;; (progn
        ;;   (shell-command (concat "rm " mu4e-setup-config-file))
        ;;   (mu4e-setup-configure))
        )
    (progn
      (mu4e-setup--configure-email-profiles mu4e-setup-email-profiles-list)
      (mu4e-setup--setup-config-file        mu4e-setup-email-profiles-list)

      ;; Load our just generated config file.
      (load-file mu4e-setup-config-file))))


;; Final usage should look like below:
;; (require 'mu4e-setup)

;; (setq mu4e-setup-email-profiles-list
;;       (list
;;        (mu4e-setup-email-profile
;;         :email-address "john.doe@hotmail.com"
;;         :imap-address "outlook.office365.com"
;;         :imap-port    "993"
;;         :smtp-address "smtp.office365.com"
;;         :smtp-port "587"
;;         :password-command "cat /some/file/path")

;;        (mu4e-setup-email-profile
;;         :email-address "someemail@gmail.com"
;;         :imap-address "imap.gmail.com"
;;         :smtp-address "smtp.gmail.com"
;;         :imap-port "993"
;;         :smtp-port "587"
;;         :password-command "echo hi")))

;; (mu4e-setup-configure)
