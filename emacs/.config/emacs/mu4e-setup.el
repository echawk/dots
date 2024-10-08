;;; Commentary:

;; This file is essentially a re-implementation of mutt-wizard, but for
;; mu4e. There are many missing features when compared to mutt-wizard,
;; such as POP support, but this config is really just to make it easier
;; to up email.

;; FIXME: msmtp simply cannot handle having it's config file in a different
;; location than ".config/msmtp/config" for whatever reason, at least on linux
;; it is driving me nuts.

;; I want to investigate using the built in smtp client for emacs, sincce
;; that is would hopefully be less likey to break compared to msmtp.

;; https://www.tomica.net/blog/2020/12/sending-mail-using-multiple-mu4e-contexts-in-emacs/
;; https://fanpengkong.com/post/emacs-email/emacs-email/
;; https://tushartyagi.com/blog/configure-mu4e-and-msmtp/
;; https://systemcrafters.net/emacs-mail/compose-and-send-email/
;; https://www.reddit.com/r/linuxquestions/comments/asjaw4/msmtp_account_default_not_found_no_configuration/

(require 'eieio)

(defcustom mu4e-setup-use-msmtp-p
  (if (boundp 'mu4e-setup-use-msmtp-p)
      mu4e-setup-use-msmtp-p
    nil)
  "Set to `t' if you would like to use msmtp instead of `smtpmail-send-it'.")

(defcustom mu4e-setup-mbsync-use-master-slave-p
  (if (boundp 'mu4e-setup-mbsync-use-master-slave-p)
      mu4e-setup-mbsync-use-master-slave-p
    nil)
  "Set to `t' if mbsync uses the Master/Slave terminology.")

(setq mu4e-setup-maildir      (concat (getenv "HOME") "/.local/share/mail"))
(setq mu4e-setup-dir          (concat user-emacs-directory "mu4e-setup/"))

(setq mu4e-setup-config-file  (concat mu4e-setup-dir "mu4e-config.el"))
(setq mu4e-setup-mbsync-file  (concat mu4e-setup-dir "mbsyncrc"))
;; msmtp doesn't play nice, hence the reason it is disabled by default.
(setq mu4e-setup-msmtp-file   (concat (getenv "HOME") "/.config/msmtp/config"))

(setq mu4e-setup-auth-sources-file (concat mu4e-setup-dir "authinfo"))

(setq mu4e-setup-mutt-wizard-repo "https://github.com/LukeSmithxyz/mutt-wizard")

(setq mu4e-setup-mbsync-cmd
      (concat "mbsync -c " mu4e-setup-mbsync-file " -a"))

(setq mu4e-setup-msmtp-cmd "msmtp")

(setq mu4e-setup-cert-file
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

(setq mu4e-setup-default-process-environment
      (append
       process-environment
       (list
        (concat "maildir="  mu4e-setup-maildir)
        (concat "msmtplog=" mu4e-setup-dir "msmtp.log")
        (concat "master="   (if mu4e-setup-mbsync-use-master-slave-p
                                "Master"
                              "Far"))
        (concat "slave="    (if mu4e-setup-mbsync-use-master-slave-p
                                "Slave"
                              "Near"))
        (concat "sslcert=" mu4e-setup-cert-file)
        ;; FIXME: have this be detected
        ;; FIXME: need to integrate the :smtp-mail-type into this
        ;; -or- have our own variable fr setting the imapssl type.
        ;; this is only really needed for davmail though.
        ;; for Davmail, we need to set this variable to 'None'
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

;; TODO: add in support for specifying whether the connection should use
;; 'none', 'ssl', or 'starttls' - If you were to use davmail, you don't need
;; to use the ssl code, and can just use the plain stuff.
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
   (smtp-stream-type
    :initarg :smtp-stream-type :initform "starttls" :type string
    :documentation "The type of stream for smtp. Defaults to `starttls'.")
   (password-command
    :initarg :password-command :initform "" :type string
    :documentation "A command that when ran returns the account's passowrd.")))

(defun mu4e-setup--email-profile-setup (obj)
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

      ;; FIXME: make this work better - if passwords have symbols in them
      ;; such as double quotes and whatnot this current method will not
      ;; work. May need to investigate using `princ'?
      ;; authinfo config
      (mu4e-setup--append-to-file
       (string-join
        (list
         "machine"
         smtp-address
         "port"
         smtp-port
         "login"
         email-address
         "password"
         (concat
          "\""
          (string-replace
           "\""
           "\\\""
           (string-trim (shell-command-to-string password-command)))
          "\"")
         "cert"
         mu4e-setup-cert-file
         "\n")
        " ")
       mu4e-setup-auth-sources-file)

      ;; mbsync config
      (mu4e-setup--append-to-file
       (shell-command-to-string
        (concat
         "sed '/PassCmd/s;.*;PassCmd \"" password-command "\";'"
         " < " mu4e-setup-dir "/mutt-wizard/share/mbsync-temp"
         " | "
         "envsubst"))
       mu4e-setup-mbsync-file)

      ;; This is so stupid, but mbsycnc *REFUSES* to be smart.
      (mu4e-setup--append-to-file
	   "

"
       mu4e-setup-mbsync-file)

      ;; msmtp config
      (mu4e-setup--append-to-file
       (shell-command-to-string
        (concat
         "sed '/passwordeval/s;.*;passwordeval \"" password-command "\";'"
         " < " mu4e-setup-dir "/mutt-wizard/share/msmtp-temp"
         " | "
         "envsubst"))
       mu4e-setup-msmtp-file)

      ;; Ensure this directory exists before we run mbsync.
      (shell-command (concat "mkdir -p " mu4e-setup-maildir "/" email-address)))))

(defun mu4e-setup--email-profile-add-to-mu4e-contexts (obj)
  (with-slots
      ((email-address    :email-address)
       (smtp-address     :smtp-address)
       (smtp-port        :smtp-port)
       (smtp-stream-type :smtp-stream-type))
      obj
    (let
        ((sent-folder
          (concat "/" email-address "/"
                  (cond
                   ((string= smtp-address "smtp.gmail.com") "[Gmail].Sent Mail")
                   (t                                       "Sent"))))
         (spam-folder
          (concat "/" email-address "/"
                  (cond
                   ((string= smtp-address "smtp.gmail.com") "[Gmail].Spam")
                   (t                                       "Spam"))))
         (trash-folder
          (concat "/" email-address "/"
                  (cond
                   ((string= smtp-address "smtp.gmail.com") "[Gmail].Trash")
                   (t                                       "Trash"))))
         (drafts-folder
          (concat "/" email-address "/"
                  (cond
                   ((string= smtp-address "smtp.gmail.com") "[Gmail].Drafts")
                   (t                                       "Drafts")))))

      (add-to-list
       'mu4e-contexts
       (make-mu4e-context
        :name email-address
        :match-func
        (lambda (msg)
          (when msg
            (string-prefix-p (concat "/" email-address)
                             (mu4e-message-field msg :maildir))))

        ;; Now to save the generated context into the contexts file
        :vars
        `((user-mail-address     . ,email-address)

          (smtpmail-smtp-server  . ,smtp-address)
          (smtpmail-smtp-service . ,smtp-port)
          (smtpmail-stream-type  . ,(intern smtp-stream-type))

          (mu4e-sent-folder      . ,sent-folder)
          (mu4e-spam-folder      . ,spam-folder)
          (mu4e-trash-folder     . ,trash-folder)
          (mu4e-drafts-folder    . ,drafts-folder)))))))


(defun mu4e-setup--configure-email-profiles (email-profiles-list)
  ;; Check for required commands.
  (dolist (cmd '("envsubst" "mu"))
    (unless (executable-find cmd)
      (error
       (concat "\"`" cmd "` needs to be present in $PATH."))))

  ;; Remove our old msmtp, mbsync, and auth-sources file
  (dolist (file (list
                 mu4e-setup-msmtp-file
                 mu4e-setup-mbsync-file
                 mu4e-setup-auth-sources-file))
    (when (file-exists-p file)
      (shell-command (concat "rm " file))))

  (dolist (email-profile email-profiles-list)
    (mu4e-setup--email-profile-setup email-profile))

  ;; Sync w/ mbsync
  ;;(shell-command mu4e-setup-mbsync-cmd)

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
  (with-temp-file mu4e-setup-config-file
    (prin1
     `(progn
        ;; Default configuration for mu4e here - can be overridden by
        ;; your own config - just set the variables to a different value.
        (if mu4e-setup-use-msmtp-p
            (setq sendmail-program           mu4e-setup-msmtp-cmd
                  send-mail-function         #'sendmail-send-it
                  message-sendmail-extra-arguments '("--read-envelope-from")
                  message-sendmail-f-is-evil        t)
          (setq message-send-mail-function #'smtpmail-send-it))

        (setq mu4e-change-filenames-when-moving 't
              mu4e-get-mail-command             ,mu4e-setup-mbsync-cmd
              mu4e-maildir                      ,mu4e-setup-maildir)

        ;; This is important to save (as a different symbol), since we can
        ;; compare the two lists to see if we have any new email profiles to
        ;; setup.
        (setq mu4e-setup-current-email-profiles-list
              ',email-profiles-list)

        ;; Add our custom auth sources file to the auth-sources variable.
        (add-to-list 'auth-sources ,mu4e-setup-auth-sources-file)

        ;; Ensure that each of the email profiles is a context in mu4e.
        (mapcar #'mu4e-setup--email-profile-add-to-mu4e-contexts
                mu4e-setup-current-email-profiles-list))
     (current-buffer))))

(defun mu4e-setup-configure ()
  "Configure mu4e to use the emails listed in `mu4e-setup-email-profiles-list'."
  (unless (boundp 'mu4e-setup-email-profiles-list)
    (error "mu4e-setup-email-profiles-list isn't bound!"))
  (if (file-exists-p mu4e-setup-config-file)
      (progn
        (load-file mu4e-setup-config-file)
        (unless
            ;; Ensure that the user hasn't added/removed any email configurations.
            (equal mu4e-setup-current-email-profiles-list
                   mu4e-setup-email-profiles-list)

          ;; Remove our configuration file and re-setup mu4e-setup
          (shell-command (concat "rm " mu4e-setup-config-file))
          (mu4e-setup-configure)))
    (progn
      (mu4e-setup--configure-email-profiles mu4e-setup-email-profiles-list)
      (mu4e-setup--setup-config-file        mu4e-setup-email-profiles-list)

      ;; Load our just generated config file.
      (load-file mu4e-setup-config-file))))

(provide 'mu4e-setup)

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
