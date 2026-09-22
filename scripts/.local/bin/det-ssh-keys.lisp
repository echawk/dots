#!/usr/bin/sbcl --script

;; Simple utility for deterministic ssh keys.

;; This project really only exists because there is no
;; way to seed the generation of ssh keys in `ssh-keygen`.
;; - Well, at least in common lisp :)

;; If there were, then I would have not had to write this script.

;; SPDX-License-Identifier: BSD-2-Clause

(require :uiop)

(defconstant +ssh-dir+ (concatenate 'string (namestring (user-homedir-pathname)) ".ssh/"))

(defconstant +det-config-fp+ (merge-pathnames
                              ".config/det-ssh/keys.lisp"
                              (user-homedir-pathname)))

(load (merge-pathnames (user-homedir-pathname) ".sbclrc"))

;; Libraries needed only for the ssh key generation.
(ql:quickload '(:ironclad :cl-ppcre :lesspass :cl-ssh-keys) :silent t)

;; For the gui portion.
(ql:quickload :ltk :silent t)

(defun lesspass-prof-to-keyname (lesspass-prof)
  (format nil "~{~A~^~}"
          (list "site.-"
                (cl-ppcre:regex-replace-all
                 " " (lesspass:site-of lesspass-prof) "_")
                "-.login.-"
                (cl-ppcre:regex-replace-all
                 " " (lesspass:login-of lesspass-prof) "_")
                "-.detkey")))

(defun lesspass-prof-ssh-key-exists-p (lesspass-prof)
  (let* ((keyname (lesspass-prof-to-keyname lesspass-prof))
         (priv-key-path (concatenate 'string +ssh-dir+ keyname))
         (pub-key-path  (concatenate 'string +ssh-dir+ keyname ".pub")))
    (and (uiop:file-exists-p priv-key-path)
         (uiop:file-exists-p pub-key-path))))

(defun make-lesspass-prof (site login)
  ;; Only allow the user to change the site and login.
  ;; The rest of the defaults here should be sufficient.
  (make-instance
   'lesspass:password-profile
   :site site
   :login login
   ;; Ensure that the rules are as complex as they can be.
   :rules
   '(lesspass:lowercase
     lesspass:uppercase
     lesspass:digits
     lesspass:symbols)
   ;; 32 is the proper length for us to get a valid openssh key
   ;; as output - if we make the string too long, we will get
   ;; ssh keys which are too large for openssh to appropriately use.
   :length 32
   :counter 1))

(defun get-lesspass-prof-gui ()
  (let ((password-prof (make-lesspass-prof "" "")))
    (ltk:with-ltk ()
      (let* ((site-entry  (make-instance 'ltk:entry :width 30))
             (login-entry (make-instance 'ltk:entry :width 30))
             (confirm-button
               (make-instance
                'ltk:button
                :text "confirm"
                :command
                (lambda ()
                  (setf (lesspass:site-of password-prof)
                        (ltk:text site-entry))
                  (setf (lesspass:login-of password-prof)
                        (ltk:text login-entry))
                  (ltk:exit-wish)))))

        (ltk:grid
         (make-instance 'ltk:label :text "SITE:") 0 0)
        (ltk:grid site-entry 0 1)

        (ltk:grid
         (make-instance 'ltk:label :text "LOGIN:") 1 0)
        (ltk:grid login-entry 1 1)

        (ltk:grid confirm-button 10 0)))

    (assert (not (string= "" (lesspass:site-of  password-prof))))
    (assert (not (string= "" (lesspass:login-of password-prof))))
    password-prof))

(defun get-master-pass-gui ()
  (let ((master-pass ""))
    (ltk:with-ltk ()
      (let* ((pass-entry (make-instance 'ltk:entry :width 30 :show "*"))
             (confirm-button
               (make-instance
                'ltk:button
                :text "confirm"
                :command
                (lambda ()
                  (setf master-pass (ltk:text pass-entry))
                  (ltk:exit-wish)))))
        (ltk:grid
         (make-instance 'ltk:label :text "MPW:") 0 0)
        (ltk:grid pass-entry 0 1)
        (ltk:grid confirm-button 0 2)))

    (assert (not (string= "" master-pass)))
    master-pass))

(defun get-seed-string (password-prof master-pass)
  "Return a string that is 'good enough' to seed ironclad with."
  (lesspass:generate-password password-prof master-pass))

(defun generate-deterministic-keys (seed-string)
  "Will deterministically generate a ssh key pair from SEED-STRING.

The ssh key pair is returned as a list, with the private key being first
and the public key being second."
  (let* ((checksum-int
           (reduce #'+ (mapcar #'char-int (coerce seed-string 'list))))
         (sk (ironclad:ascii-string-to-byte-array seed-string))
         (pk (ironclad:ed25519-public-key sk))

         (ironclad-priv-key (ironclad:make-private-key :ed25519 :x sk :y pk))
         (ironclad-pub-key  (ironclad:make-public-key  :ed25519 :y pk))

         (key-type (ssh-keys:get-key-type-or-lose :ssh-ed25519 :by :id))

         (pub-key
           (make-instance 'ssh-keys:ed25519-public-key
                          :kind key-type
                          :y (ironclad:ed25519-key-y ironclad-pub-key)))
         (priv-key
           (make-instance 'ssh-keys:ed25519-private-key
                          :public-key pub-key
                          :cipher-name "none"
                          :kdf-name "none"
                          :checksum-int checksum-int
                          :kind key-type
                          :y (ironclad:ed25519-key-y ironclad-priv-key)
                          :x (ironclad:ed25519-key-x ironclad-priv-key))))
    (list priv-key pub-key)))

(defun make-ssh-key-for-password-prof-mpw (password-prof mpw)
  (let* ((keyname       (lesspass-prof-to-keyname password-prof))
         (seed-str      (get-seed-string password-prof mpw))
         (keys-lst      (generate-deterministic-keys seed-str))

         (priv-key-path (concatenate 'string +ssh-dir+ keyname))
         (pub-key-path  (concatenate 'string +ssh-dir+ keyname ".pub")))

    (uiop:delete-file-if-exists priv-key-path)
    (uiop:delete-file-if-exists pub-key-path)

    (ssh-keys:write-key-to-path (first  keys-lst) priv-key-path)
    (ssh-keys:write-key-to-path (second keys-lst) pub-key-path)

    (uiop:run-program (concatenate 'string "chmod 0600 " priv-key-path))
    (uiop:run-program (concatenate 'string "chmod 0600 " pub-key-path))))

(defun make-ssh-key-gui ()
  (make-ssh-key-for-password-prof-mpw
   (get-lesspass-prof-gui)
   (get-master-pass-gui)))

(defun make-ssh-key-auto ()
  (when (uiop:file-exists-p +det-config-fp+)
    (load +det-config-fp+)
    (print "Loaded user config..."))

  (when (boundp 'det-ssh-keys-plist)
    (let ((password-prof-lst
            (mapcar
             (lambda (pl)
               (make-lesspass-prof
                  (getf pl :site)
                  (getf pl :login)))
             (loop for (key value) on (symbol-value 'det-ssh-keys-plist) by #'cddr
                   collect value))))

      (when (member
             nil
             (mapcar #'lesspass-prof-ssh-key-exists-p password-prof-lst))
        (let ((mpw (get-master-pass-gui)))
          (mapcar
           (lambda (pass-prof)
             (make-ssh-key-for-password-prof-mpw pass-prof mpw))
           password-prof-lst))))))

(defun main ()
  (make-ssh-key-auto)
  (make-ssh-key-gui))

(main)
