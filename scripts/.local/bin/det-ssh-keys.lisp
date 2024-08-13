#!/usr/bin/sbcl --script

;; Simple utility for deterministic ssh keys.

;; This project really only exists because there is no
;; way to seed the generation of ssh keys in `ssh-keygen`.
;; - Well, at least in common lisp :)

;; If there were, then I would have not had to write this script.

;; SPDX-License-Identifier: BSD-2-Clause

(require :uiop)

(load (merge-pathnames (user-homedir-pathname) ".sbclrc"))

;; Libraries needed only for the ssh key generation.
(ql:quickload '(:ironclad :lesspass :cl-ssh-keys) :silent t)

;; For the gui portion.
(ql:quickload :ltk :silent t)

(defun get-lesspass-prof-gui ()
  (let ((password-prof
          ;; Only allow the user to change the site and login.
          ;; The rest of the defaults here should be sufficient.

          (make-instance
           'lesspass:password-profile
           :site ""
           :login ""
           ;; Ensure that the rules are as complex as they can be.
           :rules
           '(lesspass:lowercase
             lesspass:uppercase
             lesspass:digits
             lesspass:symbols)
           ;; 32 is right at the point of diminishing returns.
           ;; If we go too much higher than 32, then we start to run out
           ;; of entropy for the lesspass calculation.
           ;; If we go too low, then our seed string to ironclad will
           ;; be shorter, and thus *hypothetically* easier to crack.

           ;; With the algorithm in this file, we will end up with
           ;; a string of length 1600 - this should be reasonably secure
           ;; as a seed. Maybe not perfect, but even with the setup now
           ;; it takes ~ 15 seconds to generate a keypair. Any extra length
           ;; would make this process slower.
           :length 32
           ;; The counter here is just set to 1 to make the constructor happy :)
           :counter 1)))
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

(defun get-key-filename-gui ()
  (let ((filename ""))
    (ltk:with-ltk ()
      (let* ((filename-entry (make-instance 'ltk:entry :width 30))
             (confirm-button
               (make-instance
                'ltk:button
                :text "confirm"
                :command
                (lambda ()
                  (setf filename (ltk:text filename-entry))
                  (ltk:exit-wish)))))
        (ltk:grid
         (make-instance 'ltk:label :text "FILENAME:") 0 0)
        (ltk:grid filename-entry 0 1)
        (ltk:grid confirm-button 0 2)))

    (assert (not (string= "" filename)))
    filename))

(defun get-step-size-from-string (str n)
  "Return a pseudo random number for a given STR.

Sums the value of all of the characters in STR and divides them by N."
  (let* ((entropy (reduce
                   #'+
                   (mapcar
                    (lambda (ch)
                      (expt (char-int ch) 2))
                    (coerce str 'list))))
         (step (floor (coerce (/ entropy n) 'float) 1)))
    step))

(defun get-seed-string ()
  "Return a string that is 'good enough' to seed ironclad with."
  (declare (optimize (safety 3)))
  (let* ((iters 50)
         (master-pass   (get-master-pass-gui))
         (password-prof (get-lesspass-prof-gui))

         ;; Get the step value from the master password since that information
         ;; is also secret, thus making it more secure against brute forcing.
         (step (get-step-size-from-string
                master-pass iters)))
    (apply 'concatenate 'string
           (loop :for i :from 1 :to iters
                 ;; Ensure that the counter of the lesspass prof is some
                 ;; pseudo-random value. Each different value for the
                 ;; counter *will* change the password.
                 :do (setf (lesspass:counter-of password-prof)
                           (* i step))
                 :collect
                 (lesspass:generate-password password-prof master-pass)))))

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

(defun main ()
  (let* ((keyname  (get-key-filename-gui))
         (ssh-dir  (concatenate 'string (namestring (user-homedir-pathname)) ".ssh/"))
         (seed-str (get-seed-string))
         (keys-lst (generate-deterministic-keys seed-str)))

    (let ((priv-key-path (concatenate 'string ssh-dir keyname))
          (pub-key-path  (concatenate 'string ssh-dir keyname ".pub")))


      (uiop:delete-file-if-exists priv-key-path)
      (uiop:delete-file-if-exists pub-key-path)

      (ssh-keys:write-key-to-path (first  keys-lst) priv-key-path)
      (ssh-keys:write-key-to-path (second keys-lst) pub-key-path))))

(main)
