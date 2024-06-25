#!/usr/bin/sbcl --script

;; Inspired by:
;; https://web.archive.org/web/20240430195410/https://aur.archlinux.org/cgit/aur.git/plain/wpa-psk.py?h=wpa-psk

#+sbcl
(declaim (sb-ext:muffle-conditions cl:warning))

(load (merge-pathnames (user-homedir-pathname) ".sbclrc"))

(ql:quickload '(:ironclad :clingon) :silent t)

(defconstant +valid-wifi-daemons+ '(iwd wpa-supplicant))

(setq wpa-supplicant-fmt-str
      "network={
        ssid=\"~a\"
        ~a
}
")

(setq iwd-fmt-str
      "[Security]
PreSharedKey=~a
")

(defun get-ssid-pass-hashed (ssid pass)
  (let* ((ascii-pass (ironclad:ascii-string-to-byte-array pass))
         (ascii-salt (ironclad:ascii-string-to-byte-array ssid)))

    ;; Ensure the pass is the right length.
    (let ((l (length ascii-pass)))
      (assert (and (<= 8 l) (<= l 63))))

    ;; Ensure that there are no illegal characters in the pass string.
    (assert
     (member nil (mapcar (lambda (c) (or (< c 32) (= c 127)))
                         (coerce ascii-pass 'list))))

    (ironclad:byte-array-to-hex-string
     (ironclad:derive-key
      (ironclad:make-kdf 'ironclad:pbkdf2 :digest 'ironclad:sha1)
      ascii-pass ascii-salt 4096 32))))

(defun cli-options ()
  (list
   (clingon:make-option
    :boolean/true
    :description "Make output compatible with iwd."
    :long-name "iwd"
    :key :iwd)
   (clingon:make-option
    :boolean/true
    :description "Make output compatible with wpa_supplicant."
    :long-name "wpa-supplicant"
    :key :wpa-supplicant)
   (clingon:make-option
    :string
    :description "Set the SSID for the WiFi network."
    :long-name "ssid"
    :initial-value ""
    :key :ssid)
   (clingon:make-option
    :string
    :description "Set the passphrase for the WiFi network."
    :long-name "password"
    :initial-value ""
    :key :password)))

(defun generate-output (daemon ssid password)
  (assert (not (string= "" ssid)))
  (assert (member daemon +valid-wifi-daemons+))
  (if (string= "" password)
      (cond
       ((eq daemon 'iwd) "")
       ((eq daemon 'wpa-supplicant)
        (format nil wpa-supplicant-fmt-str ssid "key_mgmt=NONE")))
    (let ((hashed-pass (get-ssid-pass-hashed ssid password)))
      (cond
       ((eq daemon 'iwd)
        (format nil iwd-fmt-str hashed-pass))
       ((eq daemon 'wpa-supplicant)
        (format nil wpa-supplicant-fmt-str
                ssid
                (concatenate 'string "psk=" hashed-pass)))))))

(defun cli-handler (cmd)
  (let ((iwd-toggle (clingon:getopt cmd :iwd))
        (wpa-toggle (clingon:getopt cmd :wpa-supplicant))
        (ssid       (clingon:getopt cmd :ssid))
        (password   (clingon:getopt cmd :password)))

    (format t "~a"
            (generate-output
             (cond
              (wpa-toggle 'wpa-supplicant)
              (iwd-toggle 'iwd)
              (t          'wpa-supplicant))
             ssid
             password))))

(defun cli-command ()
  (clingon:make-command
   :name "wpa-psk"
   :version "1.0.0"
   :options (cli-options)
   :handler #'cli-handler))

(defparameter *app* (cli-command))

(defun main () (clingon:run *app*))

(main)
