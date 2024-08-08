#!/usr/bin/env sbcl --script

;; Simple program to get TOTP codes.
;; Expects account tokens to be in ~/.config/totp/
;; ex: `echo "mysecrettoken" > ~/.config/totp/github_personal`

(require :uiop)

(load (merge-pathnames (user-homedir-pathname) ".sbclrc"))

;; cl-one-time-passwords - implements actual totp algo
;; ironclad - dep of cl-one-time-passwords, but also used to convert bytes
;; cl-base32 - needed to convert bytes
;; ltk - easy to use cross platform gui (uses tcl/tk under the hood)
(ql:quickload '(:cl-one-time-passwords :ironclad :cl-base32 :ltk) :silent t)

(defparameter *tsps-config-dir*
  (concatenate 'string (namestring (uiop:xdg-config-home)) "totp/"))

(defun get-totp-code-for-file-contents (file-path)
  (totp:totp
   (ironclad:byte-array-to-hex-string
    (cl-base32:base32-to-bytes
     (uiop:read-file-line
      file-path)))))

(ltk:with-ltk ()
  (let* ((box
           (make-instance 'ltk:combobox
                          :width 30
                          :values (uiop:directory-files *tsps-config-dir*)))
         (copy-button
           (make-instance 'ltk:button
                          :text "copy"
                          :command
                          (lambda ()
                            (let ((code
                                    (get-totp-code-for-file-contents
                                     (ltk:text box))))
                              (ltk:clipboard-clear)
                              (ltk:clipboard-append code))))))
    (ltk:grid box 0 0)
    (ltk:grid copy-button 0 1)))
