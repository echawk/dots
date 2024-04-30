#!/usr/bin/sbcl --script

;; Common Lisp port of the internet script for my status bar.

(require :uiop)

(defun identify-internet-file (file)
  "Return a single character string for FILE."
  (let ((fs
          (concatenate 'string
                       (directory-namestring file)
                       (file-namestring file))))
    (cond
      ((search "wlan" fs) "w")
      ((search "eth" fs) "e")
      (t nil))))

(defun main ()
  (mapcar
   (lambda (pair)
     (format t
             " ~a[~a] "
             (identify-internet-file (car pair))
             (if (cadr pair) "x" " ")))

   ;; Return a list of the interface file w/ a boolean indicating whether
   ;; the interface is currently active.
   (mapcar
    (lambda (file) (list file
                    (member "up" (uiop:read-file-lines file) :test #'string=)))
    (mapcar

     ;; Get the 'operstate' path, which will be read to determine the
     ;; status of the interface.
     (lambda (netpath)
       (merge-pathnames "operstate" netpath))

     ;; Get the list of all of the interfaces.
     (remove-if-not
      (lambda (path)
        (let ((s (directory-namestring path)))
          (or (search "wlan" s) (search "eth" s))))
      (uiop:subdirectories #p"/sys/class/net/"))))))

(main)
