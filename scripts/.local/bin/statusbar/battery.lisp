#!/usr/bin/sbcl --script

;; Common Lisp port of the batter script for my status bar.

(require :uiop)

(defvar battery-status-alist
  '(("Discharging" "[<-]")
    ("Not charging" "[?!]")
    ("Charging" "[->]")
    ("Unknown" "[~~]")
    ("Full" "[==]")))


(defun battery-status-to-icon (file-path)
  (car
   (remove-if-not
    #'identity
    (loop :for status :in (mapcar #'car battery-status-alist)
          :collect
          (when (member status (uiop:read-file-lines file-path) :test #'string=)
            (cadr
             (assoc status battery-status-alist :test #'string=)))))))

(defun main ()
  (format
   t
   "~a~a"
   (apply
    #'concatenate
    'string
    (loop :for batt
            :in
            (remove-if-not
             (lambda (dir) (search "BAT" (namestring dir) :test #'char=))
             (uiop:subdirectories "/sys/class/power_supply/"))
          :collect
          (let ((cap
                  (parse-integer
                   (uiop:read-file-string (merge-pathnames batt "/capacity")))))
            (format nil "~a~a ~a% - "
                    (if (< cap 25) "!" "")
                    (battery-status-to-icon (merge-pathnames batt "/status"))
                    cap))))

   (let ((time-left-info
           (nth
            2
            (uiop:split-string
             (uiop:run-program "acpi -b" :output :string)
             :separator ","))))

     (if time-left-info
         (nth 1 (uiop:split-string time-left-info :separator " "))
         ""))))

(main)
