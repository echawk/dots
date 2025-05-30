#!/usr/bin/sbcl --script

;; Common Lisp rewrite of:
;; https://github.com/salman-abedin/devour/blob/master/devour.c

#+SBCL
(declaim (sb-ext:muffle-conditions cl:warning))

(require :uiop)

(load (merge-pathnames (user-homedir-pathname) ".sbclrc"))

(ql:quickload '(:clx) :silent t)

(let* ((dis (xlib:open-default-display))
       (win (xlib:input-focus dis)))

  ;; Unmap the window and force it to sync w/ the X server.
  (xlib:unmap-window win)
  (xlib:window-map-state win)

  ;; Execute whatever arguments we were given.
  (uiop:run-program
   (format nil "~{~A ~}"
           ;; Escape any arguments that have spaces since UIOP expects this.
           (mapcar (lambda (s) (if (search " " s) (uiop:escape-shell-token s) s))
                   (uiop:command-line-arguments))))

  ;; Re-map the window and force it to sync w/ the X server.
  (xlib:map-window win)
  (xlib:window-map-state win))
