#!/usr/bin/sbcl --script

;; clopen - a Common Lisp "replacement" for xdg-open
;; Copyright (c) 2024, Ethan Hawk
;; SPDX-License-Identifier: MIT

#+SBCL
(declaim (sb-ext:muffle-conditions cl:warning))

(require :uiop)

(load (merge-pathnames (user-homedir-pathname) ".sbclrc"))

;; :trivial-mimes - Tiny library to determine mimetypes.
;; :cl-ppcre - Standard CL regex library.
(ql:quickload '(:trivial-mimes :cl-ppcre :bordeaux-threads) :silent t)

;; Directories that contain desktop files
(defparameter +system-desktop-files-dir+ #P"/usr/share/applications/")
(defparameter +user-desktop-files-dir+
  (concatenate 'string (namestring (uiop:xdg-data-home)) "applications/"))

;; The standard configuration file for xdg-open
(defparameter +mimeappslist+
  (concatenate 'string (namestring (uiop:xdg-config-home)) "mimeapps.list"))

;; These files are tab separated value files, with the first collumn
;; being the regular expression, and the second column being the command
;; to run. '%M' in the second column is replaced with the appropriate uri.
;; This is so that commands can be arbitrarily complex.

(defvar *regex-rules-file*
  (merge-pathnames "clopen/rerules.tsv" (uiop:xdg-config-home)))

(defvar *mime-rules-file*
  (merge-pathnames "clopen/mimerules.tsv" (uiop:xdg-config-home)))


(defun split-string-on-tabs (s)
  "Split S on every tab character. Uses `uiop:split-string`."
  (uiop:split-string s :separator '(#\Tab)))

(defun first-char-is-hash-p (s)
  "Return t if the first character of S is a '#', nil otherwise."
  (char= #\# (first (coerce s 'list))))

(defun read-regex-rules ()
  "Return the contents of *REGEX-RULES-FILE* as a list of lists."
  (when (uiop:file-exists-p *regex-rules-file*)
    (mapcar
     #'split-string-on-tabs
     (remove-if
      #'first-char-is-hash-p
      (uiop:read-file-lines *regex-rules-file*)))))

(defun read-mime-rules ()
  "Return the contents of *MIME-RULES-FILE* as a list of lists."
  (when (uiop:file-exists-p *mime-rules-file*)
    (mapcar
     #'split-string-on-tabs
     (remove-if
      #'first-char-is-hash-p
      (uiop:read-file-lines *mime-rules-file*)))))

(defun read-mimeapp-list ()
  "Return the contents of *MIMEAPPSLIST* as a list of lists."
  (when (uiop:file-exists-p +mimeappslist+)
    ;; Only keep lists that are length 2, since those are our pairs of
    ;; mimetypes and desktop files.
    (remove-if-not
     (lambda (lst) (= 2 (length lst)))
     (mapcar
      (lambda (line)
        ;; Remove semicolons from the line and split on '='
        (uiop:split-string
         (remove #\; line)
         :separator '(#\=)))
      (uiop:read-file-lines +mimeappslist+)))))

(defun find-matching-regex-rule (uri)
  "Return the first regex rule from *REGEX-RULES-FILE* that matches URI."
  (let* ((rules (read-regex-rules))
         (matched-rule
           (car
            (remove-if-not
             (lambda (regex) (cl-ppcre:scan regex uri))
             (mapcar #'car rules)))))
    (first
     (remove-if-not
      (lambda (rule)
        (string= matched-rule (car rule)))
      rules))))

(defun find-matching-mime-rule (mimestring)
  "Return the first mime rule from *MIME-RULES-FILE* that matches MIMESTRING."
  (first
   (remove-if-not
    (lambda (rule)
      (cl-ppcre:scan (car rule)
                     mimestring))
    (read-mime-rules))))

(defun find-matching-mimeapp-rule (mimestring)
  "Return the first rule from +MIMEAPPSLIST+ which matches MIMESTRING."
  (first
   (remove-if-not
    (lambda (rule) (string= mimestring (car rule)))
    (read-mimeapp-list))))

(defun get-desktop-file-path (desktop-file)
  "Return the absolute file path of DESKTOP-FILE.
Preference is given for files within +USER-DESKTOP-FILES-DIR+."
  (when desktop-file
    (first
     (remove-if-not
      #'identity
      (list
       (uiop:file-exists-p
        (merge-pathnames +user-desktop-files-dir+   desktop-file))
       (uiop:file-exists-p
        (merge-pathnames +system-desktop-files-dir+ desktop-file)))))))

(defun get-command-from-desktop-file (desktop-file-path)
  "Return the command contained in DESKTOP-FILE-PATH."
  (when desktop-file-path
    (cl-ppcre:regex-replace
     "^Exec="
     (first
      (remove-if-not
       (lambda (line) (cl-ppcre:scan "^Exec=" line))
       (uiop:read-file-lines desktop-file-path)))
     "")))

(defun find-matching-mimeapp-command (mimestring)
  "Return the proper command from a given MIMESTRING searching
+MIMEAPPLIST+ for the appropriate desktop file."
  (get-command-from-desktop-file
   (get-desktop-file-path
    (second (find-matching-mimeapp-rule mimestring)))))

(defun get-mimetypes-from-desktop-file (desktop-file-path)
  "Return a list of valid mimetypes within DESKTOP-FILE-PATH."
  (when desktop-file-path
    (uiop:split-string
     (cl-ppcre:regex-replace
      "^MimeType="
      (first
       (remove-if-not
        (lambda (line) (cl-ppcre:scan "^MimeType=" line))
        (uiop:read-file-lines desktop-file-path)))
      "")
     :separator '(#\;))))

(defun find-matching-system-command (mimestring)
  "Search +SYSTEM-DESKTOP-FILES-DIR+ for a desktop file which supports
MIMESTRING."
  (get-command-from-desktop-file
   (first
    (remove-if-not
     (lambda (desktop-file)
       (some
        (lambda (hay) (string= mimestring hay))
        (get-mimetypes-from-desktop-file desktop-file)))
     (uiop:directory-files +system-desktop-files-dir+)))))

(defun get-command (uri mimestring)
  "Return a symbol determining which config file we should look at
for a particular MIMESTRING."
  (first
   (remove-if-not
    #'identity
    ;; List here is in order of precedence.
    (list

     (when (uiop:file-exists-p *regex-rules-file*)
       (cl-ppcre:regex-replace
        "%M"
        (second (find-matching-regex-rule uri))
        uri))

     (when (uiop:file-exists-p *mime-rules-file*)
       (cl-ppcre:regex-replace
        "%M"
        (second (find-matching-mime-rule  mimestring))
        uri))

     (when (uiop:file-exists-p +mimeappslist+)
       (cl-ppcre:regex-replace
        "%[fFuU]"
        (find-matching-mimeapp-command mimestring)
        uri))

     ;; TODO: make this a little bit neater?
     ;; TODO: also make this whole process a bit lazier (w.r.t evaluation)
     (cl-ppcre:regex-replace
      "%[fFuU]"
      (find-matching-system-command mimestring)
      uri)))))

(defun open-uri (uri)
  "Open URI with the appropriate program in the background."
  (let* ((command (get-command uri (mimes:mime uri))))
    (when command
      (uiop:run-program command :wait nil))))

(defun main ()
  ;; Join each of the threads.
  (mapcar #'bordeaux-threads-2:join-thread
          ;; Make a separate thread for each of the arguments.
          (mapcar (lambda (arg)
                    (bordeaux-threads-2:make-thread (lambda () (open-uri arg))))
                  (uiop:command-line-arguments)))
  ;; End the main thread.
  (bordeaux-threads-2:thread-yield))

(main)
