;; Minimal CL script to behave *close enough* to GNU stow...
;; Primary reason for this script is to avoid perl like the plague...

;; Assume that we are being called from a sbcl w/ quicklisp installed.
;; - This will *eventually* be done with the `install` script at the /
;; of my dots repo. Right now it is definitely not the case at the moment.

;; I want the final script to effectively be this:
;; sbcl --load $PWD/dev/.sbclrc --load stow.lisp --eval "(stow:stow :target $HOME)"

(require :uiop)

(uiop:define-package #:stow
  (:export
   #:stow
   #:install-dots)
  (:use #:cl))

(in-package #:stow)

(setq current-dir (uiop:getcwd))

;; Thank you AI...
(defun zip (&rest lists)
  (let ((result '()))
    (loop for i from 0 below (apply #'min (mapcar #'length lists))
          do (push (mapcar (lambda (lst) (nth i lst)) lists) result))
    (nreverse result)))

(defun concat/space (lst) (format nil "~{~A ~}" lst))

(defun walk (dir &key (ignore nil))
  "Walk DIR and collect *files* that do not contain the substring IGNORE in their pathname."
  (mapcar
   #'pathname
   (remove-if
    #'uiop:emptyp
    (remove-if
     (lambda (fp) (and (not (null ignore))
                  (search ignore fp)))
     (uiop:split-string
      (uiop:run-program
       (concat/space `("find" ,(namestring dir) "-type" "f"))
       :output :string)
      :separator '(#\Newline))))))


(defun get-target-dir-path (file-path prefix-dir target-dir)
  "Given a FILE-PATH and a PREFIX-DIR, which is typically set to the
dotfiles repo, return a pathname that is located in the users home directory.
"
  (let ((prefix-str (namestring prefix-dir))
        (file-str   (namestring file-path)))
    (when
        (string= prefix-str
                 (subseq file-str
                         0
                         (length prefix-str)))
      (pathname
       (concatenate 'string
                    (namestring target-dir)
                    (subseq file-str (length prefix-str)))))))

(defun make-link (from to)
  (if (uiop:file-exists-p to)
      (format t "File '~a' already exists on the file system. Refusing to symlink '~a' to that path.~%" to from)
      (uiop:run-program (format nil "~{~A ~}" `("ln -s " ,from ,to)))))

(defun make-dir (dir)
  (if (uiop:directory-exists-p dir)
      (format t "Directory '~a' already exists on the file system.~%" dir)
      (uiop:run-program (format nil "~{~A ~}" `("mkdir -p " ,dir)))))


(defun stow (dir
             &key (target (uiop:pathname-parent-directory-pathname current-dir)))
  "'stow' a directory DIR's contents into the TARGET directory."
  ;; TODO: technically should add asserts here?
  ;; Ensure that everything actually can occur as it should.
  (let* ((files-to-symlink (walk dir :ignore ".git/"))
         (required-directories
           ;; Sort to ensure that base directories are made before their subdirs.
           (sort
            (remove-duplicates
             (mapcar
              (lambda (f)
                (make-pathname :directory (pathname-directory f)))
              files-to-symlink))
            (lambda (p1 p2)
              (<
               (length (namestring p1))
               (length (namestring p2))))))
         (required-directories-in-target
           (mapcar (lambda (dp) (get-target-dir-path dp dir target))
                   required-directories))
         (required-symlinks-in-target
           (mapcar (lambda (fp) (get-target-dir-path fp dir target))
                   files-to-symlink)))

    ;; Make any of the required directories.
    (mapcar #'make-dir required-directories-in-target)

    ;; Make our symlinks.
    (mapcar
     (lambda (p)
       (let ((from (car  p))
             (to   (cadr p)))
         (make-link from to)))
     (zip files-to-symlink required-symlinks-in-target))))

(defun install-dots ()
  "Likely the behavior that you want from stow..."
  (let ((target (user-homedir-pathname))
        (dir    current-dir))
    (mapcar (lambda (d) (stow d :target target)) (uiop:subdirectories dir))))
