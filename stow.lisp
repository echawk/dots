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
   #:stow)
  (:use #:cl))

(in-package #:stow)

#-SBCL (error "This script *sadly* requires SBCL for `sb-unix:unix-mkdir`")

(ql:quickload :osicat)

(setq stow::current-dir (uiop:getcwd))

;; Thank you AI...
(defun zip (&rest lists)
  (let ((result '()))
    (loop for i from 0 below (apply #'min (mapcar #'length lists))
          do (push (mapcar (lambda (lst) (nth i lst)) lists) result))
    (nreverse result)))

(defun walk (dir &key (ignore nil))
  "Walk DIR and collect files that do not contain the substring IGNORE in their pathname."
  (let ((files '()))
    (osicat:walk-directory
     dir
     (lambda (path)
       (let ((actual-path (merge-pathnames *default-pathname-defaults* path)))
         (unless
             (and
              (not (null ignore))
              (search ignore (namestring actual-path)))
           (push actual-path files)))))
    files))


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

(defun stow-dir-into-target (dir target)
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


;; FIXME: *technically* stow defaults to the parent directory.
;; However, since this script is *just enough* of "stow" to
;; manage my dotfiles, set the default target to be effectively $HOME.
(defun stow (&key (target (user-homedir-pathname)) (dir current-dir))
  (mapcar (lambda (d) (stow-dir-into-target d target)) (uiop:subdirectories dir)))
