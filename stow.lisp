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


;; Below will get all of our "prefix-dirs"
;; (uiop:subdirectories current-dir)

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

;; (let ((ex-path #P"/Users/ethan/Downloads/dots/X/.config/picom.conf"))
;;   (get-target-dir-path ex-path current-dir))

;; (pathname "/Users/ethan/.config/sx/sxrc")

;; (osicat:make-link ())

;; Honestly I'm pretty close to having this script done. And to think it's
;; less than 100 lines of CL!

;; We first need to get a list of all of the files/directories that we
;; have to create and pair them with their appropriate prefix.

;; Then, we get the path that the files will actually have on disk as a symlink
;; First, we check to make sure that all of the requisite directories exist on
;; disk, if a directory does *not* exist on disk, we make it and it's parents.
;; This ensures that when we go to symlink files, there are no issues.
;; Then, if the file does not already exist on disk, we use osicat:make-link
;; to make a symlink from the current directory, *to* the new one.

(defun make-link (from to)
  (if (osicat:file-exists-p to)
      (format t "File '~a' already exists on the file system. Refusing to symlink '~a' to that path.~%" to from)
      (osicat:make-link
       to
       :target from
       :hard nil)))

;;(make-link "/Users/ethan/tmp/a.py" "/Users/ethan/tmp/b.py")

(defun make-dir (dir)
  (if (osicat:file-exists-p dir)
      (format t "Directory '~a' already exists on the file system.~%" dir)
      (sb-unix:unix-mkdir (namestring dir) #o777 )))

;;(make-dir #P"/Users/ethan/tmp/ttt/a")

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
