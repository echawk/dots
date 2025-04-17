;; Minimal CL script to behave *close enough* to GNU stow...
;; Primary reason for this script is to avoid perl like the plague...

;; sbcl --load stow.lisp --eval "(stow:install-dots)"

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

(defun file-is-symlink-p (fp)
  "Portable way to determine if a file path is a symlink or not."
  (not
   (equalp
    (enough-namestring (probe-file fp))
    (enough-namestring fp))))

(defun walk (dir &key (ignore nil))
  "Walk DIR and collect *files* that do not contain the substring IGNORE in their pathname."
  (mapcar
   #'pathname
   (remove-if
    (lambda (fp)
      (or
       (uiop:emptyp fp)
       (and (not (null ignore))
            (search ignore fp))))
    (uiop:split-string
     (uiop:run-program
      (concat/space `("find" "-L" ,(namestring dir) "-type" "f"))
      :output :string)
     :separator '(#\Newline)))))


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
      (uiop:run-program (concat/space `("ln -s " ,from ,to)))))

(defun make-dir (dir)
  (if (uiop:directory-exists-p dir)
      (format t "Directory '~a' already exists on the file system.~%" dir)
      (uiop:run-program (concat/space `("mkdir -p " ,dir)))))

(defun make-symlinks (files-to-symlink required-symlinks-in-target)
  (mapcar
   (lambda (p)
     (let ((from (car  p))
           (to   (cadr p)))
       (make-link from to)))
   (zip files-to-symlink required-symlinks-in-target)))
(defun make-dirs (required-directories-in-target)
  (mapcar #'make-dir required-directories-in-target))

(defun del-link (link)
  (if (and (uiop:file-exists-p link) (file-is-symlink-p link))
      (uiop:run-program (concat/space `("unlink" ,link)))
      (format t "Link '~a' does not exist on the file system. Refusing to unlink.~%" link)))

(defun del-dir (dir)
  (if (uiop:directory-exists-p dir)
      (uiop:run-program (concat/space `("rmdir" ,dir)))
      (format t "Directory '~a' does not exist on the file system.~%" dir)))

(defun del-symlinks (symlinks-in-target)
  (mapcar #'del-link symlinks-in-target))
(defun del-dirs (directories-in-target)
  (mapcar #'del-dir directories-in-target))

(defun stow-get-install-data (dir target)
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
    `(("files-to-symlink" . ,files-to-symlink)
      ("required-directories" . ,required-directories)
      ("required-directories-in-target" . ,required-directories-in-target)
      ("required-symlinks-in-target" . ,required-symlinks-in-target))))

(defun stow (dir
             &key (target (uiop:pathname-parent-directory-pathname current-dir)))
  "'stow' a directory DIR's contents into the TARGET directory."

  ;; TODO: technically should add asserts here?
  ;; Ensure that everything actually can occur as it should.
  (let ((install-data (stow-get-install-data dir target)))

    ;; FIXME: add in flag here that can swap the behavior of the creation
    ;; of directories. Since in upstream stow, if a directory does not exist,
    ;; then instead of it being created, a symlink is made instead.

    ;; Make any of the required directories.
    (make-dirs
     (assoc "required-directories-in-target" install-data :test 'equal))

    ;; Make our symlinks.
    (make-symlinks
     (assoc "files-to-symlink"            install-data :test 'equal)
     (assoc "required-symlinks-in-target" install-data :test 'equal))))

(defun unstow (dir
               &key (target (uiop:pathname-parent-directory-pathname current-dir)))
  "Given a directory DIR, 'unstow' it from TARGET. The inverse of STOW."
  (let ((install-data (stow-get-install-data dir target)))

    ;; Make any of the required directories.
    (del-dirs
     (assoc "required-directories-in-target" install-data :test 'equal))

    ;; Make our symlinks.
    (del-symlinks
     (assoc "required-symlinks-in-target" install-data :test 'equal))))

(defun install-dots ()
  "Likely the behavior that you want from stow..."
  (let ((target (user-homedir-pathname))
        (dir    current-dir))
    (mapcar (lambda (d) (stow d :target target)) (uiop:subdirectories dir))))

(defun uninstall-dots ()
  "Uninstalls the dotfiles."
  (let ((target (user-homedir-pathname))
        (dir    current-dir))
    (mapcar (lambda (d) (unstow d :target target) (uiop:subdirectories dir)))))
