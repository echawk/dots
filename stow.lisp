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

(defvar current-dir (uiop:getcwd))

;; Thank you AI...
(defun zip (&rest lists)
  (apply #'mapcar #'list lists))

(defun concat/space (lst) (format nil "~{~A~^ ~}" lst))

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
    (when (uiop:string-prefix-p prefix-str file-str)
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

(defun stow (dir &key (target (uiop:pathname-parent-directory-pathname current-dir)))
  "'stow' a directory DIR's contents into the TARGET directory."
  (let ((install-data (stow-get-install-data dir target))
        (folded-dirs nil))

    ;; 1. Directory Folding
    ;; If a target directory doesn't exist, we link the whole thing and track it.
    (loop for src-dir in (cdr (assoc "required-directories" install-data :test 'equal))
          for tgt-dir in (cdr (assoc "required-directories-in-target" install-data :test 'equal))
          unless (some (lambda (fd) (uiop:string-prefix-p fd (namestring tgt-dir))) folded-dirs)
            do (if (uiop:directory-exists-p tgt-dir)
                   (make-dir (namestring tgt-dir))
                   (progn
                     (make-link (namestring src-dir) (namestring tgt-dir))
                     (push (namestring tgt-dir) folded-dirs))))

    ;; 2. Symlink files
    ;; We skip creating file symlinks if their parent directory was already folded.
    (loop for src-file in (cdr (assoc "files-to-symlink" install-data :test 'equal))
          for tgt-file in (cdr (assoc "required-symlinks-in-target" install-data :test 'equal))
          unless (some (lambda (fd) (uiop:string-prefix-p fd (namestring tgt-file))) folded-dirs)
            do (make-link (namestring src-file) (namestring tgt-file)))))

(defun unstow (dir &key (target (uiop:pathname-parent-directory-pathname current-dir)))
  "Given a directory DIR, 'unstow' it from TARGET. The inverse of STOW."
  (let ((install-data (stow-get-install-data dir target))
        (folded-dirs nil))

    ;; 1. Unlink folded directories
    ;; If the target is a directory AND a symlink, it was folded. Unlink it.
    (loop for tgt-dir in (cdr (assoc "required-directories-in-target" install-data :test 'equal))
          if (and (uiop:directory-exists-p tgt-dir) (file-is-symlink-p tgt-dir))
            do (progn
                 (uiop:run-program (concat/space `("unlink" ,(namestring tgt-dir))))
                 (push (namestring tgt-dir) folded-dirs)))

    ;; 2. Unlink files
    ;; Skip files that were inside a directory we just unlinked.
    (loop for tgt-file in (cdr (assoc "required-symlinks-in-target" install-data :test 'equal))
          unless (some (lambda (fd) (uiop:string-prefix-p fd (namestring tgt-file))) folded-dirs)
            do (del-link (namestring tgt-file)))

    ;; 3. Prune empty directories
    ;; Reverse the list to process subdirectories before their parents (bottom-up).
    (loop for tgt-dir in (reverse (cdr (assoc "required-directories-in-target" install-data :test 'equal)))
          unless (some (lambda (fd) (uiop:string-prefix-p fd (namestring tgt-dir))) folded-dirs)
            do (when (uiop:directory-exists-p tgt-dir)
                 (del-dir (namestring tgt-dir))))))

(defun install-dots ()
  "Likely the behavior that you want from stow..."
  (let ((target (user-homedir-pathname))
        (dir    current-dir))
    (mapcar (lambda (d) (stow d :target target)) (uiop:subdirectories dir))))

(defun uninstall-dots ()
  "Uninstalls the dotfiles."
  (let ((target (user-homedir-pathname))
        (dir    current-dir))
    (mapcar (lambda (d) (unstow d :target target)) (uiop:subdirectories dir))))
