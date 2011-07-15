;;; :FILE mon-systems/file-dir.lisp
;;; ==============================

;;; ==============================
;;
;; Following is the Lord Voldemort of CL forms:
;; It is dangerous code that one should _never_ be evaluated:
;;  (m@pc@r (lambda (x) (ignore-errors (delete-file x)))
;;         (directory "/**/*.*")) ;  <-- IOW don't evaluate that EVER!
;;
;; For more details google: <87zmgn327x.fsf@thalassa.informatimago.com>
;;
;; #lisp 2011-06-02
;; <pjb> mon_key: notice that it doesn't delete everything on your system.
;;      Actually, it doesn't delete anything of importance, only all the files
;;      owned by the user. (But don't run it as root).  It's the same as rm -rf
;;      /, which is rather benign again, unless you run it as root.
;; <pjb> mon_key: to  the contrary of, eg. FORMAT C:, which did delete
;;       everything, since there was no notion of user and file protection on
;;       MS-DOS.
;;
;;; ==============================


;;; ==============================
;;
;; :TODO
;; :EMACS-LISP-COMPAT `symbol-file', `locate-library', `process-lines', `directory-files'
;;
;; sb-int:c-strings->string-list 
;; sb-ext:parse-native-namestring
;; sb-ext:native-namestring
;; sb-ext:native-pathname
;;
;;; ==============================
;; SBCL directory/file-fun
;; sbcl/src/code/filesys.lisp
;;
;; sb-impl::query-file-system
;; sb-impl::native-file-kind
;; sb-impl::canonicalize-pathname
;; sb-impl::with-native-directory-iterator
;; sb-impl::call-with-native-directory-iterator
;; sb-impl::map-directory
;; sb-impl::map-matching-directories
;; sb-impl::last-directory-piece
;; sb-impl::map-wild
;; sb-impl::map-wild-inferiors
;; sb-impl::map-matching-entries
;; sb-impl::pathname-intersections
;; sb-impl::intersect-directory-helper
;; sb-impl::remove-backslashes)
;; sb-impl::unparse-physical-piece
;; sb-impl::extract-name-type-and-version
;; sb-ext:delete-directory
;; sb-int:sbcl-homedir-pathname
;; sb-impl::user-homedir-namestring
;; sb-impl::physicalize-pathname
;; sb-unix:unix-rename
;; sb-impl::pathname=
;; sb-impl::*logical-hosts*
;; sb-impl::*physical-host*
;; sb-impl::*win32-host*
;; sb-impl::*unix-host*
;;
;;; ==============================

;;; ==============================
;; SB-POSIX NOTES
;; 
;; :SEE info node (info "(libc)Temporary Files")
;;
;; sb-posix:mkdtemp  ;; create a directory with a unique name
;; sb-posix:mktemp   ;;  generates a unique file name
;; sb-posix:mkstemp  ;; sb-posix:mktemp does, but it also opens the file as with `cl:open'
;;
;; access
;; alarm
;; chdir
;; chmod
;; chown
;; chroot
;; close
;; closedir
;; creat
;; dup
;; dup2
;; fchdir
;; fchmod
;; fchown
;; fcntl
;; fdatasync
;; fork
;; fsync
;; ftruncate
;; getegid
;; geteuid
;; getpgid
;; getpgrp
;; getpid
;; getppid
;; getresgid
;; getsid
;; getuid
;; ioctl
;; kill
;; killpg
;; lchown
;; link
;; lockf
;; lseek
;; lstat 
;; mkdir
;; mkfifo
;; open
;; opendir
;; pause
;; readdir
;; readlink   ;; Return value of the symbolic link of PATHSPEC. :SEE (info "(libc)Symbolic Links")
;; getcwd
;; rename
;; rmdir
;; setegid
;; seteuid
;; setfsgid
;; setgid
;; setpgid
;; setpgrp
;; setregid
;; setresgid
;; setreuid
;; setsid
;; setuid
;; sb-posix:symlink ;; Return a symbolic link to OLDPATH named NEWPATH. :SEE (info "(libc)Symbolic Links")
;; sync
;; truncate
;; umask
;; unlink
;; wait
;; waitpid
;; wifexited
;; wexitstatus
;; wifsignaled
;; wtermsig
;; wifstopped
;; wstopsig
;;
;; mmap
;; munmap
;; msync
;; mlockall
;; munlockall
;; getpagesize
;; 
;; passwd         ;; CLASS
;; passwd-name    ;; ACCESSOR
;; passwd-passwd  ;; ACCESSOR
;; passwd-name    ;; ACCESSOR
;; passwd-passwd  ;; ACCESSOR
;; passwd-uid     ;; ACCESSOR
;; passwd-gid     ;; ACCESSOR
;; passwd-gecos   ;; ACCESSOR
;; passwd-dir     ;; ACCESSOR
;; passwd-shell   ;; ACCESSOR
;;
;; group          ;; CLASS
;; group-name     ;; ACCESSOR
;; group-passwd   ;; ACCESSOR
;; group-gid      ;; ACCESSOR
;; getpwnam
;; getpwuid
;; getgrnam
;; getgrgid
;;
;;
;; pipe     ;; :SEE info node (info "(libc)Pipes and FIFOs") 
;; s_isreg  ;;
;; s_isdir  ;; return non-zero if file is a directory.
;; s_ischr  ;; return non-zero if file is a character special file (e.g. like a terminal)
;; s_isblk  ;; return non-zero if file is a block special file (e.g. like a disk).
;; s_isfifo ;; return non-zero if file is a FIFO special file, or a pipe.
;; s_islnk  ;; return non-zero if file is a symbolic link.   :SEE info node "(libc)Symbolic Links"
;; s_issock ;; return non-zero if file is a socket.          :SEE info node (info "(libc)Sockets")
;; :SEE info node (info "(libc)Testing the Type of a File")
;;
;; sb-posix:termios   ;; CLASS
;; Instances of this class represent I/O characteristics of the terminal.
;; sb-posix:termios-iflag ;; Input modes.        ;; ACCESSOR
;; sb-posix:termios-oflag ;; Output modes.       ;; ACCESSOR
;; sb-posix:termios-cflag ;; Control modes.      ;; ACCESSOR
;; sb-posix:termios-lflag ;; Local modes.        ;; ACCESSOR
;; sb-posix:termios-cc    ;; Control characters  ;; ACCESSOR
;;
;; (describe-object 'sb-posix:termios t)
;; (describe 'sb-posix:termios t)
;; (getf '(:array-length 18 :documentation "docstring") :array-length)
;; (where-is "define-protocol-class")
;; flock, passwd, group, timeval, stat, termios
;;
;; ,---- 
;; | Class: sb-poxix:stat
;; | Class precedence list: `stat, standard-object, t'
;; | Slots:
;; |    * `mode' :initarg `:mode' :reader `sb-posix:stat-mode'
;; |      Mode of file.
;; |
;; |    * `ino' :initarg `:ino' :reader `sb-posix:stat-ino'
;; |      File serial number.
;; | 
;; |    * `dev' :initarg `:dev' :reader `sb-posix:stat-dev'
;; |      Device `id' of device containing file.
;; | 
;; |    * `nlink' :initarg `:nlink' :reader `sb-posix:stat-nlink'
;; |      Number of hard links to the file.
;; | 
;; |    * `uid' :initarg `:uid' :reader `sb-posix:stat-uid'
;; |      User `id' of file.
;; | 
;; |    * `gid' :initarg `:gid' :reader `sb-posix:stat-gid'
;; |      Group `id' of file.
;; | 
;; |    * `size' :initarg `:size' :reader `sb-posix:stat-size'
;; |      For regular files, the file size in bytes.
;; |      For symbolic links, the length in bytes of the filename contained in the
;; |      symbolic link.
;; | 
;; |    * `atime' :initarg `:atime' :reader `sb-posix:stat-atime'
;; |      Time of last access.
;; | 
;; |    * `mtime' :initarg `:mtime' :reader `sb-posix:stat-mtime'
;; |      Time of last data modification.
;; | 
;; |    * `ctime' :initarg `:ctime' :reader `sb-posix:stat-ctime'
;; |      Time of last status change
;; `---- :SEE info node (info "(sbcl)Lisp objects and C structures")
;;
;;; ==============================


;;; ==============================
;; 
;;  asdf::*defined-systems*
;;
;; ASDF's CPL
;; component
;;  `-> source-file
;;     `->   cl-source-file
;;     `->   c-source-file
;;     `->   java-source-file
;;     `->   static-file
;;     `->   doc-file
;;          `->   html-file 
;;  `> module
;;     `-> system 
;;
;; operation
;;  `-> compile-op
;;  `-> basic-load-op
;;     `-> load-op
;;     `-> load-source-op
;;  `-> test-op

;; asdf::*user-cache*
;; asdf::*system-cache*
;; asdf::*output-translations*
;; asdf::*default-output-translations*
;; asdf::*output-translations-file*
;; asdf::*output-translations-directory*

;; asdf::*default-source-registry-exclusions*
;; asdf::*source-registry-exclusions*
;; asdf::*source-registry*
;; asdf::*central-registry*
;; *default-source-registries*
;; *source-registry-file*
;; *source-registry-directory*
;; asdf:disable-output-translations
;;
;;
;; (sb-posix:getenv "ASDF_OUTPUT_TRANSLATIONS")
;; (sb-posix:getenv "CL_SOURCE_REGISTRY")
;; (sb-posix:getenv "SBCL_HOME")
;; (sb-posix:getenv "XDG_CONFIG_HOME")
;; (sb-posix:getenv "XDG_CONFIG_DIRS")
;; (sb-posix:getenv "XDG_CACHE_HOME")
;; (sb-posix:getenv "XDG_DATA_HOME")
;; (sb-posix:getenv "XDG_DATA_DIRS")  ;; this picks up the gpg-error shite!
;;  => "/usr/share/common-lisp/source/gpg-error/"
;; 
;; (sb-posix:getenv "DEVHOME") ;; mon-local
;;

;;; ==============================
;; (sb-impl::map-directory 
;;; ==============================
;; #lisp 2011-03-01 
;; re removing directories without following symbolinks:
;; <nikodemus> (cffi:foreign-funcall "system" :string (format nil "rm -rf ~A" dir) :int) ; the dirty option  [15:18]
;; <lichtblau> yay, from symlink problem to space-in-filename problem
;;
;;
;;; ==============================


(in-package #:mon)

(defun file-name-directory (filename)
  (directory-namestring  filename))

(defun file-truename (filename) 
  (truename filename))

(defun file-directory-p (filename)
  (and (probe-file filename)
       (equal (make-pathname :directory (pathname-directory (truename filename)))
	      (pathname (truename filename)))))

(defun directory-file-name (dirname)
  (let ((this-dir (pathname-directory dirname))
	new-dir
	as-fname)
    (setf new-dir (reverse this-dir))
    (setf as-fname (pop new-dir))
    (setf new-dir (reverse new-dir))
    (make-pathname :name as-fname :directory new-dir)))

(declaim (inline %probe-file-if-string-or-pathname))
(defun %probe-file-if-string-or-pathname (putative-pathname) ;; &key (as-pathnames t)
  ;; Is putative-pathname `cl:stringp' or `cl:pathnamep' if so return its `cl:pathname'.
  (unless (or (stringp putative-pathname) 
              (pathnamep putative-pathname))
    (return-from %probe-file-if-string-or-pathname
      (values nil (type-of putative-pathname))))
  (locally 
      (declare (pathname-or-namestring putative-pathname))
    ;; (let ((nrmlz-to-pathname (pathname putative-pathname)))
    ;; (declare (pathname nrmlz-to-pathname))
    (etypecase putative-pathname
      (string (if (string-not-empty-or-all-whitespace-p putative-pathname)
                  (values (pathname putative-pathname) :string)
                  (values nil :string-empty)))
      (pathname (values putative-pathname :pathname)))))

#+sbcl
(defun pathname-native-file-kind (putative-pathname)  ;; &key (as-pathnames t)
  (declare (inline %probe-file-if-string-or-pathname)
           (optimize (speed 3)))
  (let* ((pathname-chk 
          (multiple-value-bind (pnfk-chk pnfk-typ) (%probe-file-if-string-or-pathname putative-pathname)
            (if pnfk-chk 
                (cons pnfk-chk pnfk-typ)
                (return-from pathname-native-file-kind (values pnfk-chk pnfk-typ)))))
         (pathname-namestring-if (sb-ext:native-namestring (the pathname (car pathname-chk)))))
    (declare (cons pathname-chk)
             (string pathname-namestring-if))
    (values 
     (sb-impl::native-file-kind pathname-namestring-if)
     (ecase (cdr pathname-chk)
       (:string pathname-namestring-if)
       (:pathname (pathname pathname-namestring-if))))))

#+sbcl
(defun probe-directory (putative-pathname-dir)
  (let ((pathname-chk (multiple-value-list 
                       (pathname-native-file-kind putative-pathname-dir))))
    (ecase (car pathname-chk)
      ((nil) (values nil (cadr pathname-chk) putative-pathname-dir))
      (:file (values nil :file (cadr pathname-chk)))
      (:special (values nil :special (cadr pathname-chk)))
      (:symlink (let ((probed (probe-file  (cadr pathname-chk))))
                  (values nil :symlink (cons (pathname (cadr pathname-chk)) probed))))
      (:directory (values (truename (cadr pathname-chk)) :directory 
                          (cons (cadr pathname-chk) putative-pathname-dir))))))

(defun pathname-file-if (putative-pathname &key allow-directory) ;; (as-pathnames t)
  (declare (inline %probe-file-if-string-or-pathname)
           (optimize (speed 3)))
  (let* ((pathname-chk (multiple-value-bind (pfi-str-or-pth pfi-val)
                           (%probe-file-if-string-or-pathname putative-pathname)
                         (if pfi-str-or-pth 
                             pfi-str-or-pth
                             (return-from pathname-file-if (values pfi-str-or-pth pfi-val)))))
         (pathname-if  pathname-chk))
    (declare (pathname pathname-if))
    ;; :NOTE The non-SBCL version has a different behaviour w/r/t symlinks!
    #-sbcl
    (when (setf pathname-chk (probe-file pathname-if))
      (locally (declare (pathname pathname-chk))
        (if allow-directory
            pathname-chk
            (when (not (equal pathname-chk
                              (make-pathname :directory (pathname-directory pathname-chk))))
              pathname-chk))))
    #+sbcl 
    (multiple-value-bind (path-type path-if) (pathname-native-file-kind pathname-if)
      (ecase path-type
        ((nil :symlink) nil)
        (:file (truename path-if))
        (:directory (when allow-directory 
                      (truename path-if)))))))

(defun pathname-file-list-if (namestring-list &key allow-directory (as-pathnames t))
  (declare (boolean as-pathnames))
  (flet ((filter-files (filename)
           (pathname-file-if filename :allow-directory allow-directory)))
    (let ((filtered (remove-if-not #'filter-files namestring-list)))
      (declare (list filtered))
      (if as-pathnames
          (loop for path in filtered collect (pathname path))
          (loop for path in filtered collect (namestring path)))
      )))

;; :SOURCE slime/swank.lisp :WAS `merged-directory'
(defun pathname-directory-merged (dirname pathname-defaults)
  (pathname-directory (pathname-directory-append dirname pathname-defaults)))

;; :SOURCE slime/swank-loader.lisp :WAS `append-dir'
(defun pathname-directory-append (dirname pathname-defaults)
  (merge-pathnames 
   (make-pathname :directory `(:relative ,dirname) 
		  ;; :directory (or absolute *default-pathname-defaults*))
		  :defaults pathname-defaults) 
   pathname-defaults))

;;; :NOTE Delete once expand-file-name is finished.
(defun to-directory (path)
  (if (char= #\/ (schar path (1- (length path)))) path
      (concatenate 'string path "/")))

(defun expand-file-name (name &optional (default-dir *default-pathname-defaults*))
  (namestring (merge-pathnames (to-directory default-dir) name)))

;;; :COURTESY freedius/lisp/lisp/lisp-io.lisp
(defun unix-dot-directory-p (path)
  ;;(declare (type pathname-designator path))
  (let* ((path (if (pathnamep path) (namestring path) path))
	 (rt-side (subseq path (1+ (or (position #\/ path :from-end t ) -1)))))
    (or (string-equal rt-side ".")
	(string-equal rt-side ".."))))

;;; ==============================
;; (defun append-slash (path)
;;   "append / to path if there is none at the end"
;;   (if (char= (car (last (coerce path 'list))) #\/)
;;       (setf path (concatenate 'string path "/")))
;;   path)
;;
;;; :COURTESY buildapp-1.1/utils.lisp :WAS `directorize'
(defun directorize-namestring (namestring)
  ;;(declare (type filename-designator path))
  (declare (type string namestring))  
  (concatenate 'string (string-right-trim "/" namestring) "/"))

;;; :COURTESY freedius/lisp/lisp/lisp-io.lisp
(defun rename-file* (file new-name)
  ;; (declare (type filename-designator pathname))
  (if (pathname-type new-name)
      (rename-file file new-name)   
      (rename-file file (make-pathname :defaults new-name :type :UNSPECIFIC))))

;;; :SOURCE quicklisp/quicklisp/utils.lisp
(defun replace-file (from to)
  ;;(declare (type filename-designator from to))
  (when (probe-file to)
    (delete-file to))
  (rename-file from to))

;;; :SOURCE quicklisp/quicklisp/utils.lisp
;;
;; :NOTE Definition of `cl:open' in :FILE sbcl/src/code/fd-stream.lisp 
;; has this:
;; ,----
;; | (:probe  (values   t nil sb!unix:o_rdonly)) 
;; `----
;; where sb!unix:o_rdonly is a constant => #x0 e.g. a null byte.
;;
;; The :direction :probe causes cl:open to effectively evalutate to: 
;;  ,----
;;  | (let ((stream (%make-fd-stream :name namestring
;;  |                                :fd fd
;;  |                                :pathname pathname
;;  |                                :element-type element-type)))
;;  |     (close stream)
;;  |     stream)
;;  `----
;;
;; :NOTE Also that :external-format is not likely needed b/c were just pinging a
;; file descriptor.
;;
;; Also, the `fd-stream' defstruct in :FILE sbcl/src/code/fd-stream.lisp
;; hash this:
;;  ,----
;;  | ;; Not :DEFAULT, because we want to match CHAR-SIZE!
;;  | (external-format :latin-1)
;;  `----
;;
;; :external-format SB-IMPL::*DEFAULT-EXTERNAL-FORMAT*
;; :element-type         
;;
(defun ensure-file-exists (pathname)
  (declare (type pathname-designator pathname))
  (open pathname 
        :direction :probe 
        :if-does-not-exist :create))

;;: :TODO This should be more careful about deleting directories
(defun delete-file-if-exists (pathname)
  (declare (type pathname-designator pathname))
  (when (or (streamp pathname) 
            (wild-pathname-p pathname))
    ;; `cl:delete-file' won't delete a directory and signals an error. 
    ;; we should catch it before it has a chance.
    ;; (cl-fad:directory-pathname-p #P"./bubba/") *default-pathname-defaults*)
    (let ((wrn (format nil 
                       "~%~T:FUNCTION `delete-file-if-exists' --~%~12T~
                       declining to delete PATHNAME, got: ~S" 
                       pathname
                       ;; (or (and (streamp pathname) 
                       ;;          (string 'cl:streamp) 
                       ;;          (string 'cl:wild-pathname-p))
                       ;;     *default-pathname-defaults*)
                       )))
      (warn wrn)
      (return-from delete-file-if-exists (values nil wrn))))
  (when (probe-file pathname)
    (delete-file pathname)))

;; (open-stream-p  (ensure-file-exists #P"/tmp/pp-btree/"))
;; (probe-file (delete-file #P"/tmp/pp-btree/"))


;;; :SOURCE buildapp-1.1/utils.lisp
;;; :NOTE differs from `cl-fad:copy-file'
;;; :NOTE what about an :external-format?
(defun copy-file (input output &key (if-exists :supersede))
  ;; (declare (type pathname-designator input outuput))  
  (with-open-file (input-stream input)
    (with-open-file (output-stream output
				   :direction :output
                                   :if-exists if-exists)
      (loop
	 for char = (read-char input-stream nil)
	 while char do (write-char char output-stream)))))

;;; :SOURCE mcclim/Apps/Listener/util.lisp :WAS `strip-filespec'
(defun pathname-strip-filespec (pathname)
  ;; (declare (type pathname-designator pathname))  
  (make-pathname :name nil
                 :type nil
                 :version nil
		 #+scl :query #+scl nil
		 :defaults pathname))

(defun directory-parent (pathname)
  ;; (declare (type pathname-designator pathname))
  (declare (type pathname pathname))
  (make-pathname :host (pathname-host pathname)
                 :device (pathname-device pathname)
                 ;; :NOTE consider using `cl-fad:component-present-p'
                 :directory (if (and (pathname-name pathname)
                                     (not (eq :unspecific (pathname-name pathname))))
                                (pathname-directory pathname)
                                (butlast (pathname-directory pathname)))))

;; :SOURCE cl-docutils-20101006-git/utilities.lisp :WAS `find-file'
(defun find-file-search-path (pathname &key (search-path (or *search-path* (list *default-pathname-defaults*))))
  ;; (declare (type pathname-designator pathname))
  (flet ((ff (directory)
           (some #'probe-file (directory (merge-pathnames pathname directory)))))
    (some #'(lambda (path)
	      (etypecase path
		(list (some #'ff path))
		(string (some #'ff (string-split-on-chars path ":")))
		(pathname (ff path))))
	  search-path)))

;;; :SOURCE de.setf.utility/pathnames.lisp
#+sbcl
(defun logical-hosts ()
  ;; (where-is "*logical-hosts*")
  (when (hash-table-p (and SB-IMPL::*LOGICAL-HOSTS*))
    (loop
       for host being each hash-key of SB-IMPL::*LOGICAL-HOSTS*
       collect host)))


;;; ==============================
;;; :CL-FAD
;;; ==============================

(defun directory-files (directory) ;; &optional full match nosort)
  ;; DIRECTORY is a directory whose files to list
  ;; Return value is as if by values if DIRECTORY does not satisfy `cl-fad:directory-exists-p'
  ;;
  ;; (directory-files *default-pathname-defaults*)
  ;;
  ;; (directory-files
  ;;  (merge-pathnames 
  ;;   (make-pathname :directory '(:relative "bubba"))
  ;;   *default-pathname-defaults*))
  ;; :EMACS-COMPAT
  ;; 
  ;; (declare (type pathname-designator directory))
  (and (setf directory
	     (or (cl-fad:directory-exists-p 
		  (pathname-directory-pathname directory))
		 (return-from directory-files 
		   (values directory (cl-fad:directory-exists-p directory)))))
       (cl-fad:list-directory directory)))

(defun pathname-directory-pathname (pathname)
  (declare (type pathname-designator pathname))
  (cl-fad:pathname-as-directory pathname))

(defun make-pathname-user-homedir (&key user path)
  (declare (string-or-null user)
           (proper-list path))
  #-sbcl (check-type user string-or-null)
  #-sbcl (check-type path proper-list)
  (if user
      (if (string-not-empty-or-all-whitespace-p user)
          #-sbcl(pathname 
                 (namestring
                  (make-pathname :directory `(,@(pathname-directory (directory-file-name (user-homedir-pathname))) ,@path))))
          #+sbcl (pathname 
                  (sb-ext:native-namestring 
                   (make-pathname :directory `(:absolute :home ,user ,@path))))
          (simple-error-mon :w-sym "make-pathname-user-homedir"
                            :w-type 'function
                            :w-spec "Keyword USER did not satisfy `mon:string-not-empty-or-all-whitespace-p'"
                            :w-got user
                            :w-type-of t
                            :signal-or-only nil))
      #-sbcl (pathname 
              (namestring 
               (make-pathname :directory `(,@(pathname-directory (user-homedir-pathname)) ,@path))))
      #+sbcl (pathname 
              (sb-ext:native-namestring 
               (make-pathname :directory `(,@(pathname-directory (user-homedir-pathname)) ,@path))))))

(defun make-pathname-directory-wildcard (pathname)
  ;; Return pathname with pathname-name and pathname-type :wild
  (declare (type filename-designator pathname))
  (cl-fad::directory-wildcard pathname))

(defun make-pathname-directory-w-type-wild (pathname pathname-name)
  ;; Return value is as if by values.
  ;; PATHNAME is a pathname or namestring
  ;; PATHNAME-NAME is of type string-or-null
  ;; (make-pathname-directory-w-type-wild *default-pathname-defaults* "lisp")
  (declare (type filename-designator pathname)
           (type string-or-null pathname-name))
  (unless ;; 
      (and (or (string-null-or-empty-p pathname)
	       (string-null-or-empty-p pathname-name)
	       (not 
		(setf pathname    
		      (and (setf pathname (pathname-directory-pathname pathname))
			   (or (cl-fad:directory-exists-p pathname)
			       (return-from make-pathname-directory-w-type-wild 
				 (values pathname (wild-pathname-p pathname))))))))
	   (values pathname (wild-pathname-p pathname)))
    (setf pathname 
	  (merge-pathnames 
	   (make-pathname :name pathname-name :type :wild )
	   pathname))
    (values pathname (wild-pathname-p pathname))))

;;; ==============================
;; :FINISH-ME
;; (defun make-pathname-directory-w-name-wild (pathname)
;; Return pathname with and pathname-type :wild
;; (declare (type filename-designator pathname))
;;   (fad::directory-wildcard pathname))

;; :SOURCE asdf.lisp :WAS *wild-path*
;; (defparameter *wild-pathname*
;;   (make-pathname :directory '(:relative :wild-inferiors)
;;                  :name :wild :type :wild :version :wild))

;; :SOURCE asdf.lisp :WAS `wilden'
;; merge-pathnames-with-wild
;; (defun* wilden (path)
;;   (merge-pathnames* *wild-path* path))
;; 
;; (defun probe-sbcl-source-file  (subd file)
;;   ;;(probe-file 
;;    (merge-pathnames 
;;     (make-pathname :directory `(:relative ,subd)
;; 		   :name file
;; 		   :type "lisp")
;;     (make-pathname			; :name :wild :type "lisp" ;; wild
;;      :directory `(,@(pathname-directory 
;; 		     (truename (sb-posix:getenv "DEVHOME")))
;; 		    "CL-SYSTEMS" "sbcl" "src"))))
;;; ==============================



;;; ==============================
;;; :SOURCE freedius/freedius/lisp/system-tool/ev-pathnames.lisp
;;; This is a verision of `substitute-in-file-name' based on that found in emacs/src/fileio.c
;;; Originally, this required use of `*ev-getenv-auto-import*' and `ev-getenv'.
;;; Trying instead to use `sb-posix:getenv'/`sb-ext:posix-getenv'.
;;; The original source is included below should that not work.
;;; :NOTE :SEE `sb-ext:posix-environ' for additional ideas w/re `ev-getenv'.
;;; 
;; (defparameter *ev-getenv-auto-import* :remember)
;;
;; (defun ev-getenv (var-name) ;; :was &optional (auto-import *ev-getenv-auto-import*))
;;   (let ((symbol (intern var-name :env)))
;;     (if (boundp symbol)
;; 	(symbol-value symbol)
;; 	(when auto-import
;; 	  (let ((val (getenv var-name)))
;; 	    (when (and val (eq auto-import :remember))
;; 	      (setf (symbol-value symbol) val))
;; 	    val)))))
;;
;; (defun (setf ev-getenv) (var-val var-name)
;;   (let ((symbol (intern (format nil "*~a*" var-name) :env)))
;;     (setf (symbol-value symbol)
;; 	  var-val)))
(defun substitute-in-file-name (filename)
  (declare (type string filename))
  ;;(declare (optimize (speed 0) (safety 3) (debug 3)))
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (unless (position #\$ filename)
    (return-from substitute-in-file-name filename))
  (loop with n fixnum = (length filename)
	with jnext fixnum
	with j fixnum = 0
	with result = ""
	with ch of-type character
	for p = (position #\$ filename :start j)
	while p
	do 
     (let ((p (1+ p)))			; scan past $
       (declare (fixnum p)) 
       (when (>= p n)
	 (error "SUBSTITUTE-IN-FILE-NAME: $ at end of ~a" filename))
       (setq ch  (aref filename p))
       (if (char= ch #\$) ;; $$ in filename
	   (setq result (concatenate 'string result (subseq filename j (1+ p)))
		 j (1+ p))
	   (let* ((var-name (if (char= ch #\{)
				(let ((pc (position #\} filename :start (1+ p))))
				  (unless pc 
				    (error "SUBSTITUTE-IN-FILE-NAME: Missing \"}\" in ~a" filename))
				  (setq jnext (1+ pc))
				  (subseq filename (1+ p) pc))
				(loop for i fixnum from p below n
				   for ch of-type character = (aref filename i)
				   while (or ;;(alphanumericp ch)
					  (alpha-char-p ch)
                                          (digit-char-p ch) ;; radix??
					  (char= ch #\_))
				   finally 
				   ;;(setq jnext (min i (the fixnum (1- n))))
				   (setq jnext i)
				   (return (subseq filename p i)))))
		   ;;; :WAS (var-val (ev-getenv var-name)))
		  ;; sb-ext:posix-getenv ;; sb-posix:getenv
		  (var-val (sb-ext:posix-getenv var-name)))
	     (cond (var-val
		    (setq result (concatenate 'string result (subseq filename j (1- p)) var-val)))
		   ((char= (aref filename (1- jnext)) #\})
		    (error "SUBSTITUTE-IN-FILE-NAME: nonexistent environment variable: ~a" var-name))
		   (t (setq result (concatenate 'string result (subseq filename j jnext)))))
	     (setq j jnext))))
	finally (return (concatenate 'string result (subseq filename j)))))

(defun pathname-components (pathname-or-namestring &key (list-or-plist nil) (plist-for-backquote nil))
  (declare (pathname-or-namestring pathname-or-namestring)
           (boolean plist-for-backquote)
           (inline logical-pathname-p)
           (optimize (speed 3)))
  (assert (member list-or-plist '(:plist :list :values nil t)))
  (let* ((nrmlz-path      (pathname pathname-or-namestring))
         (path-is-logical (logical-pathname-p nrmlz-path))
         (backquotable  (and (eql list-or-plist :plist) plist-for-backquote))
         (plist-style   (if backquotable
                            (list :host :device :directory :name :type :version)
                            (list :pathname-or-logical :pathname-host :pathname-device
                                  :pathname-directory  :pathname-name :pathname-type
                                  :pathname-version)))
         (path-pieces '()))
    (declare (pathname nrmlz-path))
    (setf path-is-logical 
          (cons path-is-logical 
                (if path-is-logical :pathname-logical :pathname-physical)))
    ;; :NOTE We cons for the host value b/c cl:pathname-host may return an
    ;; unreadable-object for nrmlz-path when its not `mon:logical-pathname-p'
    ;; i.e. on SBCL: 
    ;;  (pathname-host #P"SYS:SRC;CODE;TARGET-PATHNAME.LISP")
    ;;  => #<SB-KERNEL:LOGICAL-HOST "SYS">
    (setf path-pieces
          (list 
           (cdr path-is-logical)                        ; nth-value 0
           (cons (when (pop path-is-logical) (host-namestring nrmlz-path))
                 (pathname-host nrmlz-path))             ; nth-value 1
           (or (pathname-device nrmlz-path) :unspecific) ; nth-value 2
           (pathname-directory  nrmlz-path)              ; nth-value 3
           (pathname-name       nrmlz-path)              ; nth-value 4
           (pathname-type       nrmlz-path)              ; nth-value 5
           (pathname-version    nrmlz-path)))            ; nth-value 6
    (when backquotable 
      (pop path-pieces)
      (setf path-pieces (rplaca path-pieces (caar path-pieces))))
    (ecase list-or-plist
      (:plist 
       (setf path-pieces
             (loop 
                for props in plist-style
                for vals in path-pieces
                nconcing (list props vals) into path-pieces
                finally (return path-pieces)))
       (if backquotable 
           (values path-pieces path-is-logical)
           path-pieces))
      (:list  path-pieces)
      ((:values nil t) (values-list path-pieces)))))

(defun pathname-components-funcallable-pairs (pathname-or-namestring)
  (declare (pathname-or-namestring pathname-or-namestring))
  (let* ((comp-plist (pathname-components pathname-or-namestring :list-or-plist :plist))
         (comp-tail (nthcdr 2 comp-plist))
         (comp-head (nbutlast comp-plist 12)))
    ;; pop the tail off the value of the :pathname-host property 
    ;;  ( { <STRING> | NIL } . <LOGICAL-HOST-OBJECT> ) => { <STRING> | NIL }
    (setf (cadr comp-tail) (caadr comp-tail)
          comp-head (cadr comp-head)
          comp-plist nil)
    ;; :NOTE On SBCL 1.0.47.1 return value of (make-pathname :type "bmp") is IMO bogus.
    ;; A workaround would be to special case around :pathname-type with something like this: 
    ;; #+sbcl for paths = (if (eql key :pathname-type) 
    ;;                     (pathname (pathname-type (make-pathname :type "bmp")))
    ;; Which is equally bogus...
    (loop        
       for (head . tail) on comp-tail by #'cddr
       for funs = (keyword-property-to-function head)
       for key =  (gethash head *keyword-hash-inverted*)
       for paths = `(make-pathname ,(gethash head *keyword-hash-inverted*) ,(car tail))
       ;; for paths =  (apply 'make-pathname `(,(gethash head *keyword-hash-inverted*) ,(car tail)))
       collect (list funs paths))))

;;; :SOURCE clocc/src/cllib/fileio.lisp :WAS `file-newer'
;;; :TODO elide the ignore-errors and check w/ probe-file for FILE1 FILE2 check 
(defun file-newer-than-file-p (file1 file2)
  (flet ((fwd (ff) 
	   (or (ignore-errors (file-write-date ff)) 0)))
    (> (fwd file1) (fwd file2))))

#+sbcl 
(defun remove-directory (pathname)
  (declare (optimize (speed 3)))
  (sb-posix:rmdir pathname))

;;; ==============================
;;  As of asdf::*asdf-version* => 2.010
;;  `asdf::make-defined-systems-table' specifies :test 'equal 
;;  e.g. the `cl:hash-table-test' for `asdf::*defined-systems*' => 'equal
;;  So system-name's are stored to the hash-table as string-downcase'd via
;;  `asdf:coerce-name'.
;;  Should this ever change to equalp we needn't downcase the string.
;;
;; (equal (gethash "mon" asdf::*defined-systems*) 'equal)
;; 
#+asdf
(defun pathname-directory-system (system &optional preserve-case)
  ;; b/c asdf:find-system is way too overloaded and "finds" the system by
  ;; loading it.
  ;; :EXAMPLE (pathname-directory-system :mon)
  (declare (string-or-symbol system))
  (when (or (string-empty-p system)
            (booleanp system))
    (return-from pathname-directory-system))
  (let* ((frob-w 
          (ecase (hash-table-test (the hash-table asdf::*defined-systems*))
            (equal  (or (and preserve-case (function string)) (function string-downcase)))
            (equalp (function string))))
         (sys-str 
          (funcall frob-w
                   (etypecase system
                     (string system)
                     (symbol (string system))))))
    (setf sys-str (asdf:system-registered-p sys-str))
    (and sys-str 
         (setf sys-str (cdr sys-str))
         ;; The class asdf:component doesn't initialize the absolute-pathname
         ;; slot by default...  so we must check first, e.g.:
         ;; (slot-value (class-of (cdr (asdf:system-registered-p "not-a-system")))
         ;;             'asdf::absolute-pathname) => error
         (slot-boundp sys-str 'asdf::absolute-pathname)
         (setf sys-str (slot-value  sys-str 'asdf::absolute-pathname))
         (and sys-str (truename sys-str)))))

#+asdf
(defun pathname-directory-system-ensure (system)
  (and (setf system (asdf:find-system system nil))
       (pathname-directory
        (truename (asdf:system-definition-pathname system)))))
;; 
#+asdf
(defun pathname-system (system)
  (and (setf system (pathname-directory-system system))
       (make-pathname :directory (pathname-directory system))))

#+asdf
(defun namestring-system (system)
  ;; (namestring-system :mon)
  (and (setf system (pathname-system system))
       (namestring system)))

#+asdf
(defun default-directory ()
  (asdf:truenamize (asdf:pathname-directory-pathname *default-pathname-defaults*)))

;; :SOURCE asdf.lisp :WAS `absolute-pathname-p'
(defun pathname-absolute-p (pathspec)
  (and pathspec (eq :absolute (car (pathname-directory (pathname pathspec))))))

;;; ==============================
;; :COURTESY drew mcdermott's ytools/pathname.lisp
;; (defun maybe-ensure-dirs (path-name ensure)
;;   (cond (ensure-if
;; 	 (ensure-directories-exist path-name)))
;;    path-name)


;;; ==============================
;;; :COURTESY Robert Brown's protobuf :WAS `resolve-relative-pathname'
;;; :SEE (URL `https://github.com/brown/protobuf/blob/master/protobuf.asd')
;; (defun pathname-resolve-relative (path parent-path)
;;   "When PATH doesn't have an absolute directory component, treat it as
;; relative to PARENT-PATH."
;;   (let* ((pathname (pathname path))
;;          (directory (pathname-directory pathname)))
;;     (if (and (list directory) (eq (car directory) :absolute))
;;         pathname
;;         (let ((resolved-path (merge-pathnames pathname parent-path)))
;;           (make-pathname :directory (pathname-directory resolved-path)
;;                          :name nil
;;                          :type nil
;;                          :defaults resolved-path)))))


;;; ==============================
;;; :FILE-DIR-DOCUMENTATION
;;; ============================== 

(fundoc 'directorize-namestring
"Return directory-namestring with \"/\"appended.~%~@
:EXAMPLE~%
 \(directorize-namestring
   \(directory-namestring \"/home/me/some/path/to-a/filesys.lisp\"\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc  'file-name-directory
         "Return the directory component in file name FILENAME.~%~@
Return nil if FILENAME does not include a directory.~%~@
Otherwise return a directory name.~%~@
Given a Unix syntax file name, returns a string ending in slash.~%~@
EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'pathname-file-if
        "Is PUTATIVE-PATHNAME `cl:stringp' or `cl:pathnamep' probe it as if by `cl:probe-file'.~%~@
If PUTATIVE-PATHNAME is not of type `mon:pathname-or-namestring' return as if by `cl:values':~%
 nil, \(type-of <PUTATIVE-PATHNAME>\)~%~@
If PUTATIVE-PATHNAME is a file and not a directory return its `cl:truename'.
If keyword ALLOW-DIRECTORY is non-nil and PUTATIVE-PATHNAME names a directory return its `cl:truename'.~%~@
When ALLOW-DIRECTORY is non-nil and PUTATIVE-PATHNAME names a symbolic-link
pointing to a directory the truename of the directory pointed to is returned
_not_ the truename of the symbolic-link.~%~@
:EXAMPLE~%
 \(pathname-file-if 
  \(namestring 
   \(merge-pathnames \(make-pathname :name \".sbclrc\"\) 
                    \(user-homedir-pathname\)\)\)\)~%
 \(pathname-file-if \(namestring \(user-homedir-pathname\)\)\)~%
 \(pathname-file-if \(namestring \(user-homedir-pathname\)\) :allow-directory t\)~%
 \(pathname-file-if #P\"~~/\" :allow-directory t\)~%
 \(pathname-file-if 42\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'pathname-file-list-if 
"Filter any string or pathname in NAMESTRING-LIST for which `pathname-file-if' retrurns null.
Keyword ALLOW-DIRECTORY is as with `pathname-file-if'.~%
Keyword AS-PATHNAMES when non-nil returns each valid string or pathname as if by
`cl:pathname', when nil returns each valid string or pathname as if by
`cl:namestring'. Default is T.~%~@
:EXAMPLE~%
 \(pathname-file-list-if
  \(directory \(merge-pathnames \(make-pathname :name :wild :type :wild\)
                              \(user-homedir-pathname\)\)\)\)~%
 \(pathname-file-list-if
  \(directory \(merge-pathnames \(make-pathname :name :wild :type :wild\)
                              \(user-homedir-pathname\)\)\) :allow-directory t\)~%
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc  'file-truename
"Return the truename of FILENAME.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

;; (function 'file-directory-p
;;   "Return non-nil if filename names an existing directory.~%~@
;; Symbolic links to directories count as directories.
;; See `file-symlink-p' to distinguish symlinks.~%~@
;; :EXAMPLE~%
;;  \(file-directory-p *default-pathname-defaults*\)~%
;;  \(file-directory-p \(merge-pathnames \(make-pathname :name \"bubba\" :type \".lisp\"\)\)\)~%~@
;; :EMACS-LISP-COMPAT~%~@
;; :SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'directory-file-name
"Returns the file name of the directory named directory.~%~@
This is the name of the file that holds the data for the directory directory.~%~@
This operation exists because a directory is also a file, but its name as
a directory is different from its name as a file.~%~@
In Unix-syntax, this function just removes the final slash.~%~@
:EXAMPLE~%
 \(directory-file-name *default-pathname-defaults*\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'to-directory
"Helper function for `mon:expand-file-name'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

#+sbcl
(fundoc 'probe-directory
        "Unlike `cl:probe-file', return false when PATH is a non-directory file.~%~@
Should return non-nil when PATH is a symbolic link to a directory.~%~@
Return as if by `cl:values' one of the following forms:~%
 #P<PUTATIVE-PATHNAME-DIR>, :DIRECTORY, 
 \( {\"<PUTATIVE-PATHNAME-DIR>\" | #P<PUTATIVE-PATHNAME-DIR> } . <PUTATIVE-PATHNAME-DIR>\)~%
 NIL, :SYMLINK, \(#P<PUTATIVE-PATHNAME-DIR> . \(probe-file <PUTATIVE-PATHNAME-DIR>\)\)~%
 NIL, :FILE, #P<PUTATIVE-PATHNAME-DIR>~%
 NIL, \(type-of <PUTATIVE-PATHNAME-DIR>\), <PUTATIVE-PATHNAME-DIR>~%
:EXAMPLE~%
 \(probe-directory \(cl:user-homedir-pathname\)\)~%
 \(probe-directory #P\"~~/\"\)~%
 \(probe-directory \(namestring \(cl:user-homedir-pathname\)\)\)~%
 \(probe-directory \"~~/.sbclrc\"\)~%
 \(probe-directory \"\"\)~%
 \(probe-directory \"   \"\)~%
 \(probe-directory 42\)~%
:SEE-ALSO `mon:pathname-native-file-kind', `file-directory-p', `cl:probe-file',
`cl:ensure-directories-exist'.~%▶▶▶")

(fundoc 'ensure-file-exists
        "Return PATHNAME as if by `cl:open' :direction :probe.~%~@
:EXAMPLE~%
 \(streamp \(ensure-file-exists #P\"/tmp/efe-tmp\"\)\)~%
 \(open-stream-p \(ensure-file-exists #P\"/tmp/efe-tmp\"\)\)~%
 \(let \(\(mp \(make-pathname :directory '\(:absolute \"tmp\"\) :name \"efe-tmp\"\)\)\)
   \(unwind-protect 
        \(with-open-file \(strm \(ensure-file-exists mp\)
                              :direction :output
                              :if-exists :supersede\)
          \(format strm \"existent-file: ~~S~~%with-name: ~~S\" \(cl-fad:file-exists-p strm\) mp\)
          \(format nil \"existent-file: ~~S~~%with-name: ~~S\" \(cl-fad:file-exists-p strm\) mp\)\)
     \(delete-file mp\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'pathname-directory-merged
        "Return `cl:pathname-directory' merging DIRNAME with PATHNAME-DEFAULTS.~%~@
Return valus is a a list designating an absolute directory pathname, e.g.:~%
  \(:absolute <PATHNAME-DEFAULTS> <DIRNAME>\)~%~@
The merged pathname is generated as if by `cl:merge-pathnames' with:~%
 :directory (:relative <DIRNAME>) :defaults <PATHNAME-DEFAULTS>~%~@
:EXAMPLE~%
 \(pathname-directory-merged \"tests\" \(pathname-directory-system :mon\)\)~%
 \(let* \(\(dflts \(pathname-directory-system :mon\)\)
       \(drnm \"tests\"\)
       \(rtn \(cons 
             \(pathname-directory-merged drnm dflts\)
             \(pathname-directory 
              \(merge-pathnames 
               \(make-pathname :directory `\(:relative ,drnm\) :defaults dflts\)
               dflts\)\)\)\)\)
  \(setf rtn `\(,\(equal \(car rtn\) \(cdr rtn\)\) ,@rtn\)\)\)~%~@
:SEE-ALSO `mon:pathname-directory-append', `mon:pathname-absolute-p',
`mon:pathname-components'.~%▶▶▶")

(fundoc 'pathname-directory-append
        "Merge DIRNAME as if by `cl:merge-pathnames' with :defaults PATHNAME-DEFAULTS.~%~@
PATHNAME-DEFAULTS is a pathname desigator of an absolute parent of directory with DIRNAME.~%~@
:EXAMPLE~%
 \(pathname-directory-append \"bubba\" \(user-homedir-pathname\)\)~%
:SEE-ALSO `mon:pathname-directory-merged'.~%▶▶▶")

(fundoc 'rename-file*
"Rename FILE with NEW-NAME.~%~@
Like `rename-file' but avoid a null pathname-type.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:NOTE The HyperSpec says to do \(merge-pathnames new-name file\), which
causes a null \(pathname-type new-name\) to be replaced by \(pathname-type file\),
which is \"almost\" certainly not want one wants.~%~@
:SEE-ALSO `fad:delete-directory-and-files', `replace-file',
`sb-posix:rmdir'.~%▶▶▶")

(fundoc 'unix-dot-directory-p
"Whether PATH is directory of the form \".\" or \"..\".~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'pathname-strip-filespec
"Remove name, type, and version components from PATHNAME.~%~@
Return value is as if by `cl:make-pathname'~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'make-pathname-directory-wildcard
"Return PATHNAME with `cl:pathname-name' and `cl:pathname-type' :wild~%~@
PATHNAME designates a pathname its type should be `mon:filename-designator'.
:EXAMPLE~%
 \(let \(\(ntv \(list \(sb-ext:native-namestring *default-pathname-defaults*\)\)\)\)
   \(setf ntv \(list \(car ntv\) \(sb-ext:native-namestring \(car ntv\) :as-file t\)\)\)
   \(nconc \(list \(make-pathname-directory-wildcard \(car ntv\)\)
                \(make-pathname-directory-wildcard \(cadr ntv\)\)\)
          ntv\)\)~%~@
:NOTE Implementated as a wrapper around `cl-fad::directory-wildcard'.~%~@
:SEE-ALSO `make-pathname-directory-w-type-wild'.~%▶▶▶")

(fundoc 'pathname-directory-pathname
"Return PATHNAME's the pathname of PATHNAMES pathname-directory.~%~@
:EXAMPLE~%
 \(let \(\(ntv \(list \(sb-ext:native-namestring *default-pathname-defaults* :as-file t\)\)\)\)
   \(adjoin \(pathname-directory-pathname \(car ntv\)\) ntv\)\)~%~@
:NOTE A wrapper around `cl-fad:pathname-as-directory'.~%
 ,----
 | Return a pathname reperesenting the given pathname in `directory normal form',
 | i.e. with all the name elements in the directory component and NIL in the name
 | and type components. Can not be used on wild pathnames because there's not
 | portable way to convert wildcards in the name and type into a single directory
 | component. Returns its argument if name and type are both nil or :unspecific.
 `----~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'substitute-in-file-name
"Substitute environment variables referred to in filename.~%~@
`$FOO' where FOO is an environment variable name means to substitute
the value of that variable.~%~@
The variable name should be terminated with a character not a letter, digit or
underscore; otherwise, enclose the entire variable name in braces.~%~@
If `/~~' appears, all of filename through that `/' is discarded.~%~@
If `//' appears, everything up to and including the first of those `/' is discarded.~%~@
:EXAMPLE~%
 \(substitute-in-file-name \"\$DEVHOME/bubba\"\)~%
 \(substitute-in-file-name \"/bubba$\{DEVHOME\}\"\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE \(man \"environ\"\)~%~@
:SEE \(man \"getenv\"\)~%~@
:SEE-ALSO `sb-ext:posix-environ' `sb-ext:posix-getenv'.~%▶▶▶")



(fundoc 'pathname-components
"Return the components of PATHNAME.~%~@
Keyword LIST-OR-PLIST is one of the following:~%
 { :LIST  | :PLIST | :VALUES | NIL | T }~%~@
When keyword LIST-OR-PLIST is  :VALUES, T, NIL \(the default\), return 7
objects as if by `cl:values'. Return values have the format:~%
 ;=> { :PATHNAME-LOGICAL | :PATHNAME-PHYSICAL }      ; nth-value 0
 ;   \( { <STRING> | NIL } . <LOGICAL-HOST-OBJECT> \)  ; nth-value 1
 ;   { <DEVICE> | :UNSPECIFIC }                      ; nth-value 2
 ;   <PATHNAME-DIRECTORY>                            ; nth-value 3
 ;   <PATHNAME-NAME>                                 ; nth-value 4
 ;   <PATHNAME-TYPE>                                 ; nth-value 5
 ;   <PATHNAME-VERSION>                              ; nth-value 6~%~@
When keyword LIST-OR-PLIST is :LIST return as above but as a list of seven elements.~%~@
When keyword LIST-OR-PLIST is :PLIST and keyword PLIST-FOR-BACKQUOTE is nil
return a property list of key/value pairs with the format:~%
 \( :PATHNAME-OR-LOGICAL { :PATHNAME-LOGICAL | :PATHNAME-PHYSICAL }
   :PATHNAME-HOST       \( { <STRING> | NIL } . <LOGICAL-HOST-OBJECT> \)
   :PATHNAME-DEVICE     { <DEVICE> | :UNSPECIFIC }
   :PATHNAME-DIRECTORY  <DIRECTORY>
   :PATHNAME-NAME       <NAME>
   :PATHNAME-TYPE       <TYPE> 
   :PATHNAME-VERSION    <VERSION> \)~%~@
In each of the cases, the nth-value 0, nth 0, or value of first property
evaluates to a symbol indicating whether PATHNAME is a physical-pathname or
`logical-pathname-p'.~%~@
The nth-value 1, nth 1, or value of the second property evaluates to a cons.
If PATHNAME is `mon:logical-pathname-p' the car is a `cl:hostname-string', else NIL. 
Its cdr is an object as returned from `cl:pathname-host'.~%~@
As a special case, when keyword LIST-OR-PLIST is :PLIST and keyword
PLIST-FOR-BACKQUOTE is T return 2 objects as if by `cl:values'. 
Return values have the format:~%
 ;=> \( :HOST      { <STRING> | NIL }
 ;     :DEVICE    { <DEVICE> | :UNSPECIFIC }
 ;     :DIRECTORY <DIRECTORY>
 ;     :NAME      <NAME>
 ;     :TYPE      <TYPE>
 ;     :VERSION   <VERSION> \)                      ; nth-value 0
 ;     { :PATHNAME-LOGICAL | :PATHNAME-PHYSICAL }  ; nth-value 1~%~@
nth-value 0 is a property list of key/value pairs where each key is a valid
argument to `cl:make-pathname'.
nth-value 1 is { :PATHNAME-LOGICAL | :PATHNAME-PHYSICAL }~%~@
:EXAMPLE~%
 \(pathname-components *default-pathname-defaults*\)~%
 \(pathname-components #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\)~%
 \(pathname-components \(translate-logical-pathname #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\)\)~%
 \(pathname-components #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\" :list-or-plist :list\)~%
 \(pathname-components #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\" :list-or-plist :plist\)~%
 \(pathname-components #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\" 
                      :list-or-plist :plist 
                      :plist-for-backquote t\)~%
\(pathname-components (translate-logical-pathname #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\" 
                      :list-or-plist :plist 
                      :plist-for-backquote t\)~%
 \(let* \(\(log-path #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\)
        \(trans-log \(translate-logical-pathname log-path\)\)\)
   \(setf log-path
         \(apply #'make-pathname
                \(pathname-components #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"
                                     :list-or-plist :plist 
                                     :plist-for-backquote t\)\)\)
   \(equal trans-log \(setf trans-log \(translate-logical-pathname trans-log\)\)\)\)~%
 \(multiple-value-bind \(components type\)
      \(pathname-components #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\" 
                           :list-or-plist :plist 
                           :plist-for-backquote t\)
    \(values \(apply #'make-pathname components\)
            type\)\)~%
 \(multiple-value-bind \(phys host dev dir nm typ ver\) 
    \(pathname-components *default-pathname-defaults*\)
  \(list phys host dev dir nm typ ver\)\)~%
 \(equal 
   \(pathname-components *default-pathname-defaults* :list-or-plist :list\)
   \(multiple-value-list \(pathname-components *default-pathname-defaults*\)\)\)~%
 \(getf \(pathname-components *default-pathname-defaults* :list-or-plist :plist\)
      :pathname-or-logical\)~%
 \(eql \(pathname-components #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\) :pathname-logical\)~%
 \(let \(\(logical #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\)
       \(trans-logical '\(\)\)\)
   \(when \(eql \(nth-value 0 \(pathname-components logical\)\) :pathname-logical\)
     \(setf trans-logical \(translate-logical-pathname logical\)\)
     \(list \(pathname-components trans-logical :list-or-plist :list\)
           \(multiple-value-list \(pathname-components logical\)\)\)\)\)~%~@
:NOTE ANSI Section 19.2.2.4.1 \"Restrictions on Examining a Pathname Host Component\":~%
 ,----
 | It is implementation-dependent what object is used to represent the host.
 `---- :SEE \(info \"\(ansicl\)Interpreting Pathname Component Values\"\)~%~@
As such, it can be difficult to distinguish how the following may return:~%
 \(host-namestring #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\)~%
 \(pathname-host #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\)~%~@
:NOTE Per the spec, logical pathnames require characters to be
`cl:upper-case-p'. This does not pose a problem when the output from
`pathname-components' is fed directly back into `cl:make-pathname' b/c the as
the spec indicates w/r/t :case keywords:~%
 ,---- Section 19.2.2.1.2.2 Common Case in Pathname Components:
 | Note that these conventions have been chosen in such a way that
 | translation from :local to :common and back to :local is
 | information-preserving.
 `---- :SEE \(info \"\(ansicl\)Interpreting Pathname Component Values\"\)~%~@
However, as it is not always desirable to roundtrip a logical-pathname in such a
direct manner in situations where PATHNAME is `logical-pathname-p' when
providing the return value of this function as the argument to a pathname
function which accepts the CASE keyword take care to ensure that the case of the
argument is appropriate for the intended caller!~%~@
:SEE-ALSO `pathname-host', `pathname-directory', `pathname-name', `pathname-type',
`pathname-version', `fad:copy-file', `fad:copy-stream', `fad:delete-directory-and-files',
`fad:directory-exists-p', `fad:directory-pathname-p', `fad:file-exists-p',
`fad:list-directory', `fad:pathname-as-directory', `fad:pathname-as-file',
`fad:walk-directory', `fad::component-present-p', `fad::directory-wildcard'.~%▶▶▶")

(fundoc 'file-newer-than-file-p
"Return T if file FILE1 is newer than file FILE2.~%~@
If FILE1 does not exist, return nil; otherwise, if FILE2 does not exist, return T.~%~@
:EXAMPLE~%~@
  { ... <EXAMPLE> ... } ~%~@
:NOTE Non-existent files are assumed to be VERY old.~%~@
:EMACS-LISP-COMPAT A built-in function in :FILE src/fileio.c ~%~@
:SEE-ALSO `file-write-date',`fad:copy-file', `fad:copy-stream',
`fad:delete-directory-and-files', `fad:directory-exists-p',
`fad:directory-pathname-p', `fad:file-exists-p', `fad:list-directory',
`fad:pathname-as-directory', `fad:pathname-as-file', `fad:walk-directory',
`fad::component-present-p', `fad::directory-wildcard'.~%▶▶▶")

(fundoc 'find-file-search-path
 "Return first complete pathname of an existing file.~%~@
Search performed by merging PATHNAME with each item in a SEARCH-PATH in turn.~%~@
Keyword SEARCH-PATH defaults `mon:*search-path*' when that is bound else to
value of `*default-pathname-defaults*'.~%~@
Return nil if an existing file is not found.~%~@
:EXAMPLE~%
 \(find-file-search-path \"types.lisp\"\)~%
 \(find-file-search-path \(make-pathname :name \"types\"  :type \"lisp\"\)\)~%
 \(find-file-search-path \(make-pathname :name \"types\"  :type \"lisp\"\) 
   :search-path \(list \(pathname-system :mon\)\)\)~%~@
:SEE-ALSO `mon:pathname-directory-system', `mon:pathname-system',
`fad:list-directory', `fad:pathname-as-directory', `fad:pathname-as-file',
`fad:walk-directory', `cl:probe-file'.~%▶▶▶")

#+sbcl
(fundoc 'pathname-native-file-kind
"Return the file type of PUTATIVE-PATHNAME.~%~@
Return as if by `cl:values'.~%~@
When PUTATIVE-PATHNAME is `cl:stringp' or `cl:pathnamep', nth-value 0 is as if
by `sb-impl::native-file-kind', nth-value 1 is as per `sb-ext:native-namestring'
its type is is dependent on whether putative-pathname was a string or pathname. 
Returned values have the form:~%
 [ :file | :directory | :symlink | :special ], [ <STRING> | <PATHNAME> ]~%~@
When PUTATIVE-PATHNAME is the empty-string return values have the form:~%
 nil, :string-empty~%~@
When PUTATIVE-PATHNAME is some other type return values have the form:~%
 nil, \(type-of <PUTUTIVE-PATHNAME>\)~%~@
:EXAMPLE~%~@
 \(pathname-native-file-kind \(user-homedir-pathname\)\)
 \(pathname-native-file-kind sb-ext:*CORE-PATHNAME*\)
 \(pathname-native-file-kind \"\"\)
 \(pathname-native-file-kind \"   \"\)
 \(pathname-native-file-kind 42\)~%~@
:SEE-ALSO `sb-ext:native-namestring', `sb-unix:unix-lstat', `sb-unix:unix-stat',
`sb-posix:lstat', `sb-posix:stat'.~%▶▶▶")

#+sbcl 
(fundoc 'remove-directory
"Remove directory specified by PATHNAME.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `sb-posix:rmdir', `file-directory-p', `probe-file'.~%▶▶▶")

(fundoc 'replace-file
  "Like RENAME-FILE, but deletes TO if it exists, first.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `remove-directory', `delete-file-if-exists',
`rename-file', `sb-posix:rmdir', `fad:delete-directory-and-files'.~%▶▶▶")

(fundoc 'directory-parent
"Return the parent directory PATHNAME~%~@
:EXAMPLE~%
 \(directory-parent *default-pathname-defaults*\)~%~@
:SEE-ALSO `fad:copy-file', `fad:copy-stream', `fad:delete-directory-and-files',
`fad:directory-exists-p', `fad:directory-pathname-p', `fad:file-exists-p',
`fad:list-directory', `fad:pathname-as-directory', `fad:pathname-as-file',
`fad:walk-directory', `fad::component-present-p', `fad::directory-wildcard'.~%▶▶▶")

(fundoc 'make-pathname-user-homedir
        "Return a directory pathname for USER with directory components of PATH.~%~@
USER is a string naming a user.~%~@
PATH is a proper-list of strings identifiying directores beneath USER's
`cl:user-homedir-pathname'.~%~@
:EXAMPLE~%
 \(make-pathname-user-homedir :user \"bubba\"\)~%
 \(make-pathname-user-homedir :path '\(\"a\" \"b\"\)\)~%
 \(make-pathname-user-homedir :user \"bubba\" :path '\(\"a\" \"b\"\)\)~%~@
Following each fail successfully:~%
 \(make-pathname-user-homedir :user \"\"\)~%
 \(make-pathname-user-homedir :path '\(\"a\" . \"b\"\)\)~%~@
:SEE-ALSO `cl:make-pathname', `sb-ext:native-namestring', `sb-ext:native-pathname'.~%▶▶▶")

#+asdf
(fundoc 'pathname-directory-system
"Return the `cl:truename' of SYSTEM's asdf pathname, if any.~%~@
Return value is a pathname designator if it is `asdf:system-registered-p' and
its asdf:system object has an effective slot-value for
`asdf::absolute-pathname', else nil.~%~@
:NOTE Unlike `mon:pathname-directory-system-ensure' and `asdf:find-system'
evaluation of thes function does not load an unloaded SYSTEM rather it looks for
SYSTEM's string-name in the hash-table `asdf::*defined-systems*' and then
inspects the asdf:system class object in behind the key.~%~@
:EXAMPLE~%
 \(pathname-directory-system \"cl-ppcre\"\)~%
 \(pathname-directory-system :cl-ppcre\)~%
 \(pathname-directory-system 'cl-ppcre\)~%
 \(pathname-directory-system \"CL-PPCRE\"\)~%
 \(pathname-directory-system \"CL-PPCRE\" t\)~%
 \(pathname-directory-system :CL-PPCRE t\)~%
 \(pathname-directory-system \"not-a-system\"\)~%
 \(pathname-directory-system t\)~%
 \(pathname-directory-system \"\"\)~%
 \(pathname-directory-system nil\)~%~@
:SEE-ALSO .~%▶▶▶")

#+asdf
(fundoc 'pathname-directory-system-ensure
"If SYSTEM is found by `asdf:find-system' return its `cl:pathname-directory' as
the `cl:truename' of its `asdf:system-definition-pathname', else nil.~%~@
:NOTE Evaluation of this function actually loads the system if it isn't already...
:EXAMPLE~%
 \(pathname-directory-system :mon\)~%~@
:SEE-ALSO `mon:pathname-directory-system', `mon:namestring-system',
`mon:pathname-directory-system'.~%▶▶▶")

#+asdf
(fundoc 'pathname-system
"If SYSTEM is found, return its `cl:pathname-directory' as if by `cl:make-pathname'.
SYSTEM is found as if by `mon:pathname-directory-system'.~%~@
:EXAMPLE~%
 \(pathname-system :mon\)~%~@
:SEE-ALSO `mon:namestring-system', `mon:pathname-directory-system-ensure'.~%▶▶▶")

#+asdf
(fundoc 'namestring-system
"If SYSTEM is found, return its `cl:pathname-directory' as if by `cl:namestring'.~%~@
SYSTEM is found as if by `mon:pathname-directory-system'.~%~@
:EXAMPLE~%
 \(namestring-system :mon\)~%~@
:SEE-ALSO `mon:pathname-system', `mon:pathname-directory-system-ensure'.~%▶▶▶")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
