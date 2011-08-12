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
;; #lisp 2011-03-01 
;; re removing directories without following symbolinks:
;; <nikodemus> (cffi:foreign-funcall "system" :string (format nil "rm -rf ~A" dir) :int) ; the dirty option  [15:18]
;; <lichtblau> yay, from symlink problem to space-in-filename problem
;;; ==============================


;;; ==============================
;;
;; :TODO
;; :EMACS-LISP-COMPAT `symbol-file', `locate-library', `process-lines'
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

;;; ==============================                  
;; :COURTESY Nikodemus Siivola
;; :SEE (URL `git://github.com/nikodemus/fsdb.git')
;; :FILE fsdb/src/sbcl.lisp
;; :NOTE `ensure-directory-pathname' retuns a namestring _even if_ PATH names a file.
;;       It is intended to be passed directly to `cl:ensure-directories-exist'
;; #+sbcl
;; (defun ensure-directory-pathname (path)
;;   (sb-ext:parse-native-namestring  
;;    (sb-ext:native-namestring (pathname path) :as-file nil) ;<THING>
;;    nil  ; <HOST>
;;    *default-pathname-defaults* ; <DEFAULTS> 
;;    :as-directory t ;; :START 0 :END nil :JUNK-ALLOWED nil
;;    ))
;;; ==============================
#+sbcl
(defun directory-pathname-ensure (path)
  (declare (pathname-or-namestring path))
  (let* ((path-pathname (pathname path))
         (name (if (wild-pathname-p path-pathname)
                   (file-error-wild-pathname :w-sym         "directory-pathname-ensure" 
                                             :w-type        'function
                                             :pathname       path
                                             :path-arg-locus "PATH"
                                             :signal-or-only nil)
                   ;; osicat-sys:native-namestring <-- cffi-sys:native-namestring <-- sb-ext:native-namestring <-- (funcall (host-unparse-native host) pathname as-file)
                   (sb-ext:native-namestring  path-pathname :as-file nil)))
         (kind (osicat:file-kind name :follow-symlinks t)))
    (unless kind 
      (simple-error-mon :w-sym "directory-pathname-ensure"
                        :w-spec "Arg PATH not regular-file, directory, symlink, or special-file"
                        :w-got path
                        :w-type-of t
                        :signal-or-only  nil))
    (ecase kind
      (:DIRECTORY
       (sb-ext:parse-native-namestring name nil *default-pathname-defaults* :as-directory t))
      ((:FILE :REGUALR-FILE) 
       (values path-pathname (length name))))))

(defun pathname-or-namestring-empty-p (maybe-empty-pathname-or-namestring)
  (declare (inline pathname-or-namestring-p string-empty-p)
           (optimize (speed 3)))
  (unless (pathname-or-namestring-p maybe-empty-pathname-or-namestring)
    (return-from pathname-or-namestring-empty-p nil))
  (locally 
      (declare (pathname-or-namestring maybe-empty-pathname-or-namestring))
    (etypecase maybe-empty-pathname-or-namestring
      (string   (string-empty-p maybe-empty-pathname-or-namestring))
      (pathname (pathname-empty-p maybe-empty-pathname-or-namestring)))))

;;; :SOURCE freedius/lisp/lisp/lisp-io.lisp
;;; :NOTE Following returns true (unix-dot-directory-p "/bubba/.")
(defun unix-dot-directory-p (path)
  (unless (filename-designator-p path)
    (return-from unix-dot-directory-p nil))
  (locally (declare (filename-designator path))
    (let* ((frob-path (if (pathnamep path) 
                          (namestring path)
                          path))
           (rt-side (subseq (the string frob-path) 
                            (1+ (or 
                                 (position #\/ (the string frob-path) :from-end t :test #'char=)
                                 -1)))))
      (or (string= rt-side ".")
          (string= rt-side "..")))))

;;; ==============================
;; :NOTE Should we include the type FILE-STREAM?
;; No, not right now. 
;; Write a `pathname-designator-not-empty-relative-or-wild-p'
;; if/when that is what is wanted. 
;;
;; (when (and (typep maybe-sane-pathname 'file-stream)
;;            (open-stream-p maybe-sane-pathname))
;;   (return-from pathname-or-namestring-not-empty-dotted-or-wild-p t))
;;
(defun pathname-or-namestring-not-empty-dotted-or-wild-p (maybe-sane-pathname &key (no-relatives nil))
  (declare (boolean no-relatives)
           (inline pathname-or-namestring-p)
           (optimize (speed 3)))
  (unless (pathname-or-namestring-p maybe-sane-pathname) ;; filename-designator-p
    (return-from pathname-or-namestring-not-empty-dotted-or-wild-p nil))
  (let ((non-path-string-things (list "" "." ".." " "))) ; the empty string is required for namestrings. 
    (declare (pathname-or-namestring maybe-sane-pathname)
             (list non-path-string-things))
    (when no-relatives 
      (setf non-path-string-things (nconc (list "../" "./") non-path-string-things)))
    #-sbcl (and 
            (not (pathname-or-namestring-empty-p maybe-sane-pathname))
            (not (member maybe-sane-pathname non-path-string-things))
            (not (wild-pathname-p (pathname maybe-sane-pathname)))
            t)
    (etypecase maybe-sane-pathname
      (pathname 
       (setf non-path-string-things (map 'list #'pathname non-path-string-things))
       (and (not (pathname-empty-p maybe-sane-pathname))
            (not (member maybe-sane-pathname non-path-string-things :test #'sb-impl::pathname=))
            (not (wild-pathname-p  maybe-sane-pathname))
            t))
      (string 
       (and 
        (not (member maybe-sane-pathname non-path-string-things :test #'string=))
        (not (wild-pathname-p (pathname maybe-sane-pathname)))
        t)))))

;; :NOTE This is basically a rewrite of `%probe-file-if-string-or-pathname' below.
(defun pathname-not-wild-empty-or-dotted-p (maybe-valid-pathname)
  (declare (inline pathname-or-namestring-p)
           (optimize (speed 3)))
  (unless (pathname-or-namestring-p maybe-valid-pathname)
    (return-from pathname-not-wild-empty-or-dotted-p 
      (values nil (list (type-of (pathname maybe-valid-pathname)) maybe-valid-pathname))))
  (locally (declare (pathname-or-namestring maybe-valid-pathname)) 
    (when (wild-pathname-p maybe-valid-pathname)
      (return-from pathname-not-wild-empty-or-dotted-p 
        (values nil (list :WILD (pathname maybe-valid-pathname)))))
    (let ((dots (list ".." ".")))
      (etypecase maybe-valid-pathname
        (string 
         (if (string-not-empty-or-all-whitespace-p (the string maybe-valid-pathname))
             (if (member (the string maybe-valid-pathname) dots :test 'string=)
                 (values nil (list :STRING-DOTTED    maybe-valid-pathname))
                 (values t   (list :STRING           maybe-valid-pathname)))
             (if (string-empty-p (the string maybe-valid-pathname))
                 (values nil (list :PATHNAME-EMPTY   (make-pathname :defaults maybe-valid-pathname)))
                 (values nil (list :STRING-WHITESPACE maybe-valid-pathname)))))
        (pathname 
         ;; Don't allow the empty pathname to qualify as a pathname.
         ;; we can always recover it with:
         ;; (apply 'make-pathname (%probe-file-if-string-or-pathname #P""))
         (cond ((equal maybe-valid-pathname (make-pathname :defaults ""))
                (values nil (list :PATHNAME-EMPTY      maybe-valid-pathname)))
               ((member maybe-valid-pathname (map 'list #'pathname (the list dots)) :test 'equal)
                (values nil (list :PATHNAME-DOTTED     maybe-valid-pathname)))
               ((every #'whitespace-char-p (the string (namestring maybe-valid-pathname)))
                (values nil (list :PATHNAME-WHITESPACE maybe-valid-pathname)))
               (t (values t (list :PATHNAME            maybe-valid-pathname)))))))))

(declaim (inline %probe-file-if-string-or-pathname))
(defun %probe-file-if-string-or-pathname (putative-pathname) ;; &key (as-pathnames t)
  ;; Is putative-pathname `cl:stringp' or `cl:pathnamep' if so return its `cl:pathname'.
  (unless ;; (or (stringp putative-pathname) (pathnamep putative-pathname)) 
      ;;(pathname-or-namestring-p maybe-valid-pathname))
      (filename-designator-p putative-pathname)
    (return-from %probe-file-if-string-or-pathname
      (values nil (type-of putative-pathname))))
  (locally 
      (declare (pathname-or-namestring putative-pathname))
    ;; (let ((nrmlz-to-pathname (pathname putative-pathname)))
    ;; (declare (pathname nrmlz-to-pathname))
    (etypecase putative-pathname
      (string (if (string-not-empty-or-all-whitespace-p putative-pathname)
                  (values (pathname putative-pathname) :STRING)
                  (values nil                          :STRING-EMPTY)))
      (pathname 
       ;; Don't allow the empty pathname to qualify as a pathname.
       ;; we can always recover it with:
       ;; (apply 'make-pathname (%probe-file-if-string-or-pathname #P""))
       (if  (equal putative-pathname (make-pathname))             
            (values nil               :PATHNAME-EMPTY)
            (values putative-pathname :PATHNAME))))))

(defun pathname-native-file-kind (putative-pathname &key (error-on-wild nil)) ;; &key (as-pathnames t)
  (declare (inline %probe-file-if-string-or-pathname)
           (boolean error-on-wild)
           (optimize (speed 3)))
  (let* ((pathname-chk 
          (multiple-value-bind (pnfk-chk pnfk-typ) (%probe-file-if-string-or-pathname putative-pathname)
            (if pnfk-chk 
                (if (wild-pathname-p pnfk-chk)
                    (if error-on-wild
                        (file-error-wild-pathname :w-sym         "pathname-native-file-kind" 
                                                  :w-type        'function
                                                  :pathname       putative-pathname
                                                  :path-arg-locus "PUTATIVE-PATHNAME"
                                                  :signal-or-only nil)
                        (return-from pathname-native-file-kind (values nil (list :WILD pnfk-chk))))
                    (cons pnfk-chk pnfk-typ))
                ;; If we've wound up here we return either:
                ;;  (:STRING-EMTPY  "") | (:PATHNAME-EMPTY #P"")
                ;; :NOTE There is no "kind" for an emtpy string or an emtpy path, e.g.:
                ;;  (eq (sb-impl::native-file-kind "") (%probe-file-if-string-or-pathname ""))
                ;;  (eq (sb-impl::native-file-kind "") (%probe-file-if-string-or-pathname #P""))
                ;; In either case, we can recover the pathname with:
                ;;  (multiple-value-bind (kind path) (pathname-native-file-kind "")
                ;;    (if (null kind ) (make-pathname) path))
                ;; (return-from pathname-native-file-kind (values pnfk-chk pnfk-typ))
                (case pnfk-typ
                  ((:PATHNAME-EMPTY :STRING-EMPTY) 
                   (return-from pathname-native-file-kind (values pnfk-chk (list pnfk-typ putative-pathname))))
                  (t (return-from pathname-native-file-kind (values pnfk-chk pnfk-typ)))))))
         (pathname-namestring-if 
          (osicat-sys:native-namestring (the pathname (car pathname-chk)))))
    (declare (cons pathname-chk)
             (string pathname-namestring-if))
    (values 
     (osicat:file-kind pathname-namestring-if)
     (ecase (cdr pathname-chk)
       (:STRING   pathname-namestring-if)
       (:PATHNAME (pathname pathname-namestring-if))))))

;;; ==============================
;;; :NOTE In the followings ecause clauses, the first value is as per return
;;; value of `sb-impl::native-file-kind' -- Any remaining values are as per
;;; return value of return value of `osicat:file-kind'. Hopefully this arangment
;;; may allow us to switch which we rely on as needed...
(defun probe-directory (putative-pathname-dir)
  (let* ((pathname-chk 
          (multiple-value-list (pathname-native-file-kind putative-pathname-dir)))
         (pathtype-chk   (car pathname-chk)))
    (ecase pathtype-chk
      ;; :FIXME NIL is a corner case for :WILD, :PATHNAME-EMPTY, :STRING-EMPTY, etc.
      ;; specialize with case around caadr of pathname-chk.
      ;; Or, better yet m-v-b instead of let binding above and CL:CASE inspect
      ;; the m-v-b'd 1 value instead.
      ((nil)    (values pathtype-chk (cadr pathname-chk) putative-pathname-dir))
      ((:REGULAR-FILE :FILE)
       (values nil pathtype-chk (cadr pathname-chk)))
      ((:SOCKET :BLOCK-DEVICE :CHARACTER-DEVICE :SPECIAL) 
       (values nil pathtype-chk (cadr pathname-chk)))
       ((:SYMBOLIC-LINK :SYMLINK)
        ;; :TODO Now that we're using osicat:file-kind we can rexamine whether the
        ;;       link is borken or not by re-examining the file and checking for
        ;;       symbolic-link-broken.
        (let ((probed (probe-file  (cadr pathname-chk))))
          (values nil pathtype-chk (cons (pathname (cadr pathname-chk)) probed))))
       (:DIRECTORY 
        (values (truename (cadr pathname-chk)) 
                pathtype-chk 
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
    ;; #-sbcl
    ;; (when (setf pathname-chk (probe-file pathname-if))
    ;;   (locally (declare (pathname pathname-chk))
    ;;     (if allow-directory
    ;;         pathname-chk
    ;;         (when (not (equal pathname-chk
    ;;                           (make-pathname :directory (pathname-directory pathname-chk))))
    ;;           pathname-chk))))
    ;; #+sbcl 
    (multiple-value-bind (path-type path-if) (pathname-native-file-kind pathname-if)
      (ecase path-type
        ;; NIL is a corner case for :WILD, :PATHNAME-EMPTY, :STRING-EMPTY, etc.
        ;; we keep it separate from :symlink :special for clarity
        ((nil) nil)
        ((:SYMBOLIC-LINK :SOCKET :BLOCK-DEVICE :CHARACTER-DEVICE  ; osicat:file-kind
          :SYMLINK :SPECIAL)                                      ; sb-impl::native-file-kind
         nil)
        ((:REGULAR-FILE :FILE)
         (truename path-if))
        (:DIRECTORY (when allow-directory 
                      (truename path-if)))))))

(defun pathname-file-list-if (namestring-list &key allow-directory (as-pathnames t))
  (declare (boolean as-pathnames))
  (flet ((filter-files (filename)
           (pathname-file-if filename :allow-directory allow-directory)))
    (let ((filtered (remove-if-not #'filter-files namestring-list)))
      (declare (list filtered))
      (if as-pathnames
          (loop for path in filtered collect (pathname path))
          (loop for path in filtered collect (namestring path))))))

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

;;; ==============================
;; (fundoc 'subfile
;; "Return a file pathname with name SUB in DIRECTORY-PATHNAME.
;; MAKE-PATHNAME-KEYWORDS are passed to MAKE-PATHNAME.  When DIRECTORY-PATHNAME is
;; NIL, it is interpreted to be cl:*default-pathname-defaults*.
;; :EXAMPLE~%~@
;;  { ... <EXAMPLE> ... } ~%~@
;; :SEE-ALSO `<XREF>'.~%▶▶▶")
;;
;; (defun subfile (directory-pathname sub &rest make-pathname-keywords)
;;   (merge-pathnames (apply #'make-pathname 
;;                           :directory `(:relative ,@(butlast sub)) 
;;                           :name (alexandria:lastcar sub)
;;                           make-pathname-keywords)
;;                    (or directory-pathname *default-pathname-defaults*)))
;;; ==============================

;;; ==============================
;; (defun append-slash (path)
;;   "append / to path if there is none at the end"
;;   (if (char= (car (last (coerce path 'list))) #\/)
;;       (setf path (concatenate 'string path "/")))
;;   path)
;;
;;; :COURTESY buildapp-1.1/utils.lisp :WAS `directorize'
(defun directorize-namestring (namestring)
  (declare (filename-designator path))
  (concatenate 'string (string-right-trim "/" (namestring namestring) "/")))

;;; :COURTESY freedius/lisp/lisp/lisp-io.lisp
(defun rename-file* (file new-name)
  (declare (filename-designator file new-name))
  (flet ((wild-error (path locus)
           (file-error-wild-pathname :w-sym         "replace-file" 
                                     :w-type        'function
                                     :pathname       path
                                     :path-arg-locus locus
                                     :signal-or-only nil)))
    (when (wild-pathname-p file)
      (wild-error file "FILE"))
    (when (wild-pathname-p new-name)
      (wild-error new-name "new-name")))
  (if (pathname-type new-name)
      (rename-file file new-name)   
      (rename-file file (make-pathname :defaults new-name :type :UNSPECIFIC))))

;;; :SOURCE quicklisp/quicklisp/utils.lisp
(defun replace-file (from to)
  (declare (type filename-designator from to))
  (flet ((wild-error (path locus)
           (file-error-wild-pathname :w-sym         "replace-file" 
                                     :w-type        'function
                                     :pathname       path
                                     :path-arg-locus locus
                                     :signal-or-only nil)))
    (when (wild-pathname-p from)
      (wild-error from "FROM"))
    (when (wild-pathname-p to)
      (wild-error from "FROM")))
  (when (probe-file to)
    (delete-file to))
  (rename-file from to))


;;; ==============================
;; :TODO This should be more careful about the empty string and `cl:wild-pathname-p'
;;
;; :TODO This should disallow linking directories (esp. when HARD is t) 
;; The logic being, if we want to hardlink a directory we shoul do it that
;; _hard_ way to ensure a higher level of awareness w/r/t the potential negative
;; consequences!
;;
;; :TODO use osicat:make-link instead
(defun make-symlink (&key target link-name (hard nil))
  (declare (boolean hard)
           (pathname-or-namestring target link-name))
  ;;
  ;; (osicat:make-link :target target link-name :hard hard)
  ;;
  #-(or (and sbcl (not win32))  ecl ccl) (error "`make-symlink' not-implemented") ;; (and clisp unix)
  #+(and sbcl (not win32)) 
  (if hard 
      (sb-posix:link     target link-name)
      (sb-posix:symlink  target link-name))
  ;; 
  ;; I don't find this on Clisp 2.49
  ;; #+(and clisp unix) (linux:symlink link-name add-symlink-at)
  ;;
  ;; :NOTE We assume the GNU longopts are in play when ECL is.
  #+ecl (ext:run-program "/bin/ln" (if hard 
                                       (list (namestring target) (namestring link-name))
                                       (list "--symbolic"  (namestring target) (namestring link-name)))
                         :wait t :input nil :output nil :error nil)
  ;;
  ;; According to oGMo on #lisp as of 10.5.8 GNU longopts are not accepted.
  #+ccl (ccl:run-program "/bin/ln" 
                         (if hard 
                             (list (namestring target) (namestring link-name))
                             (list "-s"  (namestring target) (namestring link-name)))
                         :wait t :input nil :output nil :error nil))

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

;: :TODO This should be more careful about deleting directories when
;; pathname-to-delete is a symlink in file form.
(defun delete-file-if-exists (pathname-to-delete)
  (declare (type pathname-designator pathname-to-delete))
  (when (or (streamp pathname-to-delete) 
            (wild-pathname-p pathname-to-delete))
    ;; `cl:delete-file' won't delete a directory and signals an error. 
    ;; we should catch it before it has a chance.
    ;; (cl-fad:directory-pathname-p #P"./bubba/") *default-pathname-defaults*)
    (let ((wrn (format nil 
                       "~%~T:FUNCTION `delete-file-if-exists' --~%~12T~
                       declining to delete PATHNAME, got: ~S" 
                       pathname-to-delete
                       ;; (or (and (streamp pathname) 
                       ;;          (string 'cl:streamp) 
                       ;;          (string 'cl:wild-pathname-p))
                       ;;     *default-pathname-defaults*)
                       )))
      (warn wrn)
      (return-from delete-file-if-exists (values nil wrn))))
  (when (probe-file pathname-to-delete)
    (delete-file pathname-to-delete)))

;; :SOURCE buildapp-1.1/utils.lisp
;; :NOTE differs from `cl-fad:copy-file'
;; :NOTE what about an :external-format?
(defun copy-file (input output &key (if-exists :supersede))
  ;; (declare (type pathname-designator input outuput))  
  (with-open-file (input-stream input)
    (with-open-file (output-stream output
				   :direction :output
                                   :if-exists if-exists)
      (loop
	 for char = (read-char input-stream nil)
	 while char 
         do (write-char char output-stream)))))

;; :SOURCE mcclim/Apps/Listener/util.lisp :WAS `strip-filespec'
(defun pathname-strip-filespec (pathname)
  ;; (declare (type pathname-designator pathname)) ;; don't bother with a file-stream!
  (declare (filename-designator pathname))
  (make-pathname :name nil
                 :type nil
                 :version nil
		 #+scl :query #+scl nil
		 :defaults (pathname pathname)))

(defun directory-parent (of-pathname)
  (declare (filename-designator of-pathname))
  (let ((of-pathath (pathname of-pathname)))
    (declare (pathname of-path))
    (make-pathname :host   (pathname-host of-path)
                   :device (pathname-device of-path)
                   ;; :NOTE consider using `osicat::component-present-p'
                   :directory (if (and (pathname-name of-path)
                                       (not (eq :unspecific (pathname-name of-path))))
                                  (pathname-directory of-path)
                                  (butlast (pathname-directory of-path))))))

;; :SOURCE cl-docutils-20101006-git/utilities.lisp :WAS `find-file'
(defun find-file-search-path (search-file &key (search-path (or *search-path* (list *default-pathname-defaults*))))
  (declare (filename-designator pathname))
  (let ((chk-search-file (if (pathname-not-wild-empty-or-dotted-p search-file)
                             (pathname search-file)
                             (file-error-wild-pathname :w-sym          "find-file-search-path"
                                                       :w-type         'function
                                                       :pathname        search-file
                                                       :path-arg-locus "search-file"
                                                       :signal-or-only  nil)))
        ;; :NOTE (and (not (wild-pathname-p (sb-ext:posix-getenv "PATH"))) "$PATH is ok man")
        ;; However, this one is likely to cause loads of _FUN_ later:
        ;;  (pathname-directory (sb-ext:posix-getenv "PATH"))
        (chk-search-path
         (if (pathname-not-wild-empty-or-dotted-p search-path)
             ;;
             ;; We get a namestring to allow for wackiness like this:
             ;;  (pathname (sb-ext:posix-getenv "PATH"))
             ;; In which case we the pathname needs to be split into components.
             ;;
             (osicat-sys:native-namestring search-path)
             (file-error-wild-pathname :w-sym          "find-file-search-path"
                                       :w-type         'function
                                       :pathname        search-path
                                       :path-arg-locus "search-path"
                                       :signal-or-only  nil))))
    (flet ((find-fl (ffsp-dir)
             (some #'probe-file 
                   #-sbcl (directory (merge-pathnames chk-search-file ffsp-dir))
                   #+sbcl (directory (merge-pathnames chk-search-file ffsp-dir) :resolve-symlinks nil))))
      ;; (declare (pathname-or-namestring chk-search-path))
      (some #'(lambda (w-path)
                (etypecase w-path
                  (list     (some #'find-fl w-path))
                  (string   (some #'find-fl (string-split-on-chars w-path ":")))
                  (pathname (find-fl w-path))))
            chk-search-path))))

;; :NOTE As of 2011-08-11 when directory is "." or ".."  return value
;; of cl-fad:list-directory and osicat:list-directory differ there is a
;; bug in osicat:call-with-directory-iterator that weirdly binds
;; *d-p-d* via osicat:absolute-pathname e.g. 
;;  (osicat:absolute-pathname ".")
;; returns the equivalent of: 
;;  (merge-pathnames "." *default-pathname-defaults*)
;; We manage to avoid this with osicat:file-exists-p but it is worth keeping in mind!
(defun directory-files (directory &key (bare-pathnames nil)) ;; &optional full match nosort)
  (declare (filename-designator directory)
           (boolean bare-pathnames))
  (let ((dir-pathname 
         (if wild-pathname-p
             (file-error-wild-pathname :w-sym         "replace-file" 
                                       :w-type        'function
                                       :pathname       directory
                                       :path-arg-locus "directory"
                                       :signal-or-only nil)
             (pathname directory))))
    (declare (pathname dir-pathname))
    (and (setf dir-pathname
               ;; (or (cl-fad:directory-exists-p (mon::pathname-as-directory dir-pathname)) ;; :error-on-empty t))
               (or (nth-value 0 (osicat:file-exists-p (mon::pathname-as-directory dir-pathname) :directory))
                   ;; :NOTE The idea behind returning values was to allow further
                   ;; processing for the (non-existent) FULL MATCH args and or to allow restarts. 
                   ;; If we aren't going to provide restarts around this it would
                   ;; prob. be better to just signal an error.
                   (return-from directory-files 
                     (values dir-pathname (osicat:file-exists-p dir-pathname :directory)))))
         (osicat:list-directory dir-pathname :bare-pathnames bare-pathnames))))

(defun directory-unfiltered-p (directory-name &key (ignorables *default-pathname-directory-ignorables* supplied-p)
                               (test 'string=))
  (declare (pathname-or-namestring directory-name)
           ((and cons list) ignorables)
           (optimize (speed 3)))
  ;; Lets just assume that the default value of
  ;; *default-pathname-directory-ignorables* is sanely bound...
  ;;
  ;; Would it be better to make ignorables as class instance and run the
  ;; following in an after method on it?
  (when supplied-p
    (unless (every #'stringp ignorables)
      (simple-error-mon :w-sym "directory-unfiltered-p" 
                        :w-type 'function
                        :w-spec "Element of IGNORABLES not `cl:stringp'~%IGORABLES: ~S"
                        :w-args (list ignorables)
                        :w-got  (find-if-not #'stringp ignorables)
                        :w-type-of t)))
  (labels ((member-frob (chk-member) 
             (declare (string chk-member))
             (not (member chk-member ignorables :test test)))
           (psn-slash (maybe-slash)
             (declare (string maybe-slash))
             (position #\/ maybe-slash))
           (token-component-p (maybe-component-only)
             (when (and (stringp maybe-component-only)
                        (not (wild-pathname-p (the string maybe-component-only))))
               (when (or (string-empty-p (the string maybe-component-only))
                         (member (the string maybe-component-only) (list ".." ".") :test 'string=))
                 (multiple-value-bind (pnfk-0 pnfk-1) (pathname-native-file-kind maybe-component-only)
                   (ecase pnfk-0
                     ((nil) (return-from directory-unfiltered-p (values pnfk-0 pnfk-1)))
                     (:DIRECTORY (return-from directory-unfiltered-p (values nil (list pnfk-0 pnfk-1)))))))
               (not (psn-slash (the string maybe-component-only)))))
           (full-frob (frob-component)
             (when (token-component-p frob-component)
               ;; We bail now, but we pack the list so that even if
               ;; `member-frob' returns T the return values of
               ;; `directory-unfiltered-p' will not be easily coercible to a
               ;; real pathname e.g. callers will get either:
               ;;  T,   (:STRING <DIRECTORY-NAME>)
               ;;  NIL, (:STRING <DIRECTORY-NAME>)
               (return-from full-frob (list frob-component (list :STRING frob-component))))
             (multiple-value-bind (key-or-null path-string-or-type) (pathname-native-file-kind directory-name)
               (case key-or-null
                 ((:FILE :REGULAR-FILE)
                  (return-from directory-unfiltered-p 
                    (values t (list key-or-null (pathname path-string-or-type)))))
                 (:DIRECTORY 
                  ;; :FIXME This is not quite right yet b/c we don't want to traverse symlinks... 
                  ;; If a dir is passed as a native filename then `pathname-native-file-kind' should identify it as a symlink.
                  ;; To accommodate not following we need to requery with
                  ;; (directory <DIR> :resolve-symlinks nil) and compare the
                  ;; output if they are equal we have a real dir, else we have a
                  ;; symlink to an existing directory and we should put
                  ;; path-string-or-type in pathname form and return now with either:
                  ;;  (NIL (:SYMLINK #P"/pathnae/of/symlink"))
                  ;;  (NIL (:SYMLINK #P"/pathnae/of/symlink/"))
                  ;;
                  (let* ((dir-path (car (directory path-string-or-type)))
                         (dir-compt (last-elt (pathname-directory dir-path))))
                    (list dir-compt (list :DIRECTORY dir-path))))
                 ((nil)
                  (cond 
                    ((listp path-string-or-type)
                     (case (car path-string-or-type)
                       ;; ""   -> NIL, (:STRING-EMPTY "")
                       ;; #P"" -> NIL, (:PATHNAME-EMPTY #P"") 
                       ((:STRING-EMPTY :PATHNAME-EMPTY)
                        (return-from directory-unfiltered-p (values key-or-null path-string-or-type)))
                       ;; "weird-and-wild/*"  -> NIL, (:WILD #P"weird-and-wild/*")
                       (:WILD  (return-from directory-unfiltered-p (values key-or-null path-string-or-type)))))                    
                    ;; "bogus-string-component/" -> NIL, (:STRING "bogus-string-component/")
                    ((and (stringp path-string-or-type) (psn-slash path-string-or-type))
                     (return-from directory-unfiltered-p  (values key-or-null (list :STRING path-string-or-type))))
                    ;; #P"bogus-and-weird-path/" -> NIL, (:RELATIVE "bogus-and-weird-path")
                    ((pathnamep path-string-or-type) 
                     (return-from directory-unfiltered-p (values key-or-null (pathname-directory path-string-or-type))))                    
                    ;; ??weird an unknown?? -> NIL, (NIL ??weird an unknown??)
                    (t (return-from directory-unfiltered-p 
                         (values key-or-null (list NIL path-string-or-type))))))
                 ;; "/some/special" | #P"/some/special"  -> (NIL (:SPECIAL #P"/some/special"))
                 ((:SYMLINK :SPECIAL    ; sb-impl::native-file-kind
                   :SYMBOLIC-LINK :SOCKET :BLOCK-DEVICE :CHARACTER-DEVICE) ; osicat:file-kind                   
                  (return-from directory-unfiltered-p 
                    (values nil (list key-or-null (pathname path-string-or-type)))))
                 ;; it may be a string component
                 (t (if (stringp path-string-or-type)
                        (if (not (token-component-p path-string-or-type))
                            (return-from directory-unfiltered-p (values nil (list :STRING path-string-or-type)))
                            (list path-string-or-type (list :STRING path-string-or-type)))
                        ;; who fu**ing knows what it is...
                        ;; (return-from directory-unfiltered-p (values nil (list :WTF key-or-null path-string-or-type)))
                        (return-from directory-unfiltered-p (values nil (list key-or-null path-string-or-type )))))))))
    ;; (full-frob directory-name)))
    (let* ((dir-part-if (full-frob directory-name))
           (chk-if (if (stringp (car dir-part-if))
                       (car dir-part-if)
                       (return-from directory-unfiltered-p (values nil (cdr dir-part-if))))))
      (declare (string chk-if))
      (values (member-frob chk-if) (cadr dir-part-if)))))

;; WORKS!
;; (ignorables *default-pathname-directory-ignorables* supplied-p) (test 'string=))
;; (print
(defun tt--gather-dir-list (dir) ;; (test 'string=) (ignorables *default-pathname-directory-ignorables* supplied-p)
  ;; gather list of files and directories in dir filtering them with `mon:directory-unfiltered-p'.
  ;; directories are included in the result we do not descend into any directory that satsifies #'directory-unfiltered-p
  ;; 
  ;; (tt--gather-dir-list *default-pathname-defaults*)
  (let ((gthr-dir-and-files '()))
    (flet ((partition-file-or-dir (dir-file-or-other)
             (multiple-value-bind (n0-t-or-nil n1-dir-file-or-other-type) (directory-unfiltered-p  dir-file-or-other)
               (case n1-dir-file-or-other-type
                 ((:DIRECTORY :FILE :REGULAR-FILE)
                  (push n1-dir-file-or-other-type gthr-dir-and-files)
                  n0-t-or-nil)
                 (t (print n1-dir-file-or-other-type) n0-t-or-nil)))))
      ;; (osicat:walk-directory
      (cl-fad:walk-directory dir
                             #'constantly ; all work done with test fncn PARTITION-FILE-OR-DIR
                             :test #'partition-file-or-dir
                             :directories :breadth-first
                             ;; :if-does-not-exist 
                             )
      gthr-dir-and-files)))

;;; ==============================
;; :PASTED (URL `http://paste.lisp.org/+2N64')
;; :NOTE I don't find CL-FAD:PATHNAME-AS-FILE particularly sane w/r/t the empty string.
;; It isn't portable and it doesn't fail in obvious ways.
;; following is an attempt at fixing that.
;; :NOTE osicat:pathname-as-file is nearly 1:1 identical with
;; cl-fad:pathname-as-file for Osicat distributed with Quicklisp
;; osicat-20110619-git/src/osicat.lisp and also accepts the empty string.
(defun pathname-as-file (pathspec &key (error-on-empty nil))
  (declare (pathname-or-namestring pathspec)
           (optimize (speed 3)))
  (let ((paf-name (pathname pathspec)))
    (declare (pathname paf-name))
    (when (wild-pathname-p paf-name)
      (file-error-wild-pathname :w-sym         "pathname-as-file" 
                                :w-type        'function
                                :pathname       pathspec
                                :path-arg-locus "PATHSPEC"
                                :signal-or-only nil))
    (when (zerop (length (the simple-string (namestring paf-name))))
      (if error-on-empty 
          (error "cl:namestring of PATHSPEC evaluates to the emtpy string")
          (return-from pathname-as-file (make-pathname :defaults pathspec))))
    ;;;;;;
    ;; (cond ((cl-fad:directory-pathname-p pathspec)
    ;;        (let* ((directory (pathname-directory pathname))
    ;;               (name-and-type (pathname (first (last directory)))))
    ;;          (make-pathname :directory (butlast directory)
    ;;                         :name (pathname-name name-and-type)
    ;;                         :type (pathname-type name-and-type)
    ;;                         :defaults pathname)))
    ;;       (t pathname))
    ;;;;;
    (osicat:pathname-as-file paf-name)))

;; Like cl-fad:pathname-as-directory but more careful about the empty string.
;; :NOTE osicat:pathname-as-directory is nearly 1:1 identical with
;; cl-fad:pathname-as-directory for Osicat distributed with Quicklisp
;; osicat-20110619-git/src/osicat.lisp and also accepts the empty string.
(defun pathname-as-directory (pathspec &key (error-on-empty nil))
  (declare (pathname-or-namestring pathspec)
           (optimize (speed 3)))
  (let ((pdp-name (pathname pathspec)))
    (declare (pathname pdp-name))
    (when (wild-pathname-p pdp-name)
      (file-error-wild-pathname :w-sym         "pathname-as-directory" 
                                :w-type        'function
                                :pathname       pathspec
                                :path-arg-locus "PATHSPEC"
                                :signal-or-only nil))
    (when (zerop (length (the simple-string (namestring pdp-name))))
      (if error-on-empty 
          (error "cl:namestring of PATHSPEC evaluates to the emtpy string")
          (return-from pathname-as-directory (make-pathname :name nil :type nil :defaults pathspec))))
    (osicat:pathname-as-directory pdp-name)))
;; 
;; :FIXME Verify this is correct.
(defun pathname-directory-pathname (pathspec) ;; &key (error-on-empty nil))
  (let ((ensure-pathname (pathname pathspec)))
    (make-pathname :name nil :type nil :defaults ensure-pathname)))

(defun make-pathname-user-homedir (&key user path)
  (declare (string-or-null user)
           (proper-list path))
  #-sbcl (check-type user string-or-null)
  #-sbcl (check-type path proper-list)
  (if user
      (if (string-not-empty-or-all-whitespace-p user)
          (pathname (osicat-sys:native-namestring (make-pathname :directory `(:absolute :home ,user ,@path))))
          (simple-error-mon :w-sym "make-pathname-user-homedir"
                            :w-type 'function
                            :w-spec "Keyword USER did not satisfy `mon:string-not-empty-or-all-whitespace-p'"
                            :w-got user
                            :w-type-of t
                            :signal-or-only nil))
      (pathname 
       (osicat-sys:native-namestring
        (make-pathname :directory `(,@(pathname-directory (user-homedir-pathname)) ,@path))))))


;; :NOTE This is a slightly tweaked cl-fad:directory-wildcard b/c there is no
;; osicat:directory-wildcard. Returns WILDEN-PATHNAME with pathname-name and pathname-type :wild
(defun make-pathname-directory-wildcard (wilden-pathname)
  (declare (filename-designator wilden-pathname))
  (when (wild-pathname-p wilden-pathname)
    (file-error-wild-pathname :w-sym         "make-pathname-directory-wildcard" 
                              :w-type        'function
                              :pathname       wilden-pathname
                              :path-arg-locus "WILDEN-PATHNAME"
                              :signal-or-only nil))
  (make-pathname :name #-:cormanlisp :wild #+:cormanlisp "*"
                 :type #-(or :clisp :cormanlisp) :wild
                 #+:clisp nil
                 #+:cormanlisp "*"
                 :defaults (pathname-as-directory wilden-pathname)))

(defun make-pathname-directory-w-type-wild (base-pathname pathname-name)
  (declare (filename-designator base-pathname)
           (string-or-null pathname-name))
  (unless (and (or (string-null-or-empty-p base-pathname)
                   (string-null-or-empty-p pathname-name)
                   (not 
                    (setf base-pathname    
                          (and (setf base-pathname (mon::pathname-as-directory base-pathname))
                               ;; :WAS (or  (cl-fad:directory-exists-p base-pathname)
                               (or (osicat:file-exists-p base-pathname :directory)
                                   (return-from make-pathname-directory-w-type-wild 
                                     (values base-pathname (wild-pathname-p base-pathname))))))))
               (values base-pathname (wild-pathname-p base-pathname)))
    (setf base-pathname
	  (merge-pathnames 
	   (make-pathname :name pathname-name :type :wild )
	   base-pathname))
    (values base-pathname (wild-pathname-p base-pathname))))

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
		  ;; sb-ext:posix-getenv sb-posix:getenv osicat-posix:getenv
                  ;; (var-val (sb-ext:posix-getenv var-name)))
		  (var-val (osicat-posix:getenv var-name)))
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

(defun remove-directory (pathname-or-namestring)
  (declare (pathname-or-namestring pathname-or-namestring)
           (optimize (speed 3)))
  ;; (defsyscall "rmdir" :int (path filename-designator))
  #-sbcl (osicat-posix:rmdir pathname-or-namestring)
  ;; (define-call "rename" int minusp (oldpath filename) (newpath filename))
  #+sbcl (sb-posix:rmdir (pathname pathname-or-namestring)))


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
;;; :DEPRECATED
;;; ==============================

;;; :NOTE Delete once expand-file-name is finished.
(defun to-directory (path)
  ;; 
  (if (char= #\/ (schar path (1- (length path))))
      path
      (concatenate 'string path "/")))
;;
(defun expand-file-name (name &optional (default-dir *default-pathname-defaults*))
  (namestring (merge-pathnames (to-directory default-dir) name)))

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

;; (fundoc 'to-directory
;; "Helper function for `mon:expand-file-name'.~%~@
;; :EXAMPLE~%~@
;;  { ... <EXAMPLE> ... } ~%~@
;; :SEE-ALSO `<XREF>'.~%▶▶▶")

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

(fundoc 'make-pathname-directory-w-type-wild
        "Return BASE-PATHNAME merged with PATHNAME-NAME with its `cl:pathname-type' wild.~%~@
Return value is as if by `cl:values'.~%~@
BASE-PATHNAME is a pathname-or-namestring designating a directory.~%~@
PATHNAME-NAME is of type mon:string-or-null.~%~@
:EXAMPLE~%
 \(make-pathname-directory-w-type-wild *default-pathname-defaults* \"lisp\"\)~%
:SEE-ALSO `<XREF>'.~%▶▶▶")

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
"Return WILDEN-PATHNAME with `cl:pathname-name' and `cl:pathname-type' :wild~%~@
WILDEN-PATHNAME designates a pathname its type should be `mon:filename-designator'.~%~@
An error is signaled if WILDEN-PATHNAME is already `cl:wild-pathname-p'.~%~@
:EXAMPLE~%
 \(let \(\(ntv \(list \(sb-ext:native-namestring *default-pathname-defaults*\)\)\)\)
   \(setf ntv \(list \(car ntv\) \(sb-ext:native-namestring \(car ntv\) :as-file t\)\)\)
   \(nconc \(list \(make-pathname-directory-wildcard \(car ntv\)\)
                \(make-pathname-directory-wildcard \(cadr ntv\)\)\)
          ntv\)\)~%~@
:NOTE Implementated as a wrapper around `cl-fad::directory-wildcard'.~%~@
:SEE-ALSO `make-pathname-directory-w-type-wild'.~%▶▶▶")

(fundoc 'pathname-directory-pathname
        "Return the pathname of PATHNAME'S `cl:pathname-directory'.~%~@
When PATHNAME is not in directory normal form, e.g. its a file and lacks a
trailing #\\/ return value is the parent of PATHNAME.~%~@
:EXAMPLE~%
 \(let \(\(ntv \(list \(pathname \(sb-ext:native-namestring *default-pathname-defaults* :as-file t\)\)\)\)\)
   \(list :ORIGINAL *default-pathname-defaults*
         :FROBBED \(adjoin \(pathname-directory-pathname \(car ntv\)\) ntv\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'pathname-as-file
        "Like `cl-fad:pathname-as-directory' but more careful about the empty string.~%~@
Converts the non-wild pathname designator PATHSPEC to file form.~%~@
Keyword ERROR-ON-EMPTY when non-nil indicates that when the namestring of
PATHSPEC is evaluates to an empty-string it should be returned as if by:
 \(make-pathname :defaults pathspec\)
When keyword ERROR-ON-EMPTY is T and PATHSPEC is evaluates to the empty-string
an error is signaled.

:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'pathname-as-directory
 "Like `cl-fad:pathname-as-directory' but filtered through `mon:pathname-directory-pathname'.~%~@
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
`pathname-version'.~%▶▶▶")

(fundoc 'file-newer-than-file-p
"Return T if file FILE1 is newer than file FILE2.~%~@
If FILE1 does not exist, return nil; otherwise, if FILE2 does not exist, return T.~%~@
:EXAMPLE~%~@
  { ... <EXAMPLE> ... } ~%~@
:NOTE Non-existent files are assumed to be VERY old.~%~@
:EMACS-LISP-COMPAT A built-in function in :FILE src/fileio.c ~%~@
:SEE-ALSO `file-write-date'.~%▶▶▶")

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
:SEE-ALSO `mon:pathname-directory-system', `mon:pathname-system', `cl:probe-file'.~%▶▶▶")

(fundoc 'directory-unfiltered-p
        "Return a boolean indicating if DIRECTORY-NAME is a `cl:member' of IGNORABLES list.~%~@
DIRECTORY-NAME should be of type `mon:pathname-or-namestring'.
It is pre-processed with `mon:pathname-native-file-kind' to determine if it
designates a regular-file, directory, symlink, special-file, wild-pathname,
emtpy-pathname, or empty-string.
Only regular-files and directories recieve further treatment all others immediately return NIL.
If DIRECTORY-NAME  designates a file return T.
If DIRECTORY-NAME designates a directory it is reduced to a single
directory-component as if by:
 (last-elt (pathname-directory <DIRECTORY-NAME>))
IGNORABLES is a list of strings naming a directories which should be filtered/ignored.
Default is `mon:*default-pathname-defaults*'.
An error is signalled if IGNORABLES is null or is contained of any element
which is not `cl:stringp'.~%~@
Useful as a test function for `cl-fad:walk-directory'.~%~@
:EXAMPLE~%
 \(directory-unfiltered-p \".BZR\" :test 'string=\)~%
 \(directory-unfiltered-p \".BZR\" :test 'string-equal\)~%
 \(directory-unfiltered-p \"bobba\" :ignorables '\(\"bubba\" \"bobby\" \"not-bobby\"\)\)~%
 \(equal \(multiple-value-list \(directory-unfiltered-p \"mon-systems\"\)\) '\(T \(:STRING \"mon-systems\"\)\)\)~%
 \(equal \(multiple-value-list \(directory-unfiltered-p \"mon-systems/*\"\)\)  '\(NIL \(:WILD #P\"mon-systems/*\"\)\)\)~%
 \(equal \(multiple-value-list \(directory-unfiltered-p #P\"mon-systems/*\"\)\) '\(NIL \(:WILD #P\"mon-systems/*\"\)\)\)~%
 \(equal \(multiple-value-list \(directory-unfiltered-p #P\"mon-systems/\"\)\) '\(NIL \(:RELATIVE \"mon-systems\"\)\)\)~%
 \(equal \(multiple-value-list \(directory-unfiltered-p \"mon-systems/\"\)\) '\(NIL \(:STRING \"mon-systems/\"\)\)\)~%
 \(equal \(multiple-value-list  \(directory-unfiltered-p #P\"*\"\)\) '\(NIL \(:WILD #P\"*\"\)\)\)~%
 \(equal \(multiple-value-list \(directory-unfiltered-p #P\"\"\)\) '\(NIL \(:PATHNAME-EMPTY #P\"\"\)\)\)~%
 \(equal \(multiple-value-list \(directory-unfiltered-p \"..\"\)\) '\(NIL \(:DIRECTORY \"..\"\)\)\)~%
 \(equal \(multiple-value-list \(directory-unfiltered-p \".\"\)\) '\(NIL \(:DIRECTORY \".\"\)\)\)~%
 \(equal \(multiple-value-list \(directory-unfiltered-p \"\"\)\)  '\(NIL \(:STRING-EMPTY \"\"\)\)\)~%
  hard to check:~%
 \(equal \(multiple-value-list \(directory-unfiltered-p \"/dev/dm-12\"\)\) '\(NIL \(:SPECIAL #P\"/dev/dm-12\"\)\)\)~%
 \(equal \(multiple-value-list \(directory-unfiltered-p #P\"/dev/dm-12\"\)\) '\(NIL \(:SPECIAL #P\"/dev/dm-12\"\)\)\)~%~@
;; Following signal an error:~%
 \(directory-unfiltered-p #P\"bubba\" :ignorables '\(\"bubba\" \"bobby\" 8 \"a\"\)\)~%
 \(directory-unfiltered-p \"bubba\" :ignorables nil\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")


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
Keyword ERROR-ON-WILD is a boolean when T indicates that 
when PUTATIVE-PATHNAME is `cl:wild-pathname-p' an error is signaled.
Default is to return:~%
 nil, (:WILD <PUTATIVE-PATHNAME>)~%~@
:EXAMPLE~%
 \(pathname-native-file-kind \(user-homedir-pathname\)\)~%
 \(pathname-native-file-kind sb-ext:*CORE-PATHNAME*\)~%
 \(pathname-native-file-kind \"\"\)~%
 \(pathname-native-file-kind \"   \"\)~%
 \(pathname-native-file-kind 42\)~%
 \(pathname-native-file-kind   #P\"/.*\"\)~%
;; Following successfully signals an error:~%
 \(pathname-native-file-kind   #P\"/.*\" :error-on-wild t \)~%
:SEE-ALSO `sb-ext:native-namestring', `sb-unix:unix-lstat', `sb-unix:unix-stat',
`sb-posix:lstat', `sb-posix:stat'.~%▶▶▶")


#+sbcl
(fundoc 'directory-pathname-ensure
"Return PATH ensuring that if it is a directory ends in a trailing slash.~%~@
Return two values as if by cl:values:~%
 - cl:nth-value 0 is the pathname of path. If it is a directory it will end in a #\\/~%
 - cl:nth-value 1 is the length of the path as if by sb-ext:parse-native-namestring~%~@
An error is signaled if PATH is not of type `mon:pathname-or-namestring'.~%~@
An error is signaled if PATH is cl:wild-pathname-p.~%~@
An error is signaled if PATH identifies a symbolic-link or special file.~%~@
:EXAMPLE~%
 \(directory-pathname-ensure *default-pathname-defaults*\)~%
 \(directory-pathname-ensure \(string-right-trim  '\(#\\\\/\) \(namestring *default-pathname-defaults*\)\)\)~%
 \(let \(\(tmp-file \(make-pathname :directory '\(:absolute \"tmp\"\) :name \"eg-file\"\)\)
       \(got-values '\(\)\)\)
   \(unwind-protect
        \(progn
          \(with-open-file \(s tmp-file :direction :output :if-exists :supersede\)
            \(write \"bubba\" :stream s\)\)
          \(push \(multiple-value-list \(directory-pathname-ensure tmp-file\)\) got-values\)
          \(push \(multiple-value-list \(directory-pathname-ensure \(directory-namestring tmp-file\)\)\) got-values \)
          \(push \(multiple-value-list \(directory-pathname-ensure \(string-right-trim  '\(#\\/\)\(directory-namestring tmp-file\)\)\)\) got-values\)\)
     \(delete-file tmp-file\)\)
   \(setf got-values \(nreverse got-values\)\)\)~%~@
:SEE-ALSO `sb-ext:native-namestring', `sb-ext:parse-native-namestring'.~%▶▶▶")

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

;; (defun make-symlink (&key target link-name (hard nil))
;; :NOTE osicat:make-link returns the created symlink on success, sb-posix:symlink returns 0
;; When the link to be created already exists
;; osicat:make-link returns a condition object: 
;;   #<EEXIST OSICAT-POSIX:SYMLINK 17 :EEXIST ""> 
;; SBCL sb-posix:symlink returns:
;;  System call error 17 (File exists) SB-POSIX:SYSCALL-ERROR 
(fundoc 'make-symlink
        "Make a link from TARGET to LINK-NAME. ~%~@
Return 0 on success, :WARNING do not rely on return value it is subject to change!
Keyword TARGET and LINKNAME are of type `mon:pathname-or-namestring'.
Keyword TARGET names an existing file which becomes the target of the
symbolic link  created at the destination LINK-NAME.
Keyword HARD is a boolean, when non-nil a hardlink is created as if by link\(2\).
Default is nil in which case the link is a symbolic link created as if by symlink\(2\)
equivalent to one of the following:~%
 shell> ln -s <TARGET> <LINK-NAME>~%
 shell> ln  <TARGET> <LINK-NAME>~%
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE \(man \"symlink\(2\)\"\)
:SEE \(man \"symlink\(7\)\"\)
:SEE \(man \"link\(2\)\"\)
:SEE \(man \"link\(1\)\"\)
:SEE \(man \"ln\"\)
:SEE \(info \"\(coreutils\) ln invocation\"\)~%~@
:SEE-ALSO `sb-posix:symlink', `sb-posix:link'.~%▶▶▶")

(fundoc 'directory-parent
"Return the parent directory PATHNAME~%~@
:EXAMPLE~%
 \(directory-parent *default-pathname-defaults*\)~%~@
:SEE-ALSO .~%▶▶▶")

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

(fundoc 'pathname-or-namestring-empty-p
"Return boolaen indicating wheether MAYBE-EMPTY-PATHNAME-OR-NAMESTRING is or is not.~%~@
Return T if it is `mon:string-empty-p' or `mon:pathname-empty-p'.~%
:EXAMPLE~%
 \(pathname-or-namestring-empty-p \(make-pathname\)\)~%
 \(pathname-or-namestring-empty-p #P\"\"\)~%
 \(pathname-or-namestring-empty-p \"\"\)~%
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'pathname-or-namestring-not-empty-dotted-or-wild-p
        "Whether MAYBE-SANE-PATHNAME is of a sane value.~%~@
Return a boolean value. When return value is T the following properties are true
of MAYBE-SANE-PATHNAME:~%~@
 - is of type `mon:pathname-or-namestring'~%
 - is not `cl:wild-pathname-p'~%
 - is not equal any of the following:
    \"\" \".\" \"..\" \" \" #P\"\" #P\".\" #P\"..\" #P\" \"~%~@
Keyword NO-RELATIVES is a boolean, when non-nil also return NIL if
MAYBE-SANE-PATHNAME is any of the following pathnames or namestrings:~%
 #P\".\" #P\"..\" #P\" \"~%
:EXAMPLE~%
 \(let \(\(possibilities '\(\"\" \".\" \"..\" \" \"  #P\"\" #P\".\" #P\"..\" #P\" \" 
                        \"*\" \"*.\" \"*.*\" #P\"*\" #P\"*.\" #P\"*.*\"
                        \"./\" \"../\" #P\"./\" #P\"../\"\)\)\)
   \(list 
    \(map 'list #'pathname-or-namestring-not-empty-dotted-or-wild-p possibilities\)
    \(map 'list #'\(lambda \(x\) 
                   \(pathname-or-namestring-not-empty-dotted-or-wild-p x :no-relatives t\)\)
         possibilities\)\)\)~%~@
:NOTE Has regression test in :FILE mon-systems/test/testing.lisp~%
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'pathname-not-wild-empty-or-dotted-p
"Return as if by `cl:values' whether MAYBE-VALID-PATHNAME is a reasonable namestring or pathname.~%~@
 - nth-value 0 is a boolean, T if MAYBE-VALID-PATHNAME is a namestring or pathname.~%~@
 - nth-value 1 is a list of the form:~%
   \( :<KEYED-TYPE> <VALUE>\)~%
  :KEYED-TYPE is one of:~%
    :WILD               :PATHNAME-EMPTY
    :STRING             :PATHNAME 
    :STRING-DOTTED      :PATHNAME-DOTTED
    :STRING-WHITESPACE  :PATHNAME-WHITESPACE~%~@
:EXAMPLE~%
 \(pathname-not-wild-empty-or-dotted-p \"/some/valid-path/designator\"\)
 ; => T, \(:STRING \"/some/valid-path/designator\"\)~%
 \(pathname-not-wild-empty-or-dotted-p #P\"/some/valid-path/designator\"\)
 ; => T, \(:PATHNAME #P\"/some/valid-path/designator\"\)~%   
 \(pathname-not-wild-empty-or-dotted-p #P\"\"\)
 ; => NIL, \(:PATHNAME-EMPTY #P\"\"\)~%
 \(pathname-not-wild-empty-or-dotted-p #P\"*\"\)
 ; => NIL, \(:WILD #P\"*\"\)~%
 \(pathname-not-wild-empty-or-dotted-p \"*.*\"\)
 ; => NIL, \(:WILD #P\"*.*\"\)~%
 \(pathname-not-wild-empty-or-dotted-p \".\"\)
 ; => NIL, \(:STRING-DOTTED \".\"\)~%
 \(pathname-not-wild-empty-or-dotted-p \"..\"\)
 ; => NIL, \(:STRING-DOTTED \"..\"\)~%
 \(pathname-not-wild-empty-or-dotted-p #P\".\"\)
 ; => NIL, \(:PATHNAME-DOTTED #P\".\"\)~%
 \(pathname-not-wild-empty-or-dotted-p #P\"..\"\)
 ; => NIL, \(:PATHNAME-DOTTED #P\"..\"\)~%
 \(pathname-not-wild-empty-or-dotted-p \(pathname \(concatenate 'string *whitespace-chars*\)\)\)
 ; => NIL, \(:PATHNAME-WHITESPACE #P\"  ... whitespace-chars-here ... \"\)~%
 \(pathname-not-wild-empty-or-dotted-p \(concatenate 'string *whitespace-chars*\)\)
 ; => NIL, \(:STRING-WHITESPACE #P\" ... whitespace-chars-here ... \"\)~%
:SEE `mon-test:pathname-not-wild-empty-or-dotted-p-TEST' :FILE mon-systems/test/testing.lisp~%
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'delete-file-if-exists
        "Delete PATHNAME-TO-DELETE if it exists.~%~@
If PATHNAME-TO-DELETE is `cl:wild-pathname-p' or `cl:streamp' and error is signaled.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'directory-files 
"Return a list of the files contained of DIRECTORY.~%~@
DIRECTORY is a non-wild pathname-or-namestring designating an existing directory.~%~@
Signal an error if pathname is `wild-pathname-p'.~%~@
Return value is as if by values if DIRECTORY does not satisfy `osicat:directory-exists-p'.
If BARE-PATHNAMES is non-NIL only the files's bare pathnames are returned
\(with an empty directory component), otherwise the files' pathnames are
merged with DIRECTORY.~%~@
:EXAMPLE~%
 \(directory-files *default-pathname-defaults*\)~%
 \(directory-files
  \(merge-pathnames 
   \(make-pathname :directory '\(:relative \"bubba\"\)\)
   *default-pathname-defaults*\)\)~%
;; Following successfully signals an error:~%
 \(directory-files 
  \(merge-pathnames \(make-pathname :name :wild :type :wild\) *default-pathname-defaults*\)\)~%~@
:EMACS-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")



;;; ==============================
;;; :SB-IMPL-NOTES
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
;; sb-impl::pathname=
;; sb-impl::*logical-hosts*
;; sb-impl::*physical-host*
;; sb-impl::*win32-host*
;; sb-impl::*unix-host*
;; sb-int:*load-source-default-type*
;; sb-impl::*short-site-name* sb-impl::short-site-name
;; sb-impl::*long-site-name*  sb-impl::long-site-name
;; sb-impl::*fasl-file-type*
;; sb-impl::*features*
;; sb-impl::unix-host-unparse-directory-separator sb-impl::*unix-host*
;; sb-impl::unix-host-customary-case sb-impl::*unix-host*
;;
;;; ==============================


;;; ==============================
;; `mon:logical-hosts'
;; lisp-implementation-type
;; lisp-implementation-version
;;
;; asdf:*default-source-registries*
;; asdf:*central-registry*
;; 
;; sb-ext::machine-type
;; sb-ext::machine-version
;; sb-ext::machine-instance

;; sb-sys::software-type
;; sb-sys::software-version
;; sb-ext:*posix-argv*
;; sb-sys::get-machine-version
;;
;; (sb-unix:unix-getrusage sb-unix:rusage_self)
;;
;;
;;; ==============================
;; (osicat-posix:getpagesize)
;; (osicat-posix:getdomainname)
;; (osicat-posix:gethostname)
;; (osicat-posix:getenv <STRING>) ;; string should be simple
;; (osicat:environment-variable <NAME>)
;; (osicat:environment)
;;
;;; ==============================


;;; ==============================
;; :SB-POSIX NOTES
;;; ==============================
;; 
;; :SEE info node (info "(libc)Temporary Files")
;;
;; sb-unix:unix-rename
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
;;; ==============================


;;; ==============================
;; sb-int:c-strings->string-list 
;; sb-ext:parse-native-namestring
;; sb-ext:native-namestring
;; sb-ext:native-pathname
;; (sb-impl::map-directory 
;;; ==============================


;;; ==============================
;; regarding NATIVE-NAMESTRING's stolen from cffi/src
;;
;; cmucl's native-namestring (ext:unix-namestring pathname nil)
;; openmcl  (ccl::native-translated-namestring pathname)
;; scieneer (ext:unix-namestring pathname nil)
;; sbcl     (sb-ext:native-namestring 
;; osicat-sys:native-namestring <-- cffi-sys:native-namestring <-- sb-ext:native-namestring
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
;;
;; asdf::*user-cache*
;; asdf::*system-cache*
;; asdf::*output-translations*
;; asdf::*default-output-translations*
;; asdf::*output-translations-file*
;; asdf::*output-translations-directory*
;;
;; asdf::*default-source-registry-exclusions*
;; asdf::*source-registry-exclusions*
;; asdf::*source-registry*
;; asdf::*central-registry*
;; *default-source-registries*
;; *source-registry-file*
;; *source-registry-directory*
;; asdf:disable-output-translations
;;
;;; ==============================



;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
