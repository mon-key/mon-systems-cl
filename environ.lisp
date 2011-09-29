;;; :FILE-CREATED <Timestamp: #{2011-05-07T16:24:06-04:00Z}#{11186} - by MON>
;;; :FILE environ.lisp
;;; ==============================

;; CL:MACHINE-TYPE 
;; CL:MACHINE-VERSION
;; CL:MACHINE-INSTANCE what a name for a symbol... 
;; We have (make-pathname :host {...} ) not (make-pathname :machine-instance {...} )
;;
;; (osicat-posix:gethostname)


(in-package #:mon)
;; *package*

;; :SORUCE git://git.feelingofgreen.ru/executor :FILE executor/portable-spawn.lisp
(defun setenv (variable value)
  ;;
  ;; #+osicat (setf (osicat:environment-variable variable) value)
  ;;
  ;; :NOTE putenv on sbcl unix is unsetenv && setenv
  #+sbcl
  (sb-posix:putenv (concatenate 'string variable "=" value))
  #+ccl
  (ccl:setenv variable value t)
  #+ecl
  (si:setenv variable value)
  #+clisp
  (setf (ext:getenv variable) value))

(defun getenv-path-pathnames ()
  ;; Return the pathnmames of directories for current environment varbiable $PATH.~%~@
  ;;:EXAMPLE~%
  ;;  \(getenv-path-pathnames\)
  ;; 
  (let* ((env-path  #+sbcl (sb-posix:getenv "PATH")
                    #-sbcl (osicat:environment-variable "PATH"))
         (env-split (and env-path (cl-ppcre:split ":" env-path))))
    (when env-split
      (setf env-split
            (delete-if #'(lambda (maybe-null-or-wild)
                           (or (null maybe-null-or-wild)
                               (wild-pathname-p maybe-null-or-wild))) env-split))
      (when env-split
        (map 'list #'mon:pathname-as-directory env-split)))))

#+sbcl
(defun executable-find (command &key (skip-alias     t)
                                     (skip-functions t)
                                     (skip-dot       t)
                                     (skip-tilde     t))
  (declare (string command)
           (boolean skip-alias skip-functions skip-dot skip-tilde))
  (unless (and (sb-posix:getenv "HOME") ; (osicat-posix:getenv "HOME")
               (not (string= "" command)))
    (return-from executable-find 
      (values nil (cons :exit-status "ENOHOME"))))
  (let ((which-args (delete-if #'null (list (and skip-tilde     "--skip-tilde")
                                            (and skip-functions "--skip-functions")
                                            (and skip-dot       "--skip-dot")
                                            (and skip-alias     "--skip-alias")
                                            command)))
        (out-string (make-string-output-stream))
        (got-path   nil)
        (status     nil))
    (unwind-protect
         (progn 
           (setf status (sb-ext:process-exit-code 
                         (sb-ext:run-program "which" which-args :output out-string :search t)))
           (unless (zerop status)
             (close out-string)
             (return-from executable-find (values nil (cons :exit-status status))))
           (setf got-path (get-output-stream-string out-string)))
      (close out-string))
    (when got-path 
      (setf got-path (string-right-trim *whitespace-chars* got-path))
      (let ((length-got      (length got-path))
            (length-exec     (length command))
            (ensure-got-path (search command got-path :from-end t)))
        (and ensure-got-path 
             (= (- length-got ensure-got-path) length-exec)
             (values (namestring (truename got-path))
                     (cons command got-path)))))))

;;; ==============================
;; :TODO verify if ccl:native-translated-namestring is the way to go here.
;; :SEE (URL `http://clozure.com/pipermail/openmcl-devel/2011-May/012812.html')
;; :SEE (URL `http://trac.clozure.com/ccl/ticket/632')
;; #+ccl   (ccl::%chdir (ccl:native-translated-namestring to-directory))
;; ,----
;; | #lisp 2011-07-27
;; | <rme> mon_key: ccl:current-directory returns the lisp's current directory (in
;; |       the operating system sense) as a pathname.  It can also be used with
;; |       setf to change the current directory.
;; `----
;; So, presumably for CCL would we could/should do:
;;  (setf (ccl:current-directory) (namestring to-directory))
;;
;; :NOTE The implementation equivalences are courtesy :FILE pergamum/feet-of-clay.lisp
;; :SEE (URL `git://git.feelingofgreen.ru/pergamum') 
(defun set-posix-working-directory (to-directory &key (return-as-pathname nil))
  (declare (boolean return-as-pathname))
  ;; :NOTE Clisp accepts NIL and "" for arg TO-DIRECTORY is not good IMHO.
  ;; SBCL doesn't and errors. We go with SBCL and bail early.
  (when (or (null to-directory) 
            (string-empty-p to-directory)
            (wild-pathname-p to-directory))
    (error ":FUNCTION `set-posix-working-directory' -- arg TO-DIRECTORY not valid~%~Tgot: ~S~%~Ttype-of: ~S~%"
           to-directory (type-of to-directory)))
  (values
   (zerop
    #-(or sbcl ecl ccl clisp) (osicat-posix:chdir to-directory)
    #+ecl   (si:chdir pathname)
    #+ccl   (ccl::%chdir (namestring to-directory))
    #+clisp (if (null to-directory) (ext:cd to-directory) 0)
    #+sbcl  (sb-posix:chdir to-directory))
   (or 
    (and return-as-pathname 
         (pathname to-directory))
    to-directory)))

;; :NOTE The implementation equivalences are courtesy :FILE pergamum/feet-of-clay.lisp
;; :SEE (URL `git://git.feelingofgreen.ru/pergamum') 
(defun posix-working-directory ()
  #-(or sbcl ecl ccl clisp) (osicat-posix::getcwd)
  #+ecl   (si:getcwd)
  #+ccl   (ccl::current-directory-name)
  #+clisp (ext:cd)
  #+sbcl  (sb-posix:getcwd))

(defun (setf posix-working-directory) (to-directory)
  (multiple-value-bind (bool set-pth)
      (set-posix-working-directory to-directory)
    (values set-pth bool)))

#+sbcl
(defun syslog-action (&key 
                      (log-message "<EMPTY-SBCL-LOG-MESSAGE>")
                      (log-ident "sbcl")
                      (log-priority 6)) ;; 6 indicates informational
  (declare (string log-message log-ident)
           ((mod 8) log-priority))
  (unwind-protect
       (progn
         (sb-posix:openlog log-ident 
                           (or 
                            (and (string-equal log-ident "sbcl") 
                                 sb-posix:log-pid)
                            0)
                           sb-posix:log-user)
         (sb-posix:syslog log-priority log-message))
    (sb-posix:closelog)))

(defun %syslog-action-osicat (&key 
                              (log-message "<EMPTY-SBCL-LOG-MESSAGE>")
                              (log-ident "sbcl")
                              (log-priority 6)) ;; 6 indicates informational
  ;; (%syslog-action-osicat)
  ;; (%syslog-action-osicat :log-message "A different log message")
  ;; (%syslog-action-osicat :log-message "A different log message"  :log-ident "bubba")
  (declare (string log-message log-ident)
           ((mod 8) log-priority))
  (unwind-protect
       (progn
         (osicat-posix:openlog log-ident 
                               (or 
                                (and (string-equal log-ident "sbcl")
                                     osicat-posix:log-pid)
                                0)
                               :user)
         (osicat-posix:syslog log-priority log-message))
    (osicat-posix:closelog)))


;;; :SOURCE de.setf.utility/pathnames.lisp
#+sbcl
(defun logical-hosts ()
  ;; (where-is "*logical-hosts*")
  (when (hash-table-p (and SB-IMPL::*LOGICAL-HOSTS*))
    (loop
       for host being each hash-key of SB-IMPL::*LOGICAL-HOSTS*
       collect host)))

(defun lisp-implementation-description (&optional stream)
  #-sbcl (format stream "Lisp Implementation: ~A ~A~%~%" 
                 (lisp-implementation-type) 
                 (lisp-implementation-version))
  #+sbcl (format stream "Lisp Implementation: ~A ~A~%~
                  Lisp Runtime Pathname: ~S~%~
                  Lisp Core Pathname: ~S~%~
                  Lisp File Types: ~S, ~S~%~
                  Lisp Default External Format: ~S~%~
                  Host Machine Spec: ~A ~A~%~
                  Host Machine Instance: ~A~%~
                  Host OS Type: ~A~%~
                  Host OS Implementation: ~A~%~
                  Host Directory Separator: ~S~%~
                  Host Customary Case: ~S~%"
                 (lisp-implementation-type)
                 (lisp-implementation-version)
                 sb-ext:*runtime-pathname* ;; sb-ext:*POSIX-ARGV* 
                 sb-ext:*core-pathname*
                 sb-int:*load-source-default-type*
                 sb-impl::*fasl-file-type*
                 sb-impl::*default-external-format*
                 (sb-ext::machine-type)
                 (sb-ext::machine-version) ;; (sb-sys::get-machine-version)
                 (sb-ext::machine-instance)
                 (sb-sys::software-type)
                 (sb-sys::software-version)
                 (sb-impl::unix-host-unparse-directory-separator sb-impl::*unix-host*)
                 (sb-impl::unix-host-customary-case sb-impl::*unix-host*)
                 ))

;; (sb-unix: (sb-unix:UNIX-GETUID)
;; (sb-unix:UID-USERNAME (sb-unix:UNIX-GETUID))
;; (sb-posix:GETUID)
;; (sb-posix:GETEUID)
;; sb-posix:OPENDIR

;; (osicat-posix:getpwnam <NAME>)
;; (osicat-posix::funcall-getpw #'osicat-posix::%getpwnam-r <NAME>)
;;
;; (osicat-posix:getpwnam "<NAME>")
;; => <NAME>, <PASSWD>, <UID>, <GID>, <GECOS>, <DIR>, <SHELL>
;;
;; (let ((verify-getpwnam 
;;        (sb-posix:getpwnam 
;;         (elt (pathname-directory (namestring (user-homedir-pathname))) 2))))
;;   (values-list 
;;    (list  
;;     (sb-posix:passwd-name   verify-getpwnam)
;;     (sb-posix:passwd-passwd verify-getpwnam)
;;     (sb-posix:passwd-uid    verify-getpwnam)
;;     (sb-posix:passwd-gid    verify-getpwnam)
;;     (sb-posix:passwd-gecos  verify-getpwnam)
;;     (sb-posix:passwd-dir    verify-getpwnam)
;;     (sb-posix:passwd-shell  verify-getpwnam))))
; => <NAME>, <PASSWD>, <UID>, <GID>, <GECOS>, <DIR>, <SHELL>


;;; ==============================
;; :NOTE `sb-ext::machine-type', `sb-ext::machine-version', `sb-ext::machine-instance'
(defun username-for-system-var-p (verify-with)
  #-sbcl(and (format t "~%:FUNCTION `username-for-system-var-p' -- Current implementation not SBCL~%~
                    Declining further verification of argument VERIFY-WITH~%") nil)
  ;; 
  ;; osicat-posix::passwd 
  #+sbcl
  (if (find-package "SB-POSIX")
      (let* ((verify-nm verify-with)
             (verify-home (elt (pathname-directory (namestring (user-homedir-pathname))) 2))
             ;; #+osicat  (verify-getpwnam (osicat-posix:getpwnam (car verify-nm)))
             (verify-getpwnam (sb-posix:getpwnam (car verify-nm))))
        (and 
         ;; #+osicat (string-equal (or verify-getpwnam "") (car verify-nm))
         (string-equal (or (and verify-getpwnam (sb-posix:passwd-name verify-getpwnam)) "") (car verify-nm))
         (string-equal verify-home  (car verify-nm))
         verify-nm))
      (and (format t 
                   "~%:FUNCTION `username-for-system-var-p' -- Current implementation is SBCL~%~
                    need SB-POSIX:PASSWD-NAME but did not find-package SB-POSIX")
           nil)))

;; ;; :NOTE (translate-logical-pathname "MON:MON-SYSTEMS;")
(defun username-for-system-var-bind (bind-it)
  (declare (special bind-it))
  (let ((val-of (symbol-value bind-it))
        (msg-if ":FUNCTION `username-for-system-var-bind' value of arg BIND-IT "))
    (setf msg-if
          (with-output-to-string (msg)
            (typecase val-of
              (null (format msg "~%~Anull at loadtime~% ~
                                    arg: ~S~%" msg-if bind-it))
              (cons (format msg "~%~Aalready bound~% ~
                                    arg: ~S~% ~
                                    binding: ~S~%" msg-if bind-it val-of))
              (pathname 
               (with-input-file (unm val-of)
                 (let ((rd-nm-pair (read unm)))
                   (or (and rd-nm-pair 
                            (consp rd-nm-pair)
                            (or (and (username-for-system-var-p rd-nm-pair)
                                     (progn (setf (symbol-value bind-it) rd-nm-pair)
                                            (format msg "~%~Anow bound~% ~
                                                   arg: ~S~% ~
                                                   binding:~% ~S~%" msg-if bind-it rd-nm-pair)
                                            t))
                                (progn (format msg "~%~A not bound~% ~
                                                      failed to satisfy `username-for-system-var-p' ~% ~
                                                      arg: ~S~% ~
                                                      failed-with:~% ~S~%" msg-if bind-it rd-nm-pair)
                                       t)))
                       (format msg "~%~A not bound file empty or value not `cl:consp'~% ~
                                       arg: ~S~% ~
                                       got: ~S~%" msg-if bind-it rd-nm-pair)))))
              (t (format msg "~%~Ais neither `cl:consp' nor `cl:pathnamep'~% ~
                                 arg: ~S~% ~
                                 got: ~S~% ~
                                 type-of: ~S~%" msg-if bind-it val-of (type-of val-of))))
            msg))
    (setf val-of (symbol-value  bind-it))
    (or (and (consp val-of)
             (prog1 val-of (princ msg-if *standard-output*)))
        (prog1 nil (princ msg-if *standard-output*)))))


;;; ==============================
;;; :DOCUMENTATION
;;; ==============================

(fundoc 'setenv 
        "Set the value of the environment VARIABLE named variable to VALUE.~%
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `sb-posix:putenv', `sb-posix:getenv'.~%▶▶▶")

(fundoc 'username-for-system-var-p
"Verify value of consed pair VERIFY-WITH before setting value of `mon:*user-name*'.~%~@
Return VERIFY-WITH if its car matches `sb-posix:passwd-name' and the final
directory component of `cl:user-homedir-pathname'.~%~@
Evaluated at system loadtime by `mon:username-for-system-var-bind'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `sb-posix:getpwnam', `sb-posix:passwd-name', `sb-ext::machine-type',
`sb-ext::machine-version', `sb-ext::machine-instance', `sb-ext:posix-environ',
`cl:user-homedir-pathname'.~%▶▶▶")

(fundoc 'username-for-system-var-bind
        "Bind and return symbol-value of BIND-IT if `mon:username-for-system-var-p' returns non-nil.~%~@
If BIND-IT is not bound retrun NIL and print a message to *standard-output*.~%~@
Evaluated at system loadtime with value 
:EXAMPLE~%~@
 \(setf *user-name* \(probe-file 
                   \(merge-pathnames 
                    \(make-pathname :name \"loadtime-bind\"\) 
                    *default-pathname-defaults*\)\)\)~%
 \(username-for-system-var-bind '*user-name*\)~%~@
:SEE-ALSO `sb-posix:getpwnam', `sb-posix:passwd-name', `sb-ext:posix-environ',
`sb-ext::machine-type', `sb-ext::machine-version', `sb-ext::machine-instance',
`cl:user-homedir-pathname'.~%▶▶▶")

(fundoc 'lisp-implementation-description
"Return string describing the current lisp implementation environment~%~@
:EXAMPLE~%
 \(lisp-implementation-description\)~%
 \(lisp-implementation-description t\)~%
:SEE-ALSO `cl:lisp-implementation-type', `cl:lisp-implementation-version',
`sb-ext:*runtime-pathname*', `sb-ext:*posix-argv*', `sb-ext:*core-pathname*',
`sb-ext::machine-type', `sb-ext::machine-version', `sb-ext::machine-instance',
`sb-sys::get-machine-version', `sb-sys::software-type',
`sb-sys::software-version', `sb-int:*load-source-default-type*',
`sb-impl::unix-host-unparse-directory-separator', `sb-impl::*fasl-file-type*',
`sb-impl::*default-external-format*', `sb-impl::unix-host-customary-case',
`sb-impl::*unix-host*', `sb-impl::short-site-name', `sb-impl::long-site-name'
`sb-ext:*posix-argv*', `sb-sys::get-machine-version'.~%▶▶▶")

(fundoc 'set-posix-working-directory
        "Set the POSIX view of current directory TO-DIRECTORY.~%~@
Return as if by cl:values:~%
 - cl:nth-value 0 is T on success
 - cl:nth-value 1 is the cl:pathname of TO-DIRECTORY~%~@
When keyword RETURN-AS-PATHNAME \(a boolean\) is T, cl:nth-value 1 is returned as if by:~%
 \(pathname <TO-DIRECTORY>\)~%~@
:EXAMPLE~%
 \(set-posix-working-directory \(merge-pathnames \(make-pathname :directory '\(:relative\)\)\)\)~%
 \(set-posix-working-directory \(pathname-as-file \(merge-pathnames \(make-pathname :directory '\(:relative\)\)\)\)
                              :return-as-pathname t\)~%~@
Following successfully fails:~%
 \(set-posix-working-directory 
  \(merge-pathnames 
   \(make-pathname :directory `\(:relative ,\(symbol-name \(gensym \"non-existent-\"\)\)\)\)\)\)~%
:SEE-ALSO `mon:posix-working-directory', `sb-posix:getcwd', `sb-posix:chdir'.~%▶▶▶")

(fundoc 'posix-working-directory
"Return the POSIX idea of the current working directory.~%~@
:EXAMPLE~%
 \(list \(posix-working-directory\)
        *default-pathname-defaults*\)~%~@
:NOTE Value of posix-working-directory is `cl:setf'able with the following format:~%
 \(setf \(posix-working-directory\) <SET-PATHNAME>\)~%~@
:SEE-ALSO `mon:set-posix-working-directory', `sb-posix:getcwd', `sb-posix:chdir'.~%▶▶▶")

(setf (documentation (fdefinition '(setf posix-working-directory)) 'function)
      #.(format nil
                "Set value of place (posix-working-directory) TO-DIRECTORY.~%
:USAGE~%
 \(setf \(posix-working-directory\) <TO-DIRECTORY>\)~%~@
:EXAMPLE~%
 \(setf \(posix-working-directory\) \(user-homedir-pathname\)\)~%
 \(setf \(posix-working-directory\) *default-pathname-defaults*\)~%~@
:NOTE Setting value of posix-working-directory does not change value of
cl:*default-pathname-defaults*:~%
 \(list :DEFAULTS    *default-pathname-defaults*
       :POSIX-DIR   \(setf \(posix-working-directory\) \(user-homedir-pathname\)\)
       :DEFAULTS    *default-pathname-defaults*
       :POSIX-DIR   \(setf \(posix-working-directory\) *default-pathname-defaults*\)
       :DEFAULTS    *default-pathname-defaults*\)~%~@
:SEE-ALSO `mon:set-posix-working-directory'.~%▶▶▶"))

#+sbcl
(fundoc 'syslog-action
 "Write a syslog message.
Keyword LOG-MESSAGE is a message to write. Default is \"<EMPTY-SBCL-LOG-MESSAGE>\".
Keyword LOG-PRIORITY is an integer in the range 0,7. Default is 6 which
corresponds to `sb-posix:log-info'. Other values are as follows:~%
  0 -- sb-posix:log-emerg      system is unusable
  1 -- sb-posix:log-alert      action must be taken immediately
  2 -- sb-posix:log-crit       critical conditions
  3 -- sb-posix:log-err        error conditions
  4 -- sb-posix:log-warning    warning conditions
  5 -- sb-posix:log-notice     normal, but significant, condition
  6 -- sb-posix:log-info       informational message
  7 -- sb-posix:log-debug      debug-level message~%~@
Keyword LOG-IDENT is a string identifying the orginator of the message. Default is \"sbcl\".
When LOG-IDENT is the default message is logged with the current processes pid.~%~@
:EXAMPLE
 \(syslog-action\)
 \(syslog-action :log-message \"A different log message\"\)
 \(syslog-action :log-message \"A different log message\"  :log-ident \"bubba\"\)
:SEE-ALSO `sb-posix:syslog', `sb-posix:openlog', `sb-posix:closelog', `sb-posix:log-user'.~%▶▶▶")

(fundoc 'getenv-path-pathnames
"Return the pathnmames of directories for current environment varbiable $PATH.~%~@
:EXAMPLE~%
 \(getenv-path-pathnames\)~%~@
:SEE-ALSO `sb-posix:getenv'.~%▶▶▶")

(fundoc 'executable-find
"Search for COMMAND as if by `which`.~%~@
Keywords SKIP-ALIAS, SKIP-FUNCTIONS, SKIP-DOT, and SKIP-TILDE are booleans.
They each default to T.~%~@
When T the effect is to pass the following flags to `which`:~%
 skip-alias     \"--skip-alias\"
 skip-functions \"--skip-functions\"
 skip-dot       \"--skip-dot\"
 skip-tilde     \"--skip-tilde\"~%~@
When NIL the respective flag denoted by the keyword is not passed to `which`.~%~@
Return value is as if by `cl:values'.~%~@
If command is found returned values have the following format:~%
 nth-value 0 is the cl:namestring of the cl:truename of the pathname for COMMAND
 nth-value 1 is a cons of the form: 
  ( <COMMAND> . <PATH-TO-COMMAND> )
 Where <PATH-TO-COMMAND> is a pathname returend by which prior to cl:truename
 expansion (if any).~%~@
If the value of $HOME in current environment is null returned values have the format:~%
 nth-value 0 is NIL
 nth-value 1 is a cons of the form:
  ( :EXIT-STATUS . \"ENOHOME\" )~%~@
If COMMAND is not found anywhere in `exec-path' or an error occurs in the
external evaluation of `which` returned values have the following format:~%
 nth-value 0 is NIL
 nth-value 1 is a cons of the form:~%
  ( :exit-status . <EXIT-STATUS> )~%~@
 Where <EXIT-STATUS> is as per the return value of `sb-ext:process-exit-code'.~%~@
:EXAMPLE~%
 \(executable-find \"sbcl\"\)~%
 \(executable-find \"likely-bogus-command-name\"\)~%~@
:NOTE A null value for a keyword will does not invert its meaning and is
not the same as for examle passing the flags:
 \"--read-alias\", \"--read-functions\", \"--show-dot\", \"--show-tilde\"
and no support is provisioned for doing so.~%~@
:NOTE `which` is not POSIX. However, `command -v` is a POSIX specified command
which accomplishes similiarly.
:SEE (URL `http://pubs.opengroup.org/onlinepubs/009695399/')~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
