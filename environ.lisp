;;; :FILE-CREATED <Timestamp: #{2011-05-07T16:24:06-04:00Z}#{11186} - by MON>
;;; :FILE environ.lisp
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
;; sb-int:*load-source-default-type*
;; sb-impl::*short-site-name* sb-impl::short-site-name
;; sb-impl::*long-site-name*  sb-impl::long-site-name
;; sb-impl::*fasl-file-type*
;; sb-impl::*features*
;; sb-impl::unix-host-unparse-directory-separator sb-impl::*unix-host*
;; sb-impl::unix-host-customary-case sb-impl::*unix-host*
;; sb-sys::software-type
;; sb-sys::software-version
;; sb-ext:*posix-argv*
;; sb-sys::get-machine-version
;;
;; (sb-unix:unix-getrusage sb-unix:rusage_self)
;;
;;; ==============================


(in-package #:mon)
;; *package*

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

;;; ==============================
;; :NOTE `sb-ext::machine-type', `sb-ext::machine-version', `sb-ext::machine-instance'
(defun username-for-system-var-p (verify-with)
  #-sbcl(and (format t "~%:FUNCTION `username-for-system-var-p' -- Current implementation not SBCL~%~
                    Declining further verification of argument VERIFY-WITH~%") nil)
  #+sbcl
  (if (find-package "SB-POSIX")
      (let* ((verify-nm verify-with)
             (verify-home (elt (pathname-directory (namestring (user-homedir-pathname))) 2))
             (verify-getpwnam (sb-posix:getpwnam (car verify-nm))))
        (and 
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

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
