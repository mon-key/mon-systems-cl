;;; :FILE mon-systems/introspect.lisp
;;; ==============================


(in-package #:mon)
;; *package*

(defun fset (symbol def)
  (unless (and (typep symbol 'symbol-not-a-constant)
               (not (special-operator-p  symbol)))
    (simple-error-mon :w-sym 'fset 
                      :w-type 'function 
                      :w-spec "Arg SYMBOL not of type symbol-not-a-constant"
                      :w-got  symbol
                      :w-type-of t))
  (let ((clusr (find-package (find-package "COMMON-LISP-USER")))
        (cl    (find-package "COMMON-LISP")))
    (and (member *package* (list clusr cl))
         (simple-error-mon :w-sym 'fset
                           :w-type 'function
                           :w-spec "Current value of *package* either CL-USER or COMMON-LISP~% ~
                                    refusing to set SYMBOL to DEF"
                           :w-args (list symbol def))))
  (if (null def)
      (and (fboundp symbol) (fmakunbound symbol))
      (setf (fdefinition symbol)
            (if (functionp def) 
                def 
                (and (fboundp def) 
                     (not (special-operator-p def))
                     ;; (symbol-function def)
                     (fdefinition def))))))

(defun keyword-prune (arg-list &rest keys)
  (declare (type list arg-list)) ;; (inline memq)
  (let ((chk-len (or (and (null arg-list) (return-from keyword-prune nil))
                     (length arg-list))))
    (declare (type index chk-len))
    (when (oddp chk-len)
      (simple-error-mon :w-sym  'keyword-prune 
                        :w-type 'function 
                        :w-spec "Arg ARG-LIST not `cl:evenp',~%~
                                    with &rest arg KEYS: ~S~%~
                                    with ARG-LIST of length: ~S"
                        :w-args (list keys chk-len)
                        :w-got  arg-list
                        :w-type-of t 
                        :signal-or-only nil))
    (loop 
       with chk-member = (or (null keys) (and (null (car keys)) (= (length keys) 1)))
       for key-idx from 0 below chk-len by 2
       for val-idx from 1 below chk-len by 2 
       for key = (nth key-idx arg-list)
       for val = (nth val-idx arg-list)
       ;; Don't allow nil to appear in a key postion of ARG list
       unless (typep key 'symbol-not-null)
       do (simple-error-mon :w-sym 'keyword-prune
                            :w-type 'function
                            :w-spec "member of ARG-LIST in key postion ~
                                     not of type `mon:symbol-not-null'~%~
                                     with &rest arg KEYS~% got: ~S~%~
                                     with ARG-LIST~% got: ~S~%~%~
                                     with offending elt in key position"
                            :w-args (list keys arg-list)
                            :w-got  (or key ''nil)
                            :w-type-of t
                            :signal-or-only nil)
       when chk-member
       nconc (list key val)
       else 
       unless (memq key keys)
       nconc (list  key val))))

;; (declaim (inline keyword-property-to-function))
(defun keyword-property-to-function (property &key (constantly nil))
  (declare (symbol property)
           (optimize (speed 3)))
  (let* ((cl-pkg #.(find-package "COMMON-LISP"))
         (maybe-sym (multiple-value-list (find-symbol (string property) cl-pkg))))
    (if (and (car maybe-sym)
             (eql (cadr maybe-sym) :external)
             (fboundp (car maybe-sym)))
        ;; (ignore-errors (symbol-function (car maybe-sym))))))
        (if (macro-function (car maybe-sym))
            (constantly constantly)
            (handler-case (symbol-function (car maybe-sym))
              (undefined-function () (constantly constantly))))
        (constantly constantly))))

;; :SOURCE sbcl/contrib/sb-introspect/introspect.lisp
#+sbcl 
(defun function-arglist (fn)
  (sb-introspect:function-lambda-list fn))

(defun intern-soft (name &optional obarray)
  (declare (type string-not-empty name)
	   ((or symbol string package) obarray))
  #-sbcl (assert (string-not-null-or-empty-p sym-or-name))
   (let ((pkg (typecase obarray
	       ;; (null    *package*)
	       (boolean #+sbcl (sb-int:sane-package)
                        #-sbcl *package*)
	       (package obarray)
	       (string  (find-package (upcase obarray)))
	       (symbol  (or (and (eql obarray 'do-all) 'do-all)
			    (find-package (upcase (symbol-name obarray))))))))
    (cond (obarray     
	   (if (eql pkg 'do-all)
	       (setf pkg (find-all-symbols (string-upcase name)))
	       (and (or (assert (packagep pkg)) t)
		    (setf pkg (find-symbol (string-upcase name) pkg)))))
	  (t (setf pkg (find-symbol (string-upcase name)))))))

;;; ==============================
;; (readtable-case *readtable*)
;; :SEE (URL `http://paste.lisp.org/+2KKF')
;;
;; What is the best way to programmatically `find-all-symbols' where the case of
;; the symbol-name isn't known?
;;
;; The argument to `find-all-symbols' is supposed to be a string searched as if
;; by `string='. The reader _may_ convert symbol case. When searching for
;; symbols with `find-all-symbols'/`find-symbol' I often automatically
;; `string-upcase' the argument. However, it occured to me that this isn't
;; entirely correct b/c of |Piped Symbols|. What is the best way to check the
;; case of a potentially non-existent symbol? e.g. in following form
;; `find-all-symbols' of "hrh-bubba" doesn't find a symbol b/c the reader
;; upcased to HRH-BUBBA.
;;
;; (prog1 
;;     (progn 
;;       (mapc #'read-from-string 
;;             (list "|HRH Bubba|" "|HRH-Bubba|" "hrh-bubba"))
;;       (mapcan #'find-all-symbols 
;;               (list "HRH-BUBBA" "hrh-bubba" "HRH-Bubba" "HRH Bubba")))
;;   (dolist (un (list '|HRH Bubba| '|HRH-Bubba| 'HRH-BUBBA))
;;     (unintern un)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; antifuchs says use `with-package-iterator'
;;
;; - if symbol is _all_ upper-case-p/lower-case-p and without whitespace then dispatch according to readtable-case
;; - if symbol is mixed-case or contains whitespace then `with-package-iterator'
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pjb: says just use `string-equal'
;; (progn (read-from-string "|HRH bubba|")
;;        (mapcar #'(lambda (x) (and (string-equal x "HRH bubba") x))
;;                (find-all-symbols "HRH bubba")))
;;
;; (string-equal (symbol-name (make-symbol "BubBa")) "bubba")
;;; ==============================

(defun symbol-string-name-check (string-sym-name)
  (unless (typep string-sym-name 'string-not-empty)
    (simple-error-mon :w-sym     'symbol-string-name-check
                      :w-type    'function
                      :w-spec    "Arg STRING-SYM-NAME not of type `mon:string-not-empty'"
                      :w-got     string-sym-name
                      :w-type-of string-sym-name))
  ;; The string naming a symbol is potentially immutable. 
  ;; Using `copy-seq' helps protect callers from mutating STRING-SYM-NAME.
  (copy-seq string-sym-name))

(defun symbol-string-name-chk-whitespace (string-sym-name-maybe-wspc &key trim-whitespace)
  (ref-bind rsnpci (symbol-string-name-check string-sym-name-maybe-wspc)
    (or (and (string-no-whitespace-p rsnpci) (the string rsnpci))
        (or (and trim-whitespace
                 (not (string-contains-whitespace-p 
                       (setf rsnpci (string-trim-whitespace rsnpci))))
                 (the string rsnpci))
            (simple-error-mon  
             :w-sym  'symbol-string-name-chk-whitespace
             :w-type 'function
             :w-spec  "~%arg STRING-SYM-NAME-MAYBE-WSPC was: ~S~%~
                       keyword trim-whitespace was: ~S~%~
                       errored with `string-contains-whitespace-p' around:~%~
                       char-at-index:  ~D~%~
                       string-length:  ~S"
             :w-args  `(,string-sym-name-maybe-wspc ,trim-whitespace
                        ,@(cdr (multiple-value-list (string-contains-whitespace-p rsnpci))))
             :w-got    rsnpci ;; 
             :w-type-of t
             :signal-or-only nil)))))
    
(defun read-symbol-name-preserving-case-if (symbol-string-name)
  ;; the :trim-whitespace is for `read-symbol-name-preserving-case' 
  ;; b/c read-from-string :preserve-whitespace nil
  ;; we still signal on situations where SYMBOL-STRING-NAME is " a b "
  ;; e.g. even if we get as far as "a b" and still find whitespace in the
  ;; symbol-name then error
  (symbol-string-name-chk-whitespace symbol-string-name :trim-whitespace t))

(defun make-keyword-sanely (keyword-string)
  (declare (string keyword-string))
  (intern (symbol-string-name-chk-whitespace keyword-string :trim-whitespace t) :keyword))

(defun read-symbol-name-preserving-case (symbol-string-name)
  (let ((str-nm (read-symbol-name-preserving-case-if symbol-string-name)))
    (locally 
        (declare (special %readtable-preserved%))
      (let ((%readtable-preserved% (copy-readtable *readtable*)))
        (setf (readtable-case %readtable-preserved%) :preserve)
        (let ((*readtable* %readtable-preserved%))
          (values-list 
           `(,@(multiple-value-list (read-from-string str-nm t 'EOF :preserve-whitespace nil))
               ,(readtable-case *readtable*))))))))

;; (read-symbol-name-preserving-case "bubba")
;; (where-is-local "bubba" nil :w-case-preserved t)

;; (defun symbol-string-name-case-normalize (string-sym-name)
;;   (let ((ssn (symbol-string-name-check string-sym-name)))
;;     (string-upcase ssn)))


(defun find-package* (package &optional w-case-preserved)
  (ref-bind fnd-pkg 
      (typecase package
        (package            package)
        (symbol            (find-package package))
        ;; (find-package (string-for-readtable-case (symbol-name package) *readtable*)))
        (string-not-empty  
         (find-package (or (and w-case-preserved package)
                           (string-for-readtable-case package *readtable*))))
        (t  (simple-error-mon  
             :w-sym  'find-package*
             :w-type 'function
             :w-spec "arg PACKAGE not `cl:packagep', `cl:symbolp', or `mon:string-not-null-or-empty-p'"
             :w-got package
             :w-type-of t
             :signal-or-only nil)))
    (and (packagep fnd-pkg) 
         ;; (values fnd-pkg (package-name fnd-pkg) package-name)
         fnd-pkg)))

(defun where-is (string-symbol-name &key w-case-preserved)
  (symbol-string-name-check string-symbol-name)
  (or 
   (and w-case-preserved (find-all-symbols string-symbol-name))
   (find-all-symbols (string-for-readtable-case string-symbol-name *readtable*))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((sb-ext:*muffled-warnings* 'style-warning))
(defun where-is-local (symbol-name &optional (package #+sbcl (sb-int:sane-package)
                                                      #-sbcl *package*
                                                      supplied-p)
                       &key w-case-preserved)
  (symbol-string-name-check symbol-name)
  (find-symbol (or (and w-case-preserved 
                        (string-invert-case symbol-name :case w-case-preserved))
                   (string-for-readtable-case symbol-name *readtable*))
               (or (and (not supplied-p) package)
                   (and supplied-p (null package)
                        #+sbcl (sb-int:sane-package)
                        #-sbcl *package*
                        )
                   (ref-bind wil-fnd-pkg (find-package* package) 
                     wil-fnd-pkg
                     (package-error-not package 
                                        :w-sym 'where-is-local 
                                        :w-type 'function
                                        :w-spec "Arg PACKAGE non-existent"
                                        :signal-or-only nil)))))
)) ;; :CLOSE EVAL-WHEN

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((sb-ext:*muffled-warnings* 'style-warning))
(defun symbol-external-p (symbol &optional (package #+sbcl (sb-int:sane-package) 
                                                    #-sbcl *package* package-supplied-p)
                          &key w-case-preserved)
  (let ((sym (and (symbol-string-name-check symbol)
                  ;; keep? doesn't appear to be for `read-symbol-name-preserving-case'
                  (not (string-all-whitespace-p symbol)) 
                  (string-for-readtable-case symbol)
                  (or (and w-case-preserved 
                           (string-invert-case symbol :case w-case-preserved))
                      (string-for-readtable-case symbol *readtable*))))
        (chk-pkg (or (and package-supplied-p
                          (or (find-package* package)
                              (package-error-not package
                                                 :w-sym 'symbol-external-p 
                                                 :w-type 'function
                                                 :w-spec "Arg PACKAGE supplied but not found"
                                                 :signal-or-only nil)))
                     (find-package* package))))
    (multiple-value-bind (result status) (where-is-local sym chk-pkg)
      (values-list (list (eql :external status)
                         `(,@(and result (list :symbol result))
                             ,@(or (and status (list status T)) (list :present nil))
                             :package ,chk-pkg
                             :symbol-name ,sym))))))))

;; (translate-logical-pathname "SYS:SRC;CODE;TARGET-DEFSTRUCT.LISP")
(defun package-external-symbols (package)
  (let ((lst ()))
    (ref-bind pes-pkg-if (find-package* package)
      (do-external-symbols (s pes-pkg-if 
                              (values (sort lst #'string-lessp) 
                                      pes-pkg-if))
        (push s lst))
      (values lst 
              package))))


;; :internal Means present and not exported.
;; :external Means present and exported.

;; (destructuring-bind (nm omit &rest symbol-types)
;;     (list 'wpi (list (find-package* "SB-EXT")) :internal :external)
;;   `(,nm ,omit ,@symbol-types))

#| 

 (let ((with-packages (list "mon")) (with-cl '()) (with-cl-user nil))
   (loop 
      :for pkgs :in with-packages
      :collect (find-package* pkgs) :into with-pkg
      :finally (return (delete-if #'null 
                                  (nconc 
                                   (list (and with-cl
                                              (find-package* "COMMON-LISP"))
                                         (and with-cl-user 
                                              (find-package* "COMMON-LISP-USER")))
                                   with-pkg)))))
|#

(defun do-all-symbols-to-stream (stream 
                                 &key
                                 with-packages
                                 with-cl
                                 with-cl-user)
  ;; (with-output-to-string (strm)
  (with-package-iterator (wpi 
                          (loop 
                             :for pkgs :in (alexandria:ensure-list with-packages)
                             :collect (find-package* pkgs) :into with-pkg
                             :finally (return (delete-if #'null 
                                                         (nconc 
                                                          (list (and with-cl
                                                                     (find-package* "COMMON-LISP"))
                                                                (and with-cl-user 
                                                                     (find-package* "COMMON-LISP-USER")))
                                                          with-pkg))))
                          :external)
    (loop 
       :named syms 
       :while (multiple-value-bind (fnd sym local away) (wpi)
                (if fnd
                    (prog1 t 
                      (format stream  "~%~(~A:~A ~45T:~A~)" (package-name away) sym local))
                    (return-from syms))))))

;;; ==============================
;;; :FROM Erik Naggum <erik@naggum.no> :NEWSGROUP comp.lang.lisp
;;; :DATE 2000/04/19 :MESSAGE-ID <3165125191606936@naggum.no>
;;; :SUBJECT Re: getting a full symbol name 
;;; :SEE (URL `http://groups.google.com/group/comp.lang.lisp/msg/9494248dd2c231e0')
(defun print-symbol-name-qualified (stream object colon-p atsign-p &rest format-args)
  (declare (ignore colon-p atsign-p format-args))
  (let ((*package* (find-package :keyword)))
    (write object :stream stream :readably t)))


;; call-with-package-graph
;;; ==============================
;; (find-all-symbols "SANE-PACKAGE") 
;; *package-names*
;; find-external-symbol
;; (where-is "print-symbol-with-prefix")
;; package-internal-symbols
;; package-external-symbols
;; package-internal-symbol-count
;; package-external-symbol-count
;; package-namify
;; package-listify
;; sb-impl::*package-names*
;; sb-impl::find-external-symbol
;; sb-impl::print-symbol-with-prefix
;; sb-int:package-external-symbol-count
;; sb-int:package-internal-symbol-count
;; sb-kernel:package-internal-symbols
;; sb-kernel:package-external-symbols

;; (error 'undefined-function :name "mon:bubba")
;; 
;; (sb-impl::print-symbol-with-prefix nil 'tt--bubba)

;; with-package-names

#|

 (let ((syms ())
       (pkg (find-package "MON")))
   (do-symbols (das pkg)
     (when (eql (symbol-package das) pkg)
       (push das syms)))
   syms)

 (when (stringp package-or-name)
 (let ((p-nm (string-upcase package-or-name)))
 (if (find-package "NO-BUBBA")

 "NO-BUBBA"
 (packagep "NO-BUBBA")
 (package-name 
 (defun find-package-package-internal-symbol-count  (package-or-name)
   (sb-impl::package-internal-symbol-count (find-package "NO-BUBBA"))

|#

#|
:SOURCE (URL `http://groups.google.com/group/comp.lang.lisp/browse_frm/thread/544807572057447e#')
:COURTESY PJB
 (progn
   (defparameter *system-loaded* '())
   (defparameter *system-errors* '())
   (flet ((got-error (system error)
	    (push (cons system error) *system-errors*))
	  (success (system result)
	    (push (cons system result) *system-loaded*)))
     (dolist (system (ql:system-list))
       (handler-case
	   (restart-case (handler-bind
			     ((error (lambda (err)
				       (got-error system (princ-to-string err))
				       (compute-restarts)
				       (invoke-restart (find-restart 'try-next-one err)))))
			   (success system (ql:quickload (ql-dist:name system))))
	     (try-next-one () nil))
	 #+clisp (SYSTEM::SIMPLE-STORAGE-CONDITION (err)
		   (got-error system (princ-to-string err)))
	 (error (err)
	   ;; should not occur...
	   (got-error system (princ-to-string err))))))
   (format t "Total     systems: ~5D~%" (length (ql:system-list)))
   (format t "Installed systems: ~5D~%" (length *system-loaded*))
   (format t "Errors:            ~5D~%" (length *system-error*))
   (finish-output))
|#
 


;;; ==============================
;;; :INTROSPECT-DOCUMENTATION
;;; ==============================

(fundoc 'fset
  "Set symbol's function definition to definition, and return definition.~%~@
:EXAMPLE%~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `fdefinition', `fmakunbound', `featurep'.~%►►►")

(fundoc 'intern-soft
		"Return the canonical symbol named NAME, or nil if none exists.~%~@
NAME must be a string satisfying `mon:string-not-empty-p'.~
An error is signaled if not.~%~@
NAME is `cl:string-upcase'd and that resulting string is searched for.~%~@
When optional argument OBARRAY is non-nil it specifies a package in which to
search for NAME.~%~@
Valid arguments include a package designator, a symbol or string naming a
package \(case is insignificant\)
When OBARRAY is the symbol 'do-all NAME is searched as if by `cl:find-all-symbols'.
Default is to limit search to value of current value of `cl:*package*'.~%~@
:EXAMPLE~%
 \(intern-soft \"INTERN-SOFT\" \"MON\"\)~%
 \(intern-soft \"intern-soft\" \"mon\"\)~%
 \(intern-soft \"INTERN-SOFT\" \(find-package \"MON\"\)\)~%
 \(intern-soft \"INTERN-SOFT\" 'mon\)~%
 \(intern-soft \"intern-soft\" 'MON\)~%
 \(intern-soft \"intern-soft\" t\)~%
 \(intern-soft \"probably-not-a-symbol\" \"MON\"\)~%
 \(intern-soft \"INTERN-SOFT\" \"COMMON-LISP-USER\"\)~%
 \(intern-soft \"INTERN-SOFT\" \"NON-EXISTEN-PACKAGE\"\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'make-keyword-sanely
"Intern the string designated by KEYWORD-STRING in the KEYWORD package.~%~@
If KEYWORD-STRING is preceded by leading or trailing whitespace trim it.~%~@
If KEYWORD-STRING contains interior whitespace signal an error.~%~@
:EXAMPLE~%
 \(make-keyword-sanely \"bubba\"\)~%
 \(make-keyword-sanely \"bub ba\"\)~%~@
:SEE-ALSO `mon:keyword-prune',`sb-int:symbolicate', `sb-int:keywordicate'.~%►►►")

(fundoc 'keyword-prune
"Return ARG-LIST with KEYS removed.~%~@
Signal an error if ARG-LIST is has length `cl:oddp' or an elt in the key
position is of type `mon:symbol-not-null'.~%~@
:EXAMPLE~%
 \(keyword-prune '\(bubba-a \"bubba-a\" bubba-b \"bubba-b\"\) 'bubba-a\)~%
 \(keyword-prune '\(bubba-a \"bubba-a\" bubba-b \"bubba-b\" bubba-c \"bubba-c\"  
                  bubba-1 \"bubba-1\" bubba-2 \"bubba-2\" bubba-3 \"bubba-3\"\)
                'bubba-1 'bubba-c 'bubba-2\)~%
 \(keyword-prune '\(bubba-a \"bubba-a\" bubba-b \"bubba-b\" :bubba-key \"bubba-key\"\) :bubba-key\)~%
 \(keyword-prune '\(bubba-key :bubba-key :bubba-key 'bubba-key\) :bubba-key\)~%
 \(keyword-prune '\(bubba-key :bubba-key :bubba-key 'bubba-key\) 'bubba-key\)~%
 \(keyword-prune '\(bubba-a \"bubba-a\" bubba-b \"bubba-b\"\)\)~%
 \(keyword-prune '\(bubba-a \"bubba-a\" bubba-b \"bubba-b\"\) nil\)~%
 \(keyword-prune '\(bubba-a \"bubba-a\" bubba-b \"bubba-b\"\) '\(nil\)\)~%
 \(keyword-prune '\(bubba-a \"bubba-a\" bubba-b \"bubba-b\"\) \(\)\)~%
 \(keyword-prune '\(bubba-a \"bubba-a\" bubba-b \"bubba-b\"\) '\(a bubba-a nil \"bubba\"\)\)~%~@
;; Following successfully signal an error:~%~@
 \(keyword-prune '\(bubba-a \"bubba-a\" bubba-b\) 'not-a-key\)~%
 \(keyword-prune '\(bubba-a \"bubba-a\" bubba-b \"bubba-b\" \"not-a-key\" 'not-a-key\) 
                 'bubba-a \"not-a-key\"\)~%
 \(keyword-prune '\(bubba-a \"bubba-a\" nil \"nil\" bubba-b \"bubba-b\"\) 'bubba-a\)~%
 \(keyword-prune '\(bubba-a \"bubba-a\" NIL \"NIL\" bubba-b \"bubba-b\"\) 'nil\)~%
 \(keyword-prune '\(nil 'nil bubba-a \"bubba-a\" bubba-b \"bubba-b\"\)\)~%~@
:SEE-ALSO `mon:make-keyword-sanely'.~%►►►")

(fundoc 'where-is
"Like `cl:find-all-symbols' but ensures that SYMBOL-NAME is `cl:string-upcase'd.~%~@
Signal an error if SYMBOL-NAME is not of type `mon:string-not-empty'.~%~@
Keyword W-CASE-PRESERVED when supplied is as per `mon:string-for-readtable-case'.~%~@
:EXAMPLE~%~@
 \(where-is \"where-is\"\)~%
:SEE-ALSO `mon:where-is-local', `cl:find-symbol'.~%►►►")

(fundoc 'where-is-local
"Like `cl:find-symbol' but ensures that SYMBOL-NAME is `cl:string-upcase'd.~%~@
Signal an error if SYMBOL-NAME is not of type `mon:string-not-empty'.~%~@
If package is provided it may be a string or symbol. 
If package is provided but `mon:find-package*' does not find PACKAGE signal an error.~%~@
Keyword W-CASE-PRESERVED when supplied is as per `mon:string-for-readtable-case'.~%~@
:EXAMPLE~%
 \(where-is-local \"where-is-local\"\)~%
 \(where-is-local \"where-is-local\" \"cl-user\"\)~%
 \(where-is-local \"where-is-local\" :cl-user\)~%
;; These succesffuly signal an error:~%
 \(where-is-local \"where-is-local\" :BUBBA\)~%
 \(where-is-local \"where-is-local\" \"BUBBA\"\)~%~@
:SEE-ALSO `mon:where-is', `mon:print-symbol-name-qualified', `symbol-external-p'.~%►►►")

(fundoc 'symbol-external-p
        "Whether SYMBOL is :external in PACKAGE.~%~@
SYMBOL should satisfy `mon:string-not-null-or-empty-p' signal an error if not.~%~@
Optional arg PACKAGE is the package-name of a package to locate symbol in.
If package is provided but `mon:find-package*' does not find PACKAGE signal with
`mon:package-error-not'.~%~@
Keyword W-CASE-PRESERVED when supplied is as per `mon:string-for-readtable-case'.~%~@
Return value is as if by `cl:values'.
First value is T when SYMBOL is exported from package.
Second value is a list of the form:~%~@
 \( { :SYMBOL <SYMBOL> } ;; if found in <PACKAGE>
   { :INTERNAL | :EXTERNAL | :INHERITED | :PRESENT | } { T | NIL }
     :PACKAGE \(package-name <PACKAGE>\)
     :SYMBOL-NAME  <SYMBOL> \)~%
:EXAMPLE~%
 \(symbol-external-p \"BUBBA\"\)~%
 \(symbol-external-p \"symbol-external-p\"\)~%
 \(symbol-external-p \"consp\" \"mon\"\)~%
 :following  successfully signal errors:~%
 \(symbol-external-p \" \"\)~%
 \(symbol-external-p \"consp\" \"bubba\"\)~%~@
:SEE-ALSO `mon:package-external-symbols', `mon:find-package*'.~%►►►")

(fundoc 'keyword-property-to-function
        "Return the `cl:symbol-function' associated with PROPERTY.~%~@
Property is a keyword with a `cl:symbol-name' which `cl:find-symbol' can find in
the package COMMON-LISP, and which is `cl:fboundp', and for which
`cl:macro-function' does not return, and for which `cl:symbol-function' returns a
`cl:funcall'able function object.~%
Keyword arg CONSTANTLY names a value to give to `cl:constantly' if PROPERTY is
not found in the package COMMON-LISP. Default is nil.~%~@
:EXAMPLE~%
 \(keyword-property-to-function :pathname-host\)~%
 \(keyword-property-to-function :*compile-file-pathname*\)~%
 \(keyword-property-to-function :*compile-file-pathname* :constantly \"bubba\"\)~%
 \(functionp \(keyword-property-to-function :pathname-host\)\)~%
 \(funcall \(keyword-property-to-function :pathname-host\)
          #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\)~%
 \(funcall \(keyword-property-to-function ':*not-gonna-be-there
                                        :constantly \"bubba\"\)\)~%
 \(map 'list #'keyword-property-to-function
      \(list :pathname-host      :pathname-device :pathname-name
            :pathname-directory :pathname-type   :pathname-version\)\)~%
 \(let* \(\(path \(translate-logical-pathname
              #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\)\)
       \(funs \(remove-if #'null 
                        \(map 'list #'keyword-property-to-function
                             \(plist-keys 
                              \(pathname-components path :list-or-plist :plist\)\)\)\)\)\)
  \(mapcar #'\(lambda \(x\) \(funcall x path\)\) funs\)\)~%~@
:SEE-ALSO `mon:keyword-prune', `mon:plist-keys'.~%►►►")

(fundoc 'package-external-symbols
"Return the symbols :external to PACKAGE.~%~@
Return value is as if by `cl:values'.
First value is a list of symbols or nil
Second value is a package designator or PACKAGE if none was found.
:EXAMPLE~%
 \(package-external-symbols \"mon\"\)~%
 \(package-external-symbols \"bubba\"\)~%~@
:SEE-ALSO `mon:find-package*', `mon:symbol-external-p'.~%►►►")

(fundoc 'do-all-symbols-to-stream
"Return the external symbols of WITH-PACKAGES to STREAM.~%~@
WITH-PACKAGES is a list of package-names or package-designators, 
If WITH-PACKAGES is an atom it is converted to a list.~%~@
When keywords WITH-CL and WITH-CL-USER are non-nil include the symbols external
to the COMMON-LISP and COMMON-LISP-USER packages.~%~@
:EXAMPLE~%
 \(do-all-symbols-to-stream nil :with-packages \"mon\"\)~%
 \(do-all-symbols-to-stream t :with-packages '\(\"MON\"\)\)~%
 \(with-output-to-string \(strm\)
  \(do-all-symbols-to-stream strm :with-packages '\(\"MON\"\)\)\)~%~@
:SEE-ALSO `mon:package-external-symbols', `cl:do-symbols', `cl:do-all-symbols',
`cl:with-package-iterator'.~%►►►")

(fundoc 'find-package*
"Like `cl:find-package' except when PACKAGE is `stringp', in which case, frob it
with `string-for-readtable-case' unless optional arg W-CASE-PRESERVED is non-nil.~%~@
:EXAMPLE~%
 \(find-package* \"mon\"\)~%
 \(find-package* 'mon\)~%
 \(find-package* :mon\)~%
 \(find-package* \(find-package 'mon\)\)~%
 \(find-package* \"mon\"\)~%
 \(find-package* \"mon\" t\)~%
 \(find-package* 'mon\)~%
 \(find-package* '#:mon\)~%
 \(find-package* 'MON\)~%
;; Following error successfully and differently:~%
 \(find-package* :bubba\)~%
 \(find-package*  \"bubba\"\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'print-symbol-name-qualified
"Print the fully qualified symbol name of OBJECT.~%~@
Return full symbol-name including package i.e.: \"<package>::<name>\"~%~@
Call from format string with the  ~~/.../ construct. e.g.:~%
 \(format nil \"~~/mon:print-symbol-name-qualified/\" <SYMBOL>\)~%
:NOTE Inside the format direcective the \"mon:\" package qualifier is required.~%
:EXAMPLE~%
 \(format nil \"~~/MON:PRINT-SYMBOL-NAME-QUALIFIED/\" 'nth-value\)~%
 \(format nil \"~~/MON:PRINT-SYMBOL-NAME-QUALIFIED/\" 'print-symbol-name-qualified\)~%~@
:SEE \(URL `http://groups.google.com/group/comp.lang.lisp/msg/9494248dd2c231e0'\)~%~@
:SEE-ALSO `mon:where-is', `mon:where-is-local'.~%►►►")

(fundoc 'symbol-string-name-check
        "Check that STRING-SYM-NAME is of type `mon:string-not-empty'.~%~@
If so return value is a copy of STRING-SYM-NAME as if by `cl:copy-seq'.~%~@
When STRING-SYM-NAME is `mon:string-empty-p' signal a `mon:simple-mon-error' condition.
:EXAMPLE~%~@
 \(symbol-string-name-check \"bubba\"\)
 \(symbol-string-name-check \"\"\)
:NOTE Following returns _correctly_:~%
 \(symbol-string-name-check \" \"\)~%~@
The function `mon:read-symbol-name-preserving-case-if' checks this function's
return value for `mon:string-contains-whitespace-p'.~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'symbol-string-name-chk-whitespace
"Check for `mon:*whitespace-chars*' in STRING-SYM-NAME-MAYBE-WSPC and maybe remove them.~%~@
Return value is a copy of STRING-SYM-NAME-MAYBE-WSPC.~%~@
Keyword trim-whitespace when non-nil says to trim leading and trailing
whitespace if present.
Signal a simple-mon-error condition if STRING-SYM-NAME-MAYBE-WSPC is
`string-contains-whitespace-p' interior whitespace \(even after trimming\).
:EXAMPLE~%
 \(symbol-string-name-chk-whitespace \"ab\"\)~%
 \(symbol-string-name-chk-whitespace \"ab\" :trim-whitespace t\)~%
 \(symbol-string-name-chk-whitespace \"a \" :trim-whitespace t\)~%
 \(symbol-string-name-chk-whitespace \" a \" :trim-whitespace t\)~%
 Following succesffuly signal an error:~%
 \(symbol-string-name-chk-whitespace \" a b \" :trim-whitespace t\)~%
 \(symbol-string-name-chk-whitespace \" a \"\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'read-symbol-name-preserving-case-if
"Helper function for `mon:read-symbol-name-preserving-case'.~%~@
When SYMBOL-STRING-NAME is `mon:string-empty-p' signal a `mon:string-empty-error'.~%
When SYMBOL-STRING-NAME is `mon:string-contains-whitespace-p' signal with
`mon:simple-error-mon'.~%~@
If both of the above constraints are satisfied return T.~%~@
:EXAMPLE~%
 \(read-symbol-name-preserving-case-if \"string-ok\"\)~%
 \(read-symbol-name-preserving-case-if \"\"\)~%
 \(read-symbol-name-preserving-case-if \"string-has-wspc-> \"\)~%~@
:SEE-ALSO `mon:string-for-readtable-case', `mon:string-invert-case',
`mon:string-no-whitespace-p', `mon:string-all-whitespace-p',
`mon:string-contains-whitespace-p', `mon:string-trim-whitespace'.~%►►►")

(fundoc 'read-symbol-name-preserving-case
"Read SYMBOL-STRING-NAME in a readtable with `cl:readtable-case' :preserve.~%~@
Dynamically binds `cl:*readtable*' to `cl:copy-readtable'd duplicate of itself.~%~@
SYMBOL-STRING-NAME is a string satisfying
`mon:read-symbol-name-preserving-case-if' signal an error if not.~%~@
Return value is as if by `cl:values'. 
The first two values are as per `cl:read-from-string'~%
 - First value is a symbol read .~%
 - Second value is an integer value zero or above indicating the length of
   string read.~%
 - Third value is :preserve.~%~@
:EXAMPLE~%
 \(let \(\(gthr
        \(loop 
           :with syms-rd = \(list \"Hrh-Bubba\" \"|Hrh-Bubba|\"\)
           :for syms in syms-rd
           :collect \(multiple-value-list 
                     \(read-symbol-name-preserving-case syms\)\) into prsrv
           :collect \(multiple-value-list \(read-from-string syms\)\) into stndrd
           :finally \(return \(nconc prsrv stndrd\)\)\)\)\)
   \(prog1 gthr
     \(mapc #'\(lambda \(x\) \(unintern \(car x\)\)\) gthr\)\)\)~%~@
:SEE-ALSO `mon:string-for-readtable-case', `mon:string-invert-case'.~%►►►")

#+sbcl
(fundoc 'symbolicate
	  "Concatenate together the names of some strings and symbols,
 producing a symbol in the current package.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `sb-int:symbolicate', `sb-int:keywordicate'.~%►►►")

#+sbcl
(fundoc 'keywordicate
	  "Like `symbolicate', but producing keywords.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `sb-int:symbolicate', `sb-int:keywordicate', `sb-int:sane-package'.~%►►►")

#+sbcl
(setf (documentation 'sb-int:sane-package 'function)
      #.(format nil
"Access *PACKAGE* in a recoverable.
Allow recovery if we'ved done something silly like:~%
 \(SETF *PACKAGE* :CL-USER\).~%~@
Such an assignment is undefined behavior, so it's sort of reasonable for it to
cause the system to go totally insane afterwards, but it's a fairly easy mistake
to make, so let's try to recover gracefully instead.~%~@
:EXAMPLE~%
 \(sb-int:sane-package\)~%~@
:SEE-ALSO `mon:where-is', `mon:where-is-local', `sb-int:symbolicate',
`sb-int:keywordicate'.~%►►►"))


#+sbcl 
(setf (documentation 'sb-impl::symbol-hash 'function)
      #.(format nil
"Return the built-in SBCL hash value for SYMBOL.~%~@
:EXAMPLE~%
 \(sb-impl::symbol-hash 'sb-impl::symbol-hash\)~%~@
:SEE-ALSO `symbol-name', `symbol-value', `symbol-plist',
`symbol-global-value', `symbol-package'.~%►►►"))

#+sbcl
(setf (documentation 'function-arglist 'function)
      #.(format nil (documentation 'sb-introspect:function-lambda-list 'function)))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
