;;; :FILE-CREATED <Timestamp: #{2010-12-16T20:16:01-05:00Z}#{10504} - by MON>
;;; :FILE mon-systems/macros.lisp
;;; ==============================


(in-package #:mon)

;; sbcl/src/code/early-extensions.lisp
;; *profile-hash-cache*
;; define-hash-cache
;; define-cached-synonym
;; defun-cached
;;; list-circular-p
;;; ==============================
;;; :NOTE MACROEXPAND-1 and SB!INT:EVAL-IN-LEXENV are the only SBCL
;;; functions that get called with the constructed environment
;;; argument. sb-int:eval-in-lexenv punts to sb-int:simple-eval-in-lexenv unless
;;; sb-ext:*evaluator-mode* is :interpret (the default is :compile)
;;; 
;;; (where-is "eval-in-lexenv")        ;=> sb-int:eval-in-lexenv
;;; (where-is "*evaluator-mode*")      ;=> sb-ext:*evaluator-mode*
;;; (where-is "simple-eval-in-lexenv") ;=> sb-int:simple-eval-in-lexenv
;;; (where-is "eval-in-native-environment") ;=> sb-eval:eval-in-native-environment
;;;
;;; :SEE :FILE sbcl/src/code/eval.lisp
;;; :SEE :FILE sbcl/src/code/full-eval.lisp
;;; ==============================


;;; ==============================
;;; :NOTE In SBCL 1.0.42.25 built from git there is a bug where the macro documentation can't be setf'd.
;;; :SEE :FILE BUG-SBCL-1-42-2010-09-20
;;; :SEE (URL `https://bugs.launchpad.net/sbcl/+bug/643958')
;;;  Until bug 643958 is fixed we need the following:
;;; (in-package :SB-PCL)
;;; ==============================
;; Should be fixed now <Timestamp: #{2010-12-16T20:16:05-05:00Z}#{10504} - by MON>
;; (defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'function)))
;;   ;; (when (and (legal-fun-name-p x) (fboundp x))
;;   (when (and (sb-pcl::legal-fun-name-p x) (fboundp x))
;;     ;; :WAS (setf (documentation (symbol-function x) t) new-value)))
;;     (setf (documentation (or (macro-function x) (symbol-function x)) t)
;; 	  new-value)))
;;; ==============================

;;; :NOTE not a macro but might as well make sure it happens early.
(declaim (inline and-so))
(defun and-so (and-x &rest and-so)
  (and and-x `(,and-x ,@and-so)))

;;; :SOURCE s-sql.lisp :NOTE sbcl/src/compiler/disassem.lisp has same as `strip-quote'
(declaim (inline dequote))
(defun dequote (val)
  (declare ((or symbol cons) val))
  (if (and (consp val) (eq (car val) 'quote))
      (cadr val)
      val))

;;; :SOURCE fare-utils-20100901-git/base/utils.lisp :WAS `eval-now'
(defmacro eval-when-all (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    ,@body))

;;: :SOURCE clocc/src/cllib :WAS `el::eval-when-compile'
(defmacro eval-when-compile (&rest body)
  `(eval-when (:compile-toplevel :load-toplevel) ,@body))

(defmacro w-debug-declared ()
  ;; :NOTE (info "(sbcl)Debugger Policy Control") says `compilation-speed` must
  ;; be 0 in order for source to be steppable, e.g.:
  ;;  `> (max speed space compilation-speed)'
  `'(declare (optimize (safety 3) (debug 3) (speed 0) (compilation-speed 0))))

(defmacro w-fast-declared ()
  ;; :NOTE Don't use (safety 0) on SBCL it can cause segv's!
  `'(declare (optimize (speed 3) (safety 1) (compilation-speed 0) (space 0))))


;;; ==============================
;;; :WITH-STYLE-MACROS
;;; ==============================

;;; Now imports `alexandria/with-gensyms' from alexandria/macros.lisp 
;; #-sbcl 
;; (defmacro with-gensyms (syms &body body)
;;   `(let ,(mapcar #'(lambda (s)
;;                      `(,s (gensym)))
;;                  syms)
;;      ,@body))

#-sbcl 
(defmacro with-gensyms (syms &body body)
  `(alexandria:with-gensyms ,syms ,@body))

#+sbcl
(defmacro with-gensyms (syms &body body)
  `(sb-int:with-unique-names ,syms ,@body))

;;; :SOURCE mcclim/ESA/utils.lisp :WAS `retaining-value'
(defmacro retaining-value ((bound-symbol &optional initial-value) &body body)
  (let ((symbol (gensym)))
    `(progn (unless (boundp ',symbol)
              (setf (symbol-value ',symbol) ,initial-value))
            (let ((,bound-symbol (symbol-value ',symbol)))
              (unwind-protect (progn ,@body)
                (setf (symbol-value ',symbol) ,bound-symbol))))))

;;; :SOURCE GBBopen/source/tools/tools.lisp
(defmacro multiple-value-setf (places form)
  (loop 
      for place in places
      for name = (gensym)
      collect name into bindings
      if (eql 'nil place)
        unless (eq place (first places))
          collect `(declare (ignore ,name)) into ignores
        end                                         
      else
        collect `(setf ,place ,name) into body
      finally (return `(multiple-value-bind ,bindings ,form
                         ,@ignores
                         ,@body
                         ;; Return the primary value (like multiple-value-setq)
                         ,(first bindings)))))

;; :NOTE The `with-gensyms' provided in mon-macros uses `with-unique-names'
;;  which differs from the with-gensyms version provided in 
;; :FILE CLOCC/cllib/port/ext.lisp
;; :SOURCE CLOCC/cllib/port/ext.lisp
(defmacro %compose (&rest functions)
  (labels ((rec (xx yy)
             (let ((rr (list (car xx) (if (cdr xx) (rec (cdr xx) yy) yy))))
               (if (consp (car xx))
                   (cons 'funcall (if (eq (caar xx) 'quote)
                                      (cons (cadar xx) (cdr rr)) rr))
                   rr))))
     ;; #-sbcl (with-gensyms ("-COMPOSE-" arg)
    (with-gensyms (arg)
      `(lambda (,arg) ,(rec functions arg)))))


;;; ==============================
(defmacro if-not (test-form then &optional else)
  `(if (not ,test-form) 
       ,then 
       ,else))


;;; ==============================
;;; :ANAPHORIC-STYLE-MACROS
;;; ==============================

;; :SOURCE alexandria/flow-control.lisp :WAS `if-bind'
(defmacro ref-bind (var test &body then/else)
  (unless (car then/else)
    (simple-error-mon :w-sym 'ref-bind 
                      :w-type 'macro 
                      :w-spec "arg then/else missing THEN clause."))
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

;; :WAS `aif'
(defmacro ref-it-if (test then &optional else)
  `(let ((it ,test)) (if it ,then ,else)))


;;; ==============================
;;; :BINDING-MACROS
;;; ==============================

(defmacro bound-and-true-p (var)
  `(and (boundp (quote ,var)) ,var))
;; 
;;; :SOURCE lice/src/global.lisp 
;;; :NOTE Probably sourced from CLOCC\cllib/port/ext.lisp
(defmacro defsubst (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list ,@body)))
;;
;; :SOURCE CLOCC\cllib/port/ext.lisp
;; (defmacro defcustom (name type init doc)
;;   "Define a typed global variable."
;;   `(progn (declaim (type ,type ,name))
;;     (defvar ,name (the ,type ,init) ,doc)))

;;; ==============================
;;; :NOTE This looses when TO-SYMBOL is redefined.
;;; (defmacro defalias (from-symbol to-symbol); &optional docstring)
;;;   `(declare (function ,to-symbol))
;;;   `(progn 
;;;      (intern (symbol-name ,from-symbol) (sb-ext::sane-package))
;;;      (setf (fdefinition ,from-symbol) (fdefinition ,to-symbol))))
;;;
;;;-------------------------------------------
;;; This is how clocc/src/cllib approaches it:
;;; "defalias is a function in Emacs, cannot use defmacro a simple 
;;;  (setf fdefinition) is no good since Emacs defalias works on macros too"
;;;  (defun defalias (symbol def)
;;;    (eval `(defmacro ,symbol (&body body) (list* ',def body))))
;;;
;;;-------------------------------------------
;;; This is how Mariano Montone approaches it:
;;; :SOURCE gestalt/deps/util/util.lisp
;;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;;   (defmacro defalias (fname &rest aliases)
;;;     (let ((setf-args '()))
;;;       `(progn
;;; 	 (setf
;;; 	  ,@(loop for alias in aliases
;;; 	       do (setf setf-args (nconc setf-args (list `(symbol-function ',alias) `(function ,fname))))
;;; 	       finally (return setf-args)))
;;; 	 ',aliases))))

;;; ==============================
;;
;; :PASTED (URL `http://paste.lisp.org/+2LDL/1')
;; <Timestamp: #{2011-03-30T16:15:04-04:00Z}#{11133} - by MON>
(defmacro defalias (dest-fun-symbol source-fun-symbol &optional docstring) ;; &environment env)
  ;; :NOTE the eval-when doesn't appear to be needed.
  ;;`(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((dest-ftype (gensym))
        ;; (cl-pkg (find-package "COMMON-LISP")))
        (cl-pkg (find-package '#:common-lisp)))
    `(let ;;((dest-ftype ,dest-ftype))
         ((,dest-ftype nil))
       (block nil  
         (if (or 
              (and 
               (not (symbolp ,source-fun-symbol))
               (setf ,dest-ftype '(not symbol)))
              (and (or 
                    (typep ,dest-fun-symbol   'boolean) 
                    (typep ,source-fun-symbol 'boolean))
                   (setf ,dest-ftype 'boolean))
              (and (special-operator-p ,dest-fun-symbol)
                   (setf ,dest-ftype 'special-operator))
              (and (special-operator-p ,source-fun-symbol)
                   (setf ,dest-ftype 'special-operator))
              (and (eq (symbol-package ,dest-fun-symbol) ,cl-pkg)
                   (setf ,dest-ftype '(symbol-package ,cl-pkg)))
              (and (not (fboundp ,source-fun-symbol))
                   (setf ,dest-ftype '(not fbound))))
             (return 
               (values
                (warn "~%~3T:MACRO defalias ~
                    -- failed to satisfy one or more constraints~%~22T~
                    declining to set fdefinition of: ~S with: ~S~%" 
                      ,dest-fun-symbol ,source-fun-symbol)
                ,dest-ftype))
             (and
              (cond 
                ((macro-function ,source-fun-symbol)
                 (setf (macro-function ,dest-fun-symbol) (macro-function ,source-fun-symbol))
                 (setf ,dest-ftype 'macro-function))
                ((functionp (,'function ,(dequote source-fun-symbol)))
                 (setf (symbol-function ,dest-fun-symbol) (symbol-function ,source-fun-symbol))
                 (setf ,dest-ftype 'symbol-function))
                (t 
                 (return 
                   (values 
                    (warn "~%~3T:MACRO defalias ~
                              -- failed to satisfy one or more constraints~%~22T~
                              declining to set fdefinition of: ~S with ~S~%"
                          ,dest-fun-symbol ,source-fun-symbol)
                    (type-of ,source-fun-symbol)))))
              (if (compiler-macro-function ,source-fun-symbol)
                  (progn
                    (setf (compiler-macro-function ,dest-fun-symbol) (compiler-macro-function ,source-fun-symbol))
                    (setf ,dest-ftype (list ,dest-ftype 'compiler-macro-function)))
                  t)
              (if (and ,docstring (stringp ,docstring))
                  (or (fundoc ,dest-fun-symbol ,docstring) t)
                  t)
              (return
                (if (atom ,dest-ftype) 
                    (values ,source-fun-symbol ,dest-ftype)
                    (values-list (adjoin ,source-fun-symbol ,dest-ftype))))))))))


;;; ==============================
;;; :TYPE-MACROS
;;; ==============================
;;; :TYPE-MACROS
;; :SOURCE sbcl/src/code/early-extensions.lisp :WAS `and/type'
(defmacro type-and (type-x type-y)
  `(multiple-value-bind (val1 win1) ,type-x
     (if (and (not val1) win1)
         (values nil t)
         (multiple-value-bind (val2 win2) ,type-y
           (if (and val1 val2)
               (values t t)
               (values nil (and win2 (not val2))))))))

;; :SOURCE sbcl/src/code/early-extensions.lisp :WAS `not/type'
(defmacro type-not (type-checked)
  (let ((val (gensym "VAL"))
        (win (gensym "WIN")))
    `(multiple-value-bind (,val ,win)
         ,type-checked
       (if ,win
           (values (not ,val) t)
           (values nil nil)))))

;; :SOURCE cxml-20101107-git/test/domtest.lisp
(defmacro string-case (keyform &rest case-clauses)
  (let ((key (gensym "key")))
    ;; Don't bother mapping when keyform is T|NIL
    (when (typep keyform 'boolean) (return-from string-case nil))
    (labels ((chk-keys (keys-seq ky) 
               (or (or (and (every #'stringp keys-seq)
                            `(find ,ky ',keys-seq :test 'string=))
                       (and (some #'booleanp keys-seq)
                            (let ((cpy (delete-if #'booleanp (copy-seq keys-seq))))
                              (every #'stringp cpy)
                              `(find ,ky ',cpy :test 'string=))))
                   (error 'type-error  
                          :datum (elt keys-seq (position-if-not #'stringp keys-seq))
                          :expected-type 'string))))
      `(let ((,key ,keyform))
         (declare (ignorable ,key))
         (cond
           ,@(loop
                for (keys . forms) in case-clauses
                for test = (etypecase keys
                             (string `(string= ,key ,keys))
                             (sequence (chk-keys keys key))
                             ((eql t) t))
                collect `(,test ,@forms) into cond-forms
                finally (return (delete-if #'booleanp cond-forms :key #'car))))))))

;; :NOTE Paul Khuong's string-case is much faster when we know that
;; CASE-CLAUSES is always a list of pairs with car of each elt `stringp'
(defmacro string-case-fast ((keyform &key (default ''nil)) &body case-clauses)
  ;; Don't bother mapping when keyform is T|NIL
  (when (typep keyform 'boolean) (return-from string-case-fast nil))
  `(string-case:string-case (,keyform ,@(and default (list :default default)))
     ,@case-clauses))



;;; ==============================
;;; :NUMBERS-MACROS
;;; ==============================

;; :COURTESY cllib/math.lisp
(defmacro to-percent (vv)
  `(* 100d0 (1- ,vv)))

;; :COURTESY cllib/withtype.lisp :WAS `dfloat'
(defmacro number-to-double-float (num)
  `(float ,num 1d0))

(defmacro byte-octets-for-integer (unsigned-integer)
  (declare (optimize (speed 3)))
  `(etypecase (integer-length ,unsigned-integer)
     ,@(loop 
        for cnt upfrom 1 below 17  
        for x upfrom 0 below 128 by 8
        for low = 0 then (+ x 1) 
        for high = 8 then (+ x 8)
        collect (list (list 'integer low high) cnt))))

;; :SOURCE sbcl/src/code/run-program.lisp :WAS `round-bytes-to-words'
;; SB-IMPL::ROUND-BYTES-TO-WORDS
#+sbcl
(defmacro bytes-round-to-words (n-bytes)
  ;; N-MACHINE-WORD-BITS the natural width of a machine word (as seen in e.g. register width,
  ;; address space)
  ;;
  ;; N-BYTE-BITS the number of bits per byte, where a byte is the smallest addressable object
   ;; 
  ;; :WAS 
  ;; (let ((bytes-per-word (/ sb-vm:n-machine-word-bits sb-vm:n-byte-bits)))
  ;;   `(logandc2 (the fixnum (+ (the fixnum ,n-bytes)
  ;;                             (1- ,bytes-per-word))) (1- ,bytes-per-word))))
  (with-gensyms (bytes-per-word per-word-less-1)
    `(let* ((,bytes-per-word (/ sb-vm:n-machine-word-bits sb-vm:n-byte-bits)) ;; => (/ 32 8) on x86-32
            (,per-word-less-1 (the fixnum (1- ,bytes-per-word)))) 
       (declare (fixnum ,per-word-less-1))
       (logandc2 (the fixnum 
                   (+ 
                    ;; This declaration is faulty b/c N-BYTES could be a value
                    ;; somewhere greater than (- most-positive-fixnum
                    ;; per-word-less-1) Although the likelihood of this occuring
                    ;; is less than realistic...
                    (the (integer 0 ,most-positive-fixnum) ,n-bytes) 
                    ,per-word-less-1)) ,per-word-less-1))))


;;; ==============================
;;; :STRING-MACROS
;;; ==============================

;; :COURTESY babel-20100901-darcs/src/strings.lisp
(defmacro string-get (string index)
  `(char-code (char ,string ,index)))

;; :COURTESY babel-20100901-darcs/src/strings.lisp
(defmacro string-set (code string index)
  `(setf (char ,string ,index) 
	 (code-char ,code)
	 ))

;;; :SOURCE texinfo-docstrings/colorize.lisp
(defmacro string-append-into (output-sym &rest args)
  `(and (or (and (null ,output-sym)
		 (setq ,output-sym ""))
	    t)
	(setq ,output-sym (concatenate (quote string) ,output-sym ,@args))))

;; :SOURCE Sonya Keene p 183 :WAS `standardize-output-stream-var'
;; :WAS (defmacro standardize-output-stream-var (stream)
;;        `(setf ,stream (cond 
;;                         ((eq ,stream t) *terminal-io*)
;;                         ((null ,stream) *standard-output*)
;;                         (t ,stream))))
;; 
(defmacro output-stream-normalize (stream)
  (let ((nrmlz (gensym)))
    `(let ((,nrmlz ,stream))
       (setf ,nrmlz (cond 
                      ((eql ,nrmlz t) *terminal-io*)
                      ((null ,nrmlz)  *standard-output*)
                      (t ,nrmlz))))))
  
;;; ==============================
;;; :CREATED <Timestamp: #{2010-09-20T17:45:16-04:00Z}#{10381} - by MON>
;;; :COURTESY Geoff Summerhayes comp.lang.lisp :WAS `sift-list'
;;; :DATE Tue, 21 May 2002 18:41:19 GMT
;;; :SUBJECT Re: Stumped (basic LISP question)
;;; 
;;; :ELISP-VERSION
;;; (defmacro list-sift (sift-list &rest sift-tests)
;;; (let ((collectors (mon-mapcar #'(lambda (genx) (edebug-gensym "--mon-sift-cllct--")) sift-tests))
;;;       (sft-last (edebug-gensym "--mon-sift-last--")))
;;;   `(let (,@collectors ,sft-last)
;;;      (dolist (sft-itm ,sift-list)
;;;        (cond ,@(mon-mapcar #'(lambda (sft-x sft-y)
;;;                                `((funcall ,sft-x sft-itm) (push sft-itm ,sft-y)))
;;;                            sift-tests collectors)
;;;              (t (push sft-itm ,sft-last))))
;;;      (list ,@collectors ,sft-last)))
;;;
;;; Common Lisp version, note the functional `values' in the tail:
(defmacro list-sift (sift-list &rest sift-tests)
  (let ((collect-sftd (mapcar #'(lambda (x) (declare (ignore x))
					(gensym)) sift-tests))
        (sft-last (gensym)))
    `(let (,@collect-sftd ,sft-last)
       (dolist (sft-itm ,sift-list)
         (cond ,@(mapcar #'(lambda (sft-x sft-y)
                             `((funcall ,sft-x sft-itm) (push sft-itm ,sft-y)))
                         sift-tests collect-sftd)
               (t (push sft-itm ,sft-last))))
       (values ,@collect-sftd ,sft-last))))

(define-modify-macro sortf (place &rest args) sort args)

(define-modify-macro stable-sortf (place &rest args) stable-sort args)


;;; ==============================
(defmacro popn (n-elts place)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)	; LMH ANSI CL changed name
    (with-gensyms (gn glst)
      `(let* ((,gn ,n-elts)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
	   ,set)))))


;;; ==============================
;;; :ALIST-MACRROS
;;; ==============================

;;; :SOURCE /mcclimExtensions/conditional-commands/creating-assoc.lisp :WAS `creating-assoc'
(defmacro assoc-create (item alist &environment env)
  ;; :NOTE `get-setf-expansion' place &optional environment 
  ;;  => vars, vals, store-vars, writer-form, reader-form
  ;; Any compound form is a valid place, since any compound form whose 
  ;; operator F has no setf expander are expanded into a call to `(setf F)'.
  (multiple-value-bind (dummies vals new setter getter)
      (get-setf-expansion alist env)     
    (let ((object (gensym "object-"))) ;;(gensym)))
      `(let* ((,object ,item)
	      ,@(mapcar #'list dummies vals)
	      (,(car new) ,getter))
	 (prog1
	     (or (assoc ,object ,(car new))
		 ;; :WAS (first (setq ,(car new) (cons (list ,object) ,(car new)))))
		 (car (setq ,(car new) (cons (list ,object) ,(car new)))))
	   ,setter)))))
;;
;; Should macroexpand-1 to something like this:
;; (LET* ((#:|object-15058| 'BAZ) (#:NEW15057 LIST))
;;   (PROG1
;;       (OR (ASSOC #:|object-15058| #:NEW15057)
;;           (FIRST (SETQ #:G15057 (CONS (LIST #:|object-15058|) #:NEW15057))))
;;     (SETQ LIST #:G15057)))



;;; ==============================
;;; :ITERATOR-MACROS
;;; ==============================

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym "STOP-")))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

;;; ==============================
;;; until/while using `loop'
(defmacro do-until (form test)
  `(loop ,form (when ,test (return))))
;;
(defmacro do-while (form test)
  `(loop ,form (unless ,test (return))))

;;; ==============================
;;; until/while using `do'
(defmacro until (test &body body)
  `(do () (,test) ,@body))
;;; Alternative version w/out the while call from krmcl/macros.lisp
;;; (defmacro while (test &body body)
;;;   `(do () ((not ,test)) ,@body))
(defmacro while (test &body body)
  `(until (not ,test) ,@body))

;;;  dosequence (based on James Anderson's mod of Thomas Burdick's version)
;;; :SOURCE GBBopen/source/tools/tools.lisp
(defmacro dosequence ((var sequence &optional result) &body forms)
  (with-gensyms (end-p fun)
    `(block nil
       (flet ((,fun (,var ,end-p)
                (tagbody
                  (when ,end-p (go ,end-p))
                  ,@forms
                  (return-from ,fun nil)
                  ,end-p
                  (return-from ,fun ,result))))
         (flet ((fn (element) (,fun element nil)))
           (declare (dynamic-extent #'fn))
           (map nil #'fn ,sequence))
         ,@(when result `((,fun nil t))))))) 

;;; :SOURCE GBBopen/source/tools/tools.lisp
(defmacro dosublists ((var listform &optional result) &body body)
  `(do ((,var ,listform (cdr ,var)))
       ((endp ,var) ,result)
     (declare (list ,var))
     ,@body))

;;; :SOURCE cl-difflib/difflib.lisp :WAS `enumerate'
;;; Hey, it's just like Python's range function!
(defmacro doenumerated ((index-var elt-var sequence &optional result-form) &body body)
  (let ((sequence-var (gensym "SEQUENCE")))
    `(let ((,sequence-var ,sequence))
       (dotimes (,index-var (length ,sequence-var) ,result-form)
	 (let ((,elt-var (elt ,sequence-var ,index-var)))
	   ,@body)))))

;;; :SOURCE cl-difflib/difflib.lisp :WAS `do-range'
;; Hey, it's just like Python's range function!
(defmacro dorange ((var start-form end-form &optional result-form) &body body)
  (let ((start-var (gensym))
	(end-var (gensym)))
    `(let ((,start-var ,start-form)
	   (,end-var ,end-form))
       (do ((,var ,start-var (1+ ,var)))
	   ((>= ,var ,end-var) ,result-form)
	 ,@body))))


;;; ==============================
;;; :VECTOR-MACROS
;;; ==============================

;;; ==============================
;; :NOTE This causes the SBCL compiler to warn:
;; SB-KERNEL:%ASET called on constant data.
;; This macro is prob. not what is wanted. So, mabye instead use the defun
;; below. At least that way we can live in a state of semi-ignorant comfort.
(defmacro aset (array index new-element)
  (declare ((integer 0 *) index))
  `(setf (aref ,array ,index) ,new-element))
;;
;; (defun aset (array index new-element)
;;   (declare ((integer 0 *) index))
;;   (setf (aref array index) new-element))
;;; ==============================

(defmacro mk-array (type init &optional len)
  (if len `(make-array ,len :element-type ,type :initial-element ,init)
      `(make-array (length ,init) :element-type ,type
        :initial-contents ,init)))

;;; :SOURCE freedius/freedius/lisp/lisp/binary-search.lisp :WAS `remove-element'
;;(eval-when (:compile-toplevel  :load-toplevel )
(eval-when (:compile-toplevel :load-toplevel :execute)
(defmacro vector-insert-element (vector pos element num &optional (grow-factor 2))
  `(let ((new-num (1+ ,num))
	 (max (length ,vector)))
     (declare (type fixnum new-num max))
     (cond ((= ,num max)
	    ;; grow the vector
	    (let ((new (make-array (truncate (* max ,grow-factor)) :initial-element nil)))
	      (declare (type simple-vector new))
	      ;; Blt the new buggers into place leaving a space for
	      ;; the new element
	      (replace new ,vector :end1 ,pos :end2 ,pos)
	      (replace new ,vector 
		       :start1 (1+ ,pos) 
		       :end1 new-num
		       :start2 ,pos 
		       :end2 ,num)
	      (fill ,vector nil)
	      (setf (svref new ,pos) ,element)
	      new))
	   (t
	    ;; move the buggers down a slot
	    (replace ,vector ,vector :start1 (1+ ,pos) :start2 ,pos)
	    (setf (svref ,vector ,pos) ,element)
	    ,vector))))
;;
(defmacro vector-remove-element (vector pos num)
  `(progn
     (replace ,vector ,vector :start1 ,pos :start2 (1+ ,pos) :end1 (1- ,num) :end2 ,num)
     (setf (svref ,vector (1- ,num)) nil)
     ,vector))
) ;; :CLOSE eval-when

;; :COURTESY cllib/withtype.lisp :WAS `map-vec'
(defmacro vector-map (type len &rest args)
  `(map-into (make-array ,len :element-type ,type) ,@args))

;;; :SOURCE mcclim/Apps/Scigraph/dwim/macros.lisp
(defmacro condition-case ((&rest varlist) form &rest clauses)
  ;; #+genera (declare (zwei:indentation 1 4 2 2))
  ;; #+genera `(scl:condition-case ,varlist ,form ,@clauses)
  ;; #-genera
  `(let ,(cdr varlist)
     (handler-case
	 ,form
       ,@(mapcar #'(lambda (cl)
		     `(,(first cl)
		       ,(if (first varlist) (list (first varlist)))
		       ,@(cdr cl)))
	   clauses))))

(defmacro multiple-value-nth-p (expr nth-list &key (test 'equal))
  (with-gensyms (m-v-rest)
    `(destructuring-bind (&rest ,m-v-rest) (multiple-value-list ,expr)
       (if (or (null ,m-v-rest)
               (< (length ,m-v-rest) 2))
           nil
           (,test ',nth-list
                  (list (car ,m-v-rest) (cadr ,m-v-rest)))))))

(defmacro multiple-value-nil-nil-p (expr)
  `(multiple-value-nth-p ,expr (nil nil)))

(defmacro multiple-value-nil-t-p (expr)
  `(multiple-value-nth-p ,expr (nil t)))

(defmacro multiple-value-t-nil-p (expr)
  `(multiple-value-nth-p ,expr (t nil)))

(defmacro multiple-value-t-t-p (expr)
  `(multiple-value-nth-p ,expr (t t)))

;;; ==============================
;;: :ASSERT-MACROS
;;; ==============================
;;; #:assert-nil-nil
;;; #:assert-nil-t
;;; #:assert-t-t

;; :SOURCE sbcl/tests/type.impure.lisp
;; (defmacro assert-nil-nil (expr)
;;   `(assert (equal '(nil nil) (multiple-value-list ,expr))))

;; (defmacro assert-nil-t (expr)
;;   `(assert (equal '(nil t) (multiple-value-list ,expr))))

;; (defmacro assert-t-t (expr)
;;   `(assert (equal '(t t) (multiple-value-list ,expr))))


;;; ==============================
;;; :MACROS-DOCUMENTATION
;;; ==============================


;;; ==============================
;;; :EVAL-WHEN-MACROS
;;; ==============================

(fundoc 'eval-when-all
"Expands to an `eval-when' form with all when specifiers present.~%~@
BODY is wrapped inside a form of the type:~%~@
 \(eval-when \(:compile-toplevel :load-toplevel :execute\)\)~%@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'eval-when-compile
"Evaluate BODY when :compile-toplevel :load-toplevel.~%~@
:EXAMPLE~%~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT eval-when-compile is an elisp macro in :FILE lisp/emacs-lisp/byte-run.el.~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

;;; ==============================
;;; :SYMBOL-MACROS
;;; ==============================

(fundoc 'defsubst
"Define NAME as an inline function.  The syntax is just like that of `defun'.~%~@
A `defsubst'd function is one which is `declaim'd as:~% 
 \(declaim \(inline <NAME>\)\)~%~@
:EXAMPLE~%~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT defsubst is a Lisp macro in :FILE lisp/emacs-lisp/byte-run.el.~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'bound-and-true-p
  "Return the value of symbol VAR if it is bound, else nil.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... }~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `boundp'.~%▶▶▶")

(fundoc 'multiple-value-setf
"Like `multiple-value-setq', but works with places.~%~@
A \"place\" of nil means to ignore the corresponding value from FORM.
Return the primary value of evaluating FORM.~%~@
:EXAMPLE~%~% { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc '%compose
  "Compose FUNCTIONS or macros of 1 argument into a funcallable lambda form.~%~@
:EXAMPLE~%
 \(compose abs \(dl-val zz\) 'key\)~%
 ;=> \(lambda \(yy\) \(abs \(funcall \(dl-val zz\) \(funcall key yy\)\)\)\)~%~@
:NOTE This is macro is different from `alexandria:compose' which returns a closure.~%~@
:SEE-ALSO `compose-fun', `compose-all'.~%▶▶▶")


;;; ==============================
;;; :WITH-SYTLE-MACROS
;;; ==============================

(fundoc 'with-gensyms 
  "Create gensyms with SYMS around BODY.~%~@
Automates the oft found macro idiom:~%
 \(let \(\(foo \(gensym \"foo\"\)\)
       \(max-index \(gensym \"max-index-\"\)\)\)
   { ... } \)~%~@
\"Good notation eliminates thought.\" -- Eric Siggia~%~@
:EXAMPLE~%
 \(macroexpand 
  '\(with-gensyms \(a-sym b-sym\) \(list \(identity a-sym\) \(identity b-sym\)\)\)\)~%
 \(with-gensyms \(a-sym b-sym\) \(list \(identity a-sym\) \(identity b-sym\)\)\)~%~@
:NOTE When #+sbcl This is `sb-int:with-unique-names' and returns with slightly
different format than the \"On Lisp\" variant, e.g.~%
 \(with-gensyms \(a-sym b-sym\) `\(,a-sym ,b-sym\)\)~%
 \(with-gensyms \(asym  bsym\)  `\(,asym  ,bsym\)\)~%~@
:SEE-ALSO `gensym', `gentemp', `sb-int:block-gensym'.~%▶▶▶")

(fundoc 'w-debug-declared
  "Inline a declaration to produce debuggable/steppable code.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:NOTE Use with <sharp-sign><period> in forms when this is used.~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

;; (defun foo ()
;;    \\#\\.(w-debug-declared)
;;    (let ((bar zeb))
;;      (or (frob bar)
;;          (quux bzr))))

(fundoc 'w-fast-declared
	"Inline a declaration to produce fast/safe code.~%~@
When evalutated at top of defining form expands to:~%~@
 \(declare \(optimize \(speed 3\) \(safety 1\) \(compilation-speed 0\) \(space 0\)\)\)~%~@
:EXAMPLE~%
 \(defun some-fast-fun \(\)
   #.\(w-fast-declared\)
   {... <DO-FAST-STUFF-HERE> ...} \)~%~@
:NOTE Use with <sharp-sign><period> in forms when this is used.~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'retaining-value
  "Evaluate `body' with `bound-symbol' bound to `initial-value' (default NIL).~%~@
The next time `body' is evaluated, `bound-symbol' will be bound to whatever its
value was the last time evaluation of `body' ended.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'and-so
"When AND-X returns non-nil return a list comprised of AND-X and AND-SO.~%~@
In other words:~%~@
 \(and AND-X `\(AND-X ,@AND-SO\)\)~%~@
:EXAMPLE~%
 \(and-so t \"bubba\" \"more-bubba\"\)~%
 \(and-so \(+ 1 3\) \"bubba\" \"more-bubba\"\)~%
 \(and-so \(not t\) \"bubba\" \"more-bubba\"\)~%~@
:SEE-ALSO `mon:refbind', `mon:retaining-value'.~%▶▶▶")


;;; ==============================
;;; :TYPE-MACROS
;;; ==============================

(fundoc 'type-and
"Whether both TYPE-X and TYPE-Y are true.~%~@
Return as if by `cl:values'.~%~@
Utility macro for two `values' predicates.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:type-not', `mon:type-any', `mon:type-every', `cl:typep',
`cl:subtypep', `cl:type-of', `cl:typecase', `cl:etypecase'.~%▶▶▶")

(fundoc 'type-not
 "Whether TYPE-CHECKED is true. Return as if by `cl:values'.~%~@
Utility macro for type predicates.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:type-and', `mon:type-any', `mon:type-every',
`cl:subtypep', `cl:type-of', `cl:typecase', `cl:etypecase'.~%▶▶▶")


;;; ==============================
;;; :EMACS-COMPAT-MACROS 
;;; ==============================

(fundoc 'condition-case
"Regain control when an error is signaled.~%~@
Executes bodyform and returns its value if no error happens.~%~@
Each element of handlers looks like \(CONDITION-NAME BODY...\)
where the BODY is made of Lisp expressions.~%~@
A handler is applicable to an error
if CONDITION-NAME is one of the error's condition names.~%~@
If an error happens, the first applicable handler is run.~%~@
The car of a handler may be a list of condition names
instead of a single condition name.  Then it handles all of them.~%~@
When a handler handles an error, control returns to the `condition-case'
and it executes the handler's BODY...
with var bound to \(ERROR-SYMBOL . SIGNAL-DATA\) from the error.~%~@
\(If var is nil, the handler can't access that information.\)
Then value of last BODY form is returned from the `condition-case' expression.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `cl:define-condition', `cl:make-condition', `cl:handler-case',
`cl:handler-bind', `cl:restart-case', `cl:restart-bind', `cl:find-restart',
`cl:restart', `cl:signal', `cl:error'.~%▶▶▶")



;;; ==============================
;;; :NUMBERS-MACROS
;;; ==============================

(fundoc 'to-percent 
"Return percentage value of VV.~%~@
:EXAMPLE~%
 \(to-percent 1.234\)~%~@
:SEE-ALSO .~%▶▶▶")

(fundoc   'number-to-double-float
  "Coerce NUM to double float.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")


;;; ==============================
;;; :SEQUENCE-MACROS-DOCUMENTATION
;;; ==============================

(fundoc 'list-sift
  "SIFT-LIST with SIFT-TESTS.~%~@
On a Common Lisp return is as if by values.~%~@
:EXAMPLE~%
 \(list-sift '\( 1 2 3 4 5 6 7 8 9 10\) #'\(lambda \(x\) \(> x 4\)\)\)
  ;=> \(10 9 8 7 6 5\) \(4 3 2 1\)~%
 \(list-sift '\(1 2 3 -1 -2 -3\) #'oddp #'plusp\)
  ;=> \(-3 -1 3 1\) \(2\) \(-2\)~%
 \(list-sift '\(1 2 3 -1 -2 -3\) #'plusp #'oddp\)
  ;=> \(3 2 1\) \(-3 -1\) \(-2\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'popn 
   "Pop N-ELTS off PLACE.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... }~%~@
:SEE-ALSO `cl:pop', `cl:push'.~%▶▶▶")

(fundoc 'assoc-create
"Like `assoc' but create requested alist item on-the-fly if not yet existing.~%~@
:EXAMPLE~%
 \(let* \(\(list '\(\(foo 1\)\)\)\)
   \(list \(assoc 'foo list\)
	 \(assoc 'baz list\)
	 \(assoc-create 'baz list\)
	 \(assoc 'baz list\)
	 list\)\)~%
 ;=> \(\(FOO 1\)
 ;    NIL
 ;    \(BAZ\)
 ;    \(BAZ\)
 ;    \(\(BAZ\) \(FOO 1\)\)\)~%
 \(macroexpand-1 '\(assoc-create 'baz list\)\)~%~@
:SEE (URL `http://paste.lisp.org/display/13846#2')~%~@
:SEE info node `(ansicl)get-setf-expansion)'~%~@
:SEE-ALSO `cl:assoc', `mon:assq'.~%▶▶▶")


;;; ==============================
;;; :ITERATOR-MACROS
;;; ==============================

(fundoc 'while
 "If TEST yields non-nil, eval body... and repeat.~%~@
The order of execution is thus test, body, test, body and so on
until test returns nil.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... }~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:until', `mon:do-while', `mon:do-until'.~%▶▶▶")

(fundoc 'for 
   "A curly-braced `for` style function.~%~@
:EXAMPLE~%
 \(for \(i 0 8\) \(princ i\)\)~%~@
:SEE-ALSO `dohash', `collect', `mon:dosublists', `mon:dosequence',
`mon:do-while', `mon:do-until', `mon:for'.~%▶▶▶")

(fundoc 'until 
 "Until TEST is non-nil do BODY.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... }~%~@
:NOTE This macro uses `cl:do', the `mon:do-until' macro uses `cl:loop'.
:SEE-ALSO `mon:while', `mon:do-while', `mon:for'.~%▶▶▶")

(fundoc 'do-until
"Do FORM until TEST evaluates non-nil.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:NOTE This macros uses `cl:loop' macro, the `mon:until' macro uses `cl:do'.~%~@
:SEE-ALSO `mon:while', `mon:do-while', `mon:for', `dohash', `collect'
`mon:dosublists', `mon:dosequence', `mon:do-while', `mon:do-until',.~%▶▶▶")

(fundoc 'dosublists
"<DOCSTR>~%~@
Arg MAPL-STYLE ~%~@
Arg DOLIST ~%~@
Arg VARIANT ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `dohash', `collect', `mon:dosublists', `mon:dosequence',
`mon:do-while', `mon:do-until', `mon:for'.~%▶▶▶")

(fundoc 'doenumerated
"Iterate over SEQUENCE while keeping track of an index.~%~@
INDEX-VAR is as VAR arg to the do macros.~%~@
For each step through SEQUENCE ELT-VAR is is the current value of `cl:elt' at
index-var's index.~%~@
Optional arg RESULT-FORM is as `cl:dotimes'.~%~@
:EXAMPLE~%
 \(doenumerated \(i e '\(a b c\)\)
    \(format T \\\"~~&~~S ~~S\\\" i e\)\)
  ;=> 1 a
  ;   2 b
  ;   3 c~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

;; (var start-form end-form &optional result-form) &body body)
(fundoc 'dorange  
"Iterate VAR through the range of integers in [START-FORM,END-FORM].~%~@
Return the value of END-FORM (at the time END-FORM is evaluated.~%~@
VAR is bound to the value of END-FORM.~%~@
Optional arg RESULT-FORM is as the `cl:do' macro.~%~@
:EXAMPLE~%
 \(do-range \(i 10 \(length s\)\)
    \(print \(elt s i\)\)\)~%
 \(lambda \(stream char seq bar\)
  \(multiple-value-bind \(a b\)
      \(frob bar\)
    \(do-range \(i a b\)
      \(format stream \"~~&~~A ~~A\" char \(elt seq i\)\)\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")


;;; ==============================
;;; ARRAY-MACROS
;;; ==============================

(fundoc 'aset
"Store into the element of ARRAY at INDEX the value NEW-ELEMENT.~%~@
Return new-element.~%~@
ARRAY may be a vector, a string, or bool-vector.~%~@
INDEX begins from 0.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mk-array'.~%▶▶▶")

(fundoc 'mk-array      
"Macrofied `make-array' with :element-type as TYPE.~%~@
When LEN is non-nil make-array of specified length with :initial-element as INIT.~%~@
When LEN is ommitted make-array of length INIT with :initial-contents as INIT.~%~@
:EXAMPLE~%
 \(mk-array 'simple-string '\(\"Mon\" \"Tue\" \"Wed\" \"Thu\" \"Fri\" \"Sat\" \"Sun\"\)\)~%
 \(mk-array 'char \"abcdef\"\)~%
 \(mk-array 'char \"abcdef\"\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

;; (vector pos element num &optional (grow-factor 2))
(fundoc 'vector-insert-element
"Insert at pos ELEMENT into VECTOR of length NUM.~%~@
VECTOR should be a simple vector.~%~@
ELEMENT is the element to insert at POS.~%~@
Optional argument GROW-FACTOR may be specified to control the new size of the
array if allocation of a new vector is necessary. Default is 2.~%~@
NUM elements \(which may be less than or equal to the length of VECTOR\).~%@
The result of VECTOR-INSERT-ELEMENT must be used, as a new vector may be created.~%~@
The old vector is \"cleared out\" in order that it won't hold on to garbage should
it happen to be in static space.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:NOTE The intent of this macro is to encapsulate the hairiness of inserting an
element into a simple vector.
:NOTE The arguments should probably be lexicals since some of them are evaluated
more than once.
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'vector-map 
"MAP into a simple-array with elements of TYPE and length LEN.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")


;;; ==============================
;;; :STRING-MACROS-DOCUMENTATION
;;; ==============================

(fundoc 'dequote
"If VAL is `cl:consp' and its car is `cl:quote' return VAL's, else return VAL.~%~@
VAL is an object of type `cl:symbol' or `cl:cons'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'string-append-into     
"Concatenate ARGS to a string setting return value to symbol OUTPUT-SYM.~%~@
Rest arg ARGS is a list of `concatenate'able objects, either strings, or
sequences of characters \(lists or vectors\).~%~@
:EXAMPLE~%
 \(defparameter *tt--sym* nil\)~%
 \(string-append-into *tt--sym* '\(#\\a #\\b\) #\(#\\a #\\b\)\)~%
 \(string-append-into *tt--sym* '\(#\\space #\\a #\\b #\\space \) #\(#\\a #\\b\)\)~%
 \(string-append-into *tt--sym* \" string \" #\(#\\a #\\b\)\)~%
 *tt-sym*~%
 \(unintern '*tt--sym*\)~%~@
:SEE-ALSO `mon:string-get', `mon:string-set', `mon:string-append'.~%▶▶▶")

(fundoc 'string-set      
"Set INDEX in STRING to character with `cl:char-code' CODE.~%~@
:EXAMPLE~%
 \(string-set #\\q \"string\" 4\)~%~@
:SEE-ALSO `mon:string-get', `mon:string-set', `mon:string-append'.~%▶▶▶")

(fundoc 'string-get
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:string-get', `mon:string-set', `mon:string-append'.~%▶▶▶")

(fundoc 'string-case
        "Like `cl:case' but matches value of KEYFORM as if by `cl:string='.~%~@
Select a CASE-CLAUSES clause for conditional execution by matching a test-key
produced by evaluating KEYFORM, the return value of which when `cl:string=' is a
key at the head of a clause in CASE-CLAUSES. When matching test-key in
case-clauses the head of each case clause may be a string or a list of strings,
An error is signaled as if by `cl:etypecase' if KEYFORM is not a `cl:string' or
if any elt at head of case-clauses is not a string or sequence contained of
strings.~%~@
:EXAMPLE~%
 \(mapcar #'\(lambda \(x\)
             \(string-case x 
                          \(\"EqUaLs\"    \(string-invert-case x :case :invert\)\)
                          \(\"NOTEQUALS\" \(string-downcase x\)\)
                          \(\"notequals\" \(string-upcase x\)\)
                          \(\(\"eight\" nil \"nine\"  t \"ten\"\) \"8|9|10\"\)
                          \(t \"bobb\"\)
                          \(nil \"bobb\"\)\)\)
         '\(\"EqUaLs\" \"NOTEQUALS\" \"notequals\" \"eight\" nil  \"nine\" t \"ten\"\)\)~%
 \(macroexpand-1 '\(string-case \"bob\"
                  \(\"\" 'empty\)
                  \(\(\"foo\" \"bar\" \"baz\"\) 'foo-bar-baz\)
                  \(nil \"NIL-remains-on-expansion-but-never-found\"\)
                  \(\(\"quid\" nil \"pro\" \"quo\"\) \"quo-found-with-NIL-in-case\"\)
                  \(\(\"fob\" t \"bob\"\) 'fob-t-bob\)
                  \(\"foobar\" 'hit\)
                  \(t \"T-removed-on-expansion-nev3er-found\"\)\)\)~%
\(macroexpand-1 '\(string-case t
                 \(\"no-see-me\"    \(string-invert-case x :case :invert\)\)\)\)~%
:SEE-ALSO `mon:string-case-fast' `string-case:string-case'.~%▶▶▶")

(fundoc 'string-case-fast
"Like `mon:string-case' but with a different syntax and potentially much faster.~%~@
This macro is built upon the `string-case:string-case' macro but attempts to
retain some of the semantics of `mon:string-case'. Specifcally, niether
`mon:string-case' nor `mon:string-case-fast' process boolean elts appearing at
the car of CASE-CLAUSES, nor doe either signal an error when an elt of
CASE-CLAUSES is not matched.~%~@
The caveat being that the car of elts in CASE-CLAUSES must be `stringp' e.g.:~%
 \(\(\"<KEYFORM-A>\" <RESULT>\)
  \(\"<KEYFORM-B>\" <RESULT>\)\)~%~@
Whereas `mon:string-case' allows this syntax:~%
 \(\(\(\"<KEYFORM-A>\" \"<KEYFORM-B>\" \"<KEYFORM-C>\"\) <RESULT>\)
    (\(\"<KEYFORM-A>\" <BOOLEAN> \"<KEYFORM-C>\"\) <RESULT>\)
    \(\"<KEYFORM-D>\" <RESULT>\)\)\)~%~@
Also, KEYFORM is passed as an unqouted list with an optional DEFAULT keyword
whereas `mon:string-case' does not default.~%~@
The difference in convention is as follows:~%
 \(string-case-fast {\(\"<KEYFORM>\"\) | \(\"<KEYFORM>\" :default <DEFAULT>\)}
   \(\(\"<KEYFORM-A>\" <RESULT>\)
    \(\"<KEYFORM-B>\" <RESULT>\) 
    \(...\)*\)\)~%
 \(string-case \"<KEYFORM>\"
  \(\(\"<KEYFORM-A>\" <RESULT>\)
   \(\(\"<KEYFORM-B>\" <BOOLEAN> \"<KEYFORM-C>\"\) <RESULT>\)
   \(\(\"<KEYFORM-D>\" \"<KEYFORM-E>\" \"<KEYFORM-F>\"\) <RESULT>\)
   \(...\)*\)\)~%~@
:EXAMPLE~%~@
\(macroexpand-1 '\(STRING-CASE-FAST \(\"no-find-me\"\)
                 \(\"\" 'empty\)
                 \(\"foo\" 'foo\)
                 \(nil \"NIL-removed-on-expansion\"\)
                 \(\"fob\" 'fob\)
                 \(\"foobar\" 'hit\)
                 \(t \"T-removed-on-expansion\"\)\)\)~%

\(macroexpand-1 '\(string-case-fast \(\"no-find-me\" :default \"bubba\"\)
                 \(\"\" 'empty\)
                 \(\"foo\" 'foo\)
                 \(\"fob\" 'fob\)
                 \(\"foobar\" 'hit\)
                 \(t \"default\"\)\)\)~%
\(macroexpand-1 
 \(macroexpand-1 '\(string-case:string-case \(\"no-find-me\" :DEFAULT \"bubba\"\)
                  \(\"\" 'EMPTY\)
                  \(\"foo\" 'FOO\)
                  \(\"fob\" 'FOB\)
                  \(\"foobar\" 'HIT\)\)\)\)~%
 \(macroexpand-1 '\(string-case \"bob\"
                  \(\"\" 'empty\)
                  \(\(\"foo\" \"bar\" \"baz\"\) 'foo-bar-baz\)
                  \(nil \"NIL-remains-on-expansion-but-never-found\"\)
                  \(\(\"quid\" nil \"pro\" \"quo\"\) \"quo-found-with-NIL-in-case\"\)
                  \(\(\"fob\" t \"bob\"\) 'fob-t-bob\)
                  \(\"foobar\" 'hit\)
                  \(t \"T-removed-on-expansion-nev3er-found\"\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'defalias
"Set DEST-FUN-SYMBOL's function definition to that of SOURCE-FUN-SYMBOL.~%~@
Associates DEST-FUN-SYMBOL with the *package* of current load file, if any.
When optional arg DOCSTRING is non-nil, it specifies the documentation string
for DEST-FUN-SYMBOL; if it is omitted or nil, DEST-FUN-SYMBOL uses the
documentation string \(if any\) of SOURCE-FUN-SYMBOL.~%~@
Source symbols with _existing_ functions defined at compile-time may be
defalias'd such that a destination symbol BAR will pick up source symbol
FOO's function definition. IOW, the toplevel defintion of FOO should appear
before its alias by BAR.~%~@
:NOTE Unlike Elisp's defalias if source symbol is later redefined destinations
symbol will not \"inherit\" sources new symbol-function, macro-function
definition, or compiler-macro-function definitions.~%~@
Return value is as if by `cl:values'.~%~@
If either DEST-FUN-SYMBOL or SOURCE-FUN-SYMBOL are of type `cl:boolean', are
`cl:special-operator-p' or are not `cl:symbolp' returned values have the form:~%
 ;=> nil, boolean~%
 ;=> nil, special-operator~%
 ;=> nil, \(not symbol\)~%~@
If SOURCE-FUN-SYMBOL is not `cl:fboundp' returned values have the form:~%
 ;=> nil, \(not fbound\)~%~@
If DEST-FUN-SYMBOL has as its `cl:symbol-package' the package \"COMMON-LISP\"
returned values have the form:~%
 ;=> nil, \(symbol-package #<package \"COMMON-LISP\">\)~%~@
For each of the above situations a warning is signalled indicating that
DEST-FUN-SYMBOL was not aliased.~%~@
When DEST-FUN-SYMBOL is successfully aliased to SOURCE-FUN-SYMBOL returned
values have one of the following forms:~%
 ;=> <source-fun-symbol>, symbol-function~%
 ;=> <source-fun-symbol>, symbol-function~%
 ;=> <source-fun-symbol>, macro-function~%
 ;=> <source-fun-symbol>, symbol-function, compiler-macro-function~%
 ;=> <source-fun-symbol>, macro-function, compiler-macro-function~%~%
 - nth-value 0 is the symbol SOURCE-FUN-SYMBOL
 - nth-value 1 is either symbol-function or macro-function
 - nth-value 2 when present indicates that SOURCE-FUN-SYMBOL had a
   compiler-macro-function, in which case DEST-FUN-SYMBOL will as well~%
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'output-stream-normalize
        "Normalize STREAM variable if T or NIL was given.~%~@
:EXAMPLE~%
 \(output-stream-normalize nil\)~%
 \(output-stream-normalize t\)~%
 \(let \(\(bubba \(make-string-output-stream\)\)\)
   \(with-output-to-string \(os\)
     \(format os \"~~S ~~S\" 
             \(output-stream-normalize bubba\)
             \(progn 
               \(close bubba\) 
               \(setf bubba nil\)
               \(output-stream-normalize bubba\)\)\)\)\)~%~@
:SEE Sonya Keene p. 183~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")


;;; ==============================
;;; :IMPORTED-MACROS-DOCUMENTATION
;;; ==============================


(fundoc 'make-gensym-list
		"Return a list of N gensyms.~%~@
Common idiom suboperation in macros and other code-manipulating code.~%~@
:EXAMPLE~%~%\(make-gensym-list 8\)~%~@
\(macroexpand '\(make-gensym-list 8\)\)~%~@
:SEE-ALSO `sb-int:make-gensym-list', `alexandria:sb-int:make-gensym-list'.~%▶▶▶")


(fundoc 'symbolicate
        #.(format nil
                  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `sb-int:symbolicate', `sb-int:keywordicate'.~%▶▶▶"))


(fundoc 'keywordicate
        "Like `symbolicate', but producing keywords.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `sb-int:symbolicate', `sb-int:keywordicate', `sb-int:sane-package'.~%▶▶▶")

;;; sbcl/src/code/early-extensions.lisp
#+sbcl 
(setf (documentation 'collect 'function)
      #.(format nil
		"Collect some values somehow.~%~@
Each of the collections specifies a bunch of things which collected during the
evaluation of BODY form.~%~@
The name of the collection is used to define a local macro, a la MACROLET.~%~@
Within BODY, this macro will evaluate each of its arguments and collect the
result, returning the current value after the collection is done.~%~@
BODY is evaluated as a PROGN; to get the final values when you are done,
call the collection macro with no arguments.~%~@
INITIAL-VALUE is a value that the collection begins with, default is NIL.~%~@
FUNCTION is the function which does the collection.~%~@
It is a function which will accept two arguments: the value to be collected and
the current collection.~%~@
The result of the function is made the new value for the collection.~%~@
As a totally magical special-case, FUNCTION may be COLLECT, which tells
us to build a list in forward order; this is the default.~%~@
When INITIAL-VALUE is supplied for COLLECT, stuff will be RPLACD'd onto the end.~%~@
:NOTE FUNCTION may be anything that can appear in the functional position,
including macros and lambdas.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... }~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶"))

#+sbcl
(setf (documentation 'sb-int:dohash 'function)
      #.(format nil
 "Iterate over the entries in a HASH-TABLE, first obtaining the lock
if the table is a synchronized table.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `collect' `mon:dosublists', `mon:dosequence', `mon:do-while', `mon:do-until'
`cl:with-hash-table-iterator'.~%▶▶▶"))

;;; ==============================
;; (find-all-symbols "ONCE-ONLY")
;; (once-only alexandria.0.dev:once-only common-lisp-user::once-only sb-int:once-only)
;;  
;; :SOURCE Docstring as commented in :FILE sbcl/src/code/early-extensions.lisp 
;;  With additions and formatting added. 
(setf (documentation 'once-only 'function)
      #.(format nil
"Evaluates FORMS with symbols specified in SPECS rebound to temporary
variables, ensuring that each initform is evaluated only once.~%~@
Each of SPECS must either be a symbol naming the variable to be rebound, or of
the form:
  \(symbol initform\)~%~@
Bare symbols in SPECS are equivalent to:~%
  \(symbol symbol\)~%
:EXAMPLE~%
  \(defmacro cons1 \(x\) 
    \(once-only \(x\) `\(cons ,x ,x\)\)\)~%
  \(let \(\(y 0\)\) \(cons1 \(incf y\)\)\)
  ;=> (1 . 1)~%
 \(eval \(once-only \(\(bubba 8\)\)
         `\(integerp ,bubba\)\)\)~%~@
The once-only utility is useful for writing source transforms and macros.~%
It provides a concise way to wrap a `let' around some code to ensure that some
forms are only evaluated once.~%~@
Create a `let*' form which evaluates each value expression of SPECS, binding a
temporary variable to the result, and wrapping the `let*' form around the result
of the evaluation of BODY.~%~@
Within the body, each spec in SPECS is bound to a corresponding temporary variable.~%
  \"The macro once-only has been around for a long time on various
   systems [..] if you can understand how to write and when to use
   once-only, then you truly understand macro.\"
   :SOURCE Peter Norvig, _Paradigms of Artificial Intelligence Programming:
           Case Studies in Common Lisp_, p. 853~%~@
:NOTE Docstring from comments in SBCL sources.
:SEE :FILE sbcl/src/code/early-extensions.lisp~%~@
:NOTE There are generally at least two easily accesible once-only macros in 
both SBCL and Alexandria as `sb-int:once-only' and `alexandria:once-only'.
We import Alexandria's b/c SBCL's has a fixme note which basically says,~%
 \"make me use destructuring-bind like the `alexandria:once-only'\"~%~@
:FILE sbcl/src/code/early-extensions.lisp
:FILE alexandria/macros.lisp~%~@
This docstring was cobbled from the referenced sources above.~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶"))

(fundoc 'ref-bind
  "Anaphoric IF control structure.~%~@
If TEST returns non-nil execute THEN, otherwise if ELSE is provided execute ELSE.~%~@
VAR \(a symbol\) is lexcically bound to the primary retrun value of TEST for duration of body.~%~@
:EXAMPLE~%
 \(ref-bind ref-it \(+ 1 3\) ref-it\)~%
Like `aif' but potentially cleaner b/c binds dedicated var instead of `it' which
isn't accesible outside the defining package without a package qualifier,
however, less clean b/c VAR will become an internal symbol of package.
:SEE-ALSO `mon:ref-it-if'.~%▶▶▶")

(fundoc 'ref-it-if
"The `aif' anaphora by a different name.~%~@
:EXAMPLE~%
 \(ref-it-if \(+ 1 3\) it\)~%~@
:NOTE the anaphora `it' is exported from by the :mon package and must be
accessed as `mon:it' by external calling forms.
:SEE-ALSO `mon:ref-bind'.~%▶▶▶")

(fundoc 'byte-octets-for-integer
        "Return count of 8 bit bytes required to represent integer in a byte-array.~%~@
:EXAMPLE~%
 \(loop for ints in '\(#xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                     #xFFFFFFFFFFFFFFFFFFFFFFFF
                     #xFFFFFFFFFFFFFFFF
                     #xFFFFFFFFFFFFFF
                     #xFFFFFFFFFFFF
                     #xFFFFFFFF
                     #x1FFFFFFF
                     #xFFFFFF
                     #xFFFF
                     #xFF
                     #x00\)
 collect \(list \(byte-octets-for-integer ints\) \(integer-length ints\) ints\)\)~%~@
:SEE-ALSO `mon:bytes-round-to-words', `mon:bytes-to-int',
`mon:byte-request-integer', `cl:byte-size', `cl:byte-position'.~%▶▶▶")

(fundoc 'bytes-round-to-words
"Return the number of word bytes required to represent N-BYTES.~%~@
Return value is rounded upward to the next word length required to hold N-BYTES.~%~@
For example, on a machine with 32 bit words (what Intel calls double-words, or
dwords), where a byte is an 8 bit octet if N-BYTES is in the range [1,4] it can
be represented in 4 bytes, e.g.:~%
 \(loop for x from 1 below 5 collect `\(,x . ,\(bytes-round-to-words x\)\)\)~%~@
Comparatively when N-BYTES is in the range [11,17] it can be represented in 12,
16, or 20 4 bytes, e.g.:~%
 \(loop for x from 11 below 18 collect `\(,x . ,\(bytes-round-to-words x\)\)\)~%~@
:EXAMPLE~%~@
 \(macroexpand '\(bytes-round-to-words 17\)\)
:SEE-ALSO `byte-octets-for-integer', `mon:bytes-to-int', `cl:byte-size',
`cl:byte-position'.~%▶▶▶")

(fundoc 'multiple-value-nth-p 
"Whether first two values returned by EXPR are `cl:equal' NTH-LIST.~%~@
When keyword TEST is non-nil it is an unquoted symbol naming a two valued
predicate, e.g.:~%~% :test equalp~%~@
:EXAMPLE~%
 \(multiple-value-nth-p \(values nil nil\) \(nil nil\)\)~%
 \(multiple-value-nth-p \(values #\(a\) #\(b\) #\(c\)\) \(#\(a\) #\(b\)\) :test equalp\)~%
 \(macroexpand-1 '\(multiple-value-nth-p \(values nil nil\) \(nil nil\)\)\)~%~@
:SEE-ALSO `mon:multiple-value-nth-p', `mon:multiple-value-nil-nil-p',
`mon:multiple-value-t-t-p', `mon:multiple-value-t-nil-p',
`mon:multiple-value-nil-t-p', `mon:multiple-value-nil-nil-p', `cl:values',
`cl:values-list', `cl:nth-value', `cl:multiple-value-bind',
`cl:multiple-value-list'.~%▶▶▶")

(fundoc 'multiple-value-nil-nil-p
"Whether first two values returned by EXPR are both null.~%~@
Test that both \(nth-value 0 <EXPR>\) and \(nth-value 1 <EXPR>\) are nil e.g. 
 \(equal '\(nil nil\) \(\(nth-value 0 <EXPR>\) \(nth-value 1 <EXPR>\)\)\)
If expr does not return at least two values, return nil.
:EXAMPLE~%
 \(multiple-value-nil-nil-p \(values 8 8 3\)\)~%
 \(multiple-value-nil-nil-p \(values nil nil 8\)\)~%
 \(multiple-value-nil-nil-p \(values nil nil\)\)~%
 \(multiple-value-nil-nil-p \(values\)\)~%
 \(multiple-value-nil-nil-p \(values nil\)\)~%
 \(macroexpand-1 '\(multiple-value-nil-nil-p \(values nil nil 8\)\)\)~%~@
:SEE-ALSO `mon:multiple-value-nth-p', `mon:multiple-value-nil-nil-p',
`mon:multiple-value-t-t-p', `mon:multiple-value-t-nil-p',
`mon:multiple-value-nil-t-p', `mon:multiple-value-nil-nil-p', `cl:values',
`cl:values-list', `cl:nth-value', `cl:multiple-value-bind',
`cl:multiple-value-list'.~%▶▶▶")

(fundoc 'multiple-value-nil-t-p
        "Whether first two values returned by EXPR are null an t.~%~@
If expr does not return at least two values return nil.~%
:EXAMPLE~%
 \(multiple-value-nil-t-p \(values nil t\)\)~%
 \(multiple-value-nil-t-p \(values nil t 8\)\)~%
 \(multiple-value-nil-t-p \(values 8 8 3\)\)~%
 \(multiple-value-nil-t-p \(values t nil\)\)~%
 \(multiple-value-nil-t-p \(values nil nil\)\)~%
 \(multiple-value-nil-t-p \(values\)\)~%
 \(multiple-value-nil-t-p \(values nil\)\)~%
 \(macroexpand-1 '\(multiple-value-nil-t-p \(values nil t 8\)\)\)~%~@
:SEE-ALSO `mon:multiple-value-nth-p', `mon:multiple-value-nil-nil-p',
`mon:multiple-value-t-t-p', `mon:multiple-value-t-nil-p',
`mon:multiple-value-nil-t-p', `mon:multiple-value-nil-nil-p', `cl:values',
`cl:values-list', `cl:nth-value', `cl:multiple-value-bind',
`cl:multiple-value-list'.~%▶▶▶")

(fundoc 'multiple-value-t-nil-p
        "Whether first two values returned by EXPR are t and null.~%~@
If expr does not return at least two values return nil.~%~@
:EXAMPLE~%
 \(multiple-value-t-nil-p \(values t nil\)\)~%
 \(multiple-value-t-nil-p \(values t nil 8\)\)~%
 \(multiple-value-t-nil-p \(values 8 8 3\)\)~%
 \(multiple-value-t-nil-p t\)~%
 \(multiple-value-t-nil-p \(values\)\)~%
 \(multiple-value-t-nil-p \(values nil\)\)~%
 \(macroexpand-1 '\(multiple-value-t-nil-p \(values t nil 8\)\)\)~%~@
:SEE-ALSO `mon:multiple-value-nth-p', `mon:multiple-value-nil-nil-p',
`mon:multiple-value-t-t-p', `mon:multiple-value-t-nil-p',
`mon:multiple-value-nil-t-p', `mon:multiple-value-nil-nil-p', `cl:values',
`cl:values-list', `cl:nth-value', `cl:multiple-value-bind',
`cl:multiple-value-list'.~%▶▶▶")

(fundoc 'multiple-value-t-t-p
        "Whether first two values returned by EXPR are both t.~%~@
If expr does not return at least two values return nil.~%~@
:EXAMPLE~%
 \(multiple-value-t-t-p \(values t t\)\)~%
 \(multiple-value-t-t-p \(values t t 8\)\)~%
 \(multiple-value-t-t-p t\)~%
 \(multiple-value-t-t-p \(values 8 8 3\)\)~%
 \(multiple-value-t-t-p \(values\)\)~%
 \(multiple-value-t-t-p \(values nil\)\)~%
 \(macroexpand-1 '\(multiple-value-t-t-p \(values t nil 8\)\)\)~%~@
:SEE-ALSO `mon:multiple-value-nth-p', `mon:multiple-value-nil-nil-p',
`mon:multiple-value-t-t-p', `mon:multiple-value-t-nil-p',
`mon:multiple-value-nil-t-p', `mon:multiple-value-nil-nil-p', `cl:values',
`cl:values-list', `cl:nth-value', `cl:multiple-value-bind',
`cl:multiple-value-list'.~%▶▶▶")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
