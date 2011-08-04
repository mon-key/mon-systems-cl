;;; :FILE-CREATED <Timestamp: #{2011-04-21T16:46:29-04:00Z}#{11164} - by MON>
;;; :FILE mon-systems/conditions.lisp
;;; ==============================

;;; ==============================
;; :MON-CONDITION-HEIRARCHY
;; error-mon                                  (ERROR)
;; simple-error-mon                           (error-mon)
;; case-error                                 (error-mon)
;; proper-list-error                          (error-mon) 
;; circular-list-error                        (TYPE-ERROR error-mon)
;; slot-non-existent-error                    (CELL-ERROR error-mon)
;; package-error-not                          (TYPE-ERROR simple-error-mon)
;; symbol-not-null-or-string-not-empty-error  (TYPE-ERROR simple-error-mon)
;; plist-error                                (TYPE-ERROR simple-error-mon)
;; plist-not-null-error                       (plist-error)
;; open-stream-output-stream-error            (TYPE-ERROR simple-error-mon)
;;
;;
;;; ==============================
;;   Slot initarg/readers for simple-error-mon and subclasses are as follows:
;; - :W-SYM     error-sym
;; - :W-TYPE    error-sym-type  ;; key with an association in `mon:*error-table*'
;; - :W-SPEC    error-spec      ;; a format-control string or list of strings to be concatenated into one 
;; - :W-ARGS    error-args      ;; format-arguments in the form of a list
;; - :W-GOT     error-got       ;; 
;; - :W-TYPE-OF w-type-of       ;; a boolean
;; 
;;  :SIGNAL-OR-ONLY is available as a key for function `simple-error-mon'.
;;   Possible values are:
;;    { signal signal-only condition condition-only t nil }
;; When SIGNAL-ONLY is either of the symbols:~%
;;   signal signal-only~%~@
;; CONDITION-OBJ is passed to `cl:signal' and associated handlers can
;; respond accordingly.
;; When SIGNAL-ONLY is any of the symbols:~%
;;  condition condition-only T~%~@
;; CONDITION-OBJ is returned and no additional invocation of `cl:error' or `cl:signal' occurs.
;;
;;; ==============================

;;; ==============================


(in-package #:mon)


;;; ==============================
;;; :CONDITION-DEFINITIONS
;;; ==============================

;;; :NOTE Following are defined in :FILE sbcl/src/code/seq.lisp
;; bad-sequence-type-error
;; sequence-type-length-mismatch-error
;; signal-index-too-large-error
;; sequence-bounding-indices-bad-error
;; array-bounding-indices-bad-error
;;; ==============================
;; :NOTE The :format-control in the following errror:
;; (error 'simple-program-error
;;        :format-control
;;        (format nil "~~@<In DEFCLASS ~~S, the slot name ~~S ~
;;                                is ~A.~~@:>" reason)
;;        :format-arguments (list class-name name))
;;; ==============================
(define-condition error-mon (error)
  ((w-sym
    :initarg :w-sym 
    :initform nil
    :reader error-sym)
   (w-type 
    :initarg :w-type 
    :initform nil
    :reader error-sym-type)
   (w-spec 
    :initarg :w-spec 
    :initform nil 
    :reader error-spec)
   (w-args 
    :initarg :w-args 
    :initform nil 
    :reader error-args))
  (:documentation
   #.(format nil
             "Base class for mon related errors.~%~@
Direct slot initarg/readers are as follows:
 - :W-SPEC ERROR-SPEC is a format-control spec.~%~@
 - :W-ARGS ERROR-ARGS are arguments to format-control spec of slot W-SPEC.~% 
 - :W-SYM  ERROR-SYM name a symbol originating the condition.~%~@
 - :W-TYPE ERROR-TYPE name the type of W-SYM. 
   When provided W-TYPE is a symbol with an association in `mon:*error-table*'~%~@
   When ommitted may default to to value of: 
    \(cdr \(assoc 'nil mon:*error-table*\)\)~%~@
:SEE-ALSO `mon:simple-error-mon'.~%▶▶▶")))

(define-condition simple-error-mon (error-mon)
  ((w-got 
    :initarg :w-got
    :initform nil
    :reader error-got)
   (w-type-of
    :initarg :w-type-of
    :initform nil
    :reader error-type-of))
  (:report (lambda (sem-condition sem-stream) 
             (declare (condition sem-condition) (stream sem-stream))
             (simple-error-mon-report sem-condition sem-stream)))
  (:documentation 
   #.(format nil
             "Like condition `mon:error-mon' but with :report formatting capabilities.~%~@
Invokes `mon:simple-error-mon-report' when formatting for :report.~%~@
Accessible via convenience function `mon:simple-error-mon'.~%~@
Direct slot initargs/readers are as follows:~%
 - :W-GOT ERROR-GOT
   If T and W-TYPE-OF is not a boolean value use its else use provided value.~%
 - :W-TYPE-OF ERROR-TYPE-OF 
    If T get the `cl:tyep-of' W-GOT, else if provided get `cl:type-of' for object.~%~@
Inherited slot initargs/readers may be formatted by `mon:simple-error-mon-report' when provided.
Formats slot initargs/readers of direct superclass `mon:error-mon' as follows:~%
 - :W-SYM  ERROR-SYM
 - :W-TYPE ERROR-TYPE
 - :W-SPEC ERROR-SPEC
 - :W-ARGS ERROR-ARGS~%~@
:SEE-ALSO `mon:*error-table*'.~%▶▶▶")))


;; :SOURCE CLOCC/cllib/port/ext.lisp
;; :NOTE :CALLED-BY `sequence-type' :FILE types.lisp
(define-condition case-error (error-mon)
  ((case-error-spec
    :type string
    :initform (concatenate 'string "case-arg: ~S~%got:~,,5@S~%type-of: ~S~%wanting: "
                           (cdr (assoc '~S-or-w-list *format-delimited-english-list-templates*)))
    :reader case-spec)
   (case-error-args 
    :type proper-list
    :initarg :w-args
    :reader case-args))
  (:report (lambda (ce-condtion ce-stream)
             (declare (condition ce-condtion)
                      (stream    ce-stream))
             (format ce-stream "~A~@[~?~]"
                     (format-error-symbol-type (error-sym ce-condtion) (error-sym-type ce-condtion))
                     (case-spec   ce-condtion)
                     `(,(first     (case-args ce-condtion))
                        ,(second   (case-args ce-condtion))
                        ,(type-of  (second (case-args ce-condtion)))
                        ,(nthcdr 2 (case-args ce-condtion))))))
  (:documentation
   #.(format nil 
              "An error for use in `cl:typecase' and `cl:case' forms.~%~@
              This error carries the function name helping to make error messages more useful.~%~@
              SLOT :CASE-ERROR-ARGS is a proper list with the the format:~%
               \( <VAR>  <VAL>  <CASE0>  <CASE1>  <CASE2> ... <CASEN> \)~%~@
              :EXAMPLE~%
                \(let \(\(seq \(make-hash-table\)\)\)
                  \(typecase seq
                    \(string 'string\)
                    \(list 'list\)
                    \(vector \(let \(\(eltype \(array-element-type seq\)\)\)
                              \(if \(eq eltype t\) 'vector \(list 'vector eltype\)\)\)\)
                    \(t \(error 'case-error
                              :w-sym 'sequence-type
                              :w-type 'function
                              :w-args \(list 'seq seq 'string 'vector 'list\)\)\)\)\)~%~@
              :SEE-ALSO `mon:code-error'.~%▶▶▶")))

;; :TODO This should inherit `cl:type-error'.
;;       The PROPER-SPEC reader should be refactored in lieu of
;;       `simple-program-mon' error-got error-type-of accessors
(define-condition proper-list-error (error-mon) 
  ((proper-list-error-spec
    :type string
    ;; (ARG VAL)
    :initform "arg ~S not `list-proper-p',~%got: ~S~%type-of: ~A"
    :reader proper-spec)
   (proper-list-error-args
    :type proper-list
    :initarg :proper-list-error-args
    :reader proper-args))
  (:report (lambda (ple-condition ple-stream)
             (declare (type stream ple-stream)
                      (type condition ple-condition))
             (proper-list-error-report ple-condition ple-stream)))
  (:documentation
   #.(format nil 
"Error condition for objects which do not satisfy `mon:list-proper-p'.~%
slot proper-list-error-args is a list of the form:
 \(ARG VAL\)~%
Where ARG names an object not of type `mon:proper-list' and VAL is its value.~%
:EXAMPLE~%
 \(error 'proper-list-error
       :w-sym 'bubba
       :w-type 'function
       :proper-list-error-args `\(bubb-arg ,\(make-hash-table\)\)\)~%~@
:SEE-ALSO `mon:case-error', `mon:slot-non-existent-error',
`mon:simple-error-mon' `mon:simple-error-mon-report', `mon:error-mon'.~%▶▶▶")))

(define-condition circular-list-error (type-error error-mon)
  ()
  (:report (lambda (cle-condition cle-stream)
             (declare (condition cle-condition)
                      (stream cle-stream))
             (let ((es       (error-sym        cle-condition))
                   (et       (error-sym-type   cle-condition))
                   (ted      (type-error-datum cle-condition))
                   (expect   '(and list (not circular-list))))
               (when ted 
                 (setf ted (write-to-string ted 
                                            :lines 1 
                                            :circle t 
                                            :pretty t 
                                            :array nil 
                                            :readably nil)))
               (setf es (format-error-symbol-type es et))
               (setf et (mapconcat #'identity 
                                   (list es "~%" 
                                         (and ted (format nil "The value: ~A~%" ted))
                                         (format nil "Is circular-list-p and not of type:~% ~A" expect))
                                   (make-string 0)))
               (format cle-stream et))))
  ;; The standard says one can rely on the accessors and initargs of a condition
  ;; to exist if specified, but _not_ on these being a _slots_ which might be
  ;; accessed or initialized. 
  ;; :SEE (URL `http://www.lispworks.com/documentation/HyperSpec/Issues/iss077_w.htm')
  ;; Therefor, the provision of :default-initargs below prevents SBCL from signalling:
  ;;   unbound condition slot: SB-KERNEL::EXPECTED-TYPE
  ;;      [Condition of type SIMPLE-ERROR]
  (:default-initargs :expected-type '(and list (not circular-list)))
  (:documentation
   #.(format nil 
             "Condition for objects of type `mon:circular-list'.~%~@
   Inherits the :DATUM slot/initarg from `cl:type-error'~%~@
   Reports the W-SYM and W-TYPE slot/initargs from `mon:error-mon'.~%~@
   :EXAMPLE~%
    \(let \(\(crcl \(alexandria:circular-list 'a 'b\)\)\)
       \(error \(make-condition 'circular-list-error :w-sym 'bubba
                                                     :w-type 'function
                                                     :datum crcl\)\)\)~%~@
   :SEE-ALSO `mon:list-circular-p', `mon:proper-list-error'.~%▶▶▶")))

(define-condition slot-non-existent-error (cell-error error-mon)
  ;; :NOTE Don't add ":initform nil" to these conditions slots b/c we don't want
  ;; to intropsect on nil.
  ((w-obj
    :initarg :w-obj
    :reader w-object-locus)
   (w-not-slot-value
    :initarg :w-not-slot-value
    :reader value-for-not-slot))
  (:report (lambda (snee-condition snee-stream)
             (declare (condition snee-condition)
                      (stream snee-stream))
             (let* ((c-e-n (cell-error-name snee-condition))
                    (w-o-l (w-object-locus snee-condition))
                    (w-o-c (and w-o-l (class-name-of w-o-l)))
                    (w-not (value-for-not-slot snee-condition))
                    (fmt  `(,(and w-o-c (cons ":SLOT-NOT-OF-CLASS~24T~S" w-o-c))
                             ,(and w-o-l (cons ":SLOT-NOT-OF-OBJECT~24T~S" w-o-l))
                             ,(and c-e-n (cons ":SLOT-THATS-NOT~24T~S" c-e-n))
                             ,(and w-not (cons ":SLOT-DONT-GOT~24T~S" w-not)))))
               (apply #'format snee-stream
                      (mapconcat #'car fmt "~%")
                      (mapcar #'cdr fmt)))))
  (:documentation
   #.(format nil
             "Condition for slots not satisfying `slot-exists-p'.~%~@
   :NAME names the offending non-existent slot.~
   Its reader is `cl:cell-error-name'.~%~@
   :W-OBJ is the object that doesn't contain slot.~
   Its reader is w-object-locus.~
   :W-NOT-SLOT-VALUE is the value that slot would get if existent.~
   Its reader is value-for-not-slot.~%~@
   :EXAMPLE~%
    \(let \(\(object *dbc-xml-dump-dir*\)\)
      \(error \(make-condition 'slot-non-existent-error 
                             :name 'non-existent-slot
                             :w-obj object
                             :w-not-slot-value 42\)\)\)~%~@
   :SEE-ALSO `mon:error-mon', `mon:code-error',`cl:unbound-slot', `cl:unbound-variable',
   `cl:undefined-function'.~%▶▶▶")))

(define-condition package-error-not (type-error simple-error-mon)
  ()
  (:report (lambda (pen-condition pen-stream) 
             (declare (condition pen-condition)
                      (stream pen-stream))
             (package-error-not-report pen-condition pen-stream)))
  ;; Can't initalize these:
  ;; :w-type-of t :w-got (type-error-datum condition)
  (:default-initargs :expected-type 'package)
  (:documentation 
   #.(format nil
"A type-error condition with `mon:simple-error-mon' and `mon:error' slot access.~%~@
Sigaled by function `mon:package-error-not'.~%~@
:report is as per `package-error-not-report'.~%~@
Report `cl:type-error-datum' and `cl:type-of' for `cl:type-error-datum' as if by
the `mon:error-got' and `mon:error-type-of' readers for `mon:simple-error-mon'.
:SEE-ALSO `<XREF>'.~%▶▶▶")))

(define-condition symbol-not-null-or-string-not-empty-error (type-error simple-error-mon)
  ((symbol-nor-string-locus
    :initarg :symbol-nor-string-locus
    :initform nil
    :reader symbol-nor-string-error-locus))
  (:report (lambda (snn-condition snn-stream)
             (declare (condition snn-condition)
                      (stream snn-stream))
             (symbol-not-null-or-string-not-empty-error-report snn-condition snn-stream)))
  (:default-initargs :expected-type 'symbol-not-null-or-string-not-empty)
   (:documentation 
   #.(format nil
"A type-error condition with `mon:simple-error-mon' and `mon:error' slot access.~%~@
Slot initarg/reader SYMBOL-NOR-STRING-LOCUS and SYMBOL-NOR-STRING-ERROR-LOCUS access the
locus of the error.~%~@
Signaled by function `mon:symbol-not-null-or-string-not-empty-error'.~%~@
:report is as per `mon:symbol-not-null-or-string-not-empty-error-report'.~%~@
Report `cl:type-error-datum' and `cl:type-of' for `cl:type-error-datum' as if by
the `mon:error-got' and `mon:error-type-of' readers for `mon:simple-error-mon'.
:SEE-ALSO `<XREF>'.~%▶▶▶")))

(define-condition plist-error (type-error simple-error-mon)
  ((plist-obj-locus
    :initarg :plist-obj-locus
    :reader plist-error-locus))
  (:report (lambda (condition stream)
             (declare (type stream stream)
                      (type condition condition))
             (plist-error-report condition stream)))
  (:default-initargs :expected-type 'proper-plist)
  (:documentation 
   #.(format nil
"A type-error condition with all slots of `mon:simple-error-mon' and `mon:error'~%~@
Slot initarg/reader W-OBJ-LOCUS and PLIST-ERROR-LOCUS access the locus of the error
Signaled by function `mon:plist-error'.~%~@
:report is as per `mon:plist-error-report'.~%~@
Report `cl:type-error-datum' and `cl:type-of' for `cl:type-error-datum' as if by
the `mon:error-got' and `mon:error-type-of' readers for `mon:simple-error-mon'.
:SEE-ALSO `mon:plist-proper-p', `mon:proper-plist'.~%▶▶▶")))

(define-condition plist-not-null-error (plist-error)
  ((plist-obj-locus
    :initarg :plist-obj-locus
    :reader plist-error-locus))
  (:report (lambda (pnne-condition pnne-stream)
             (declare (type stream pnne-stream)
                      (type condition pnne-condition))
             (plist-error-report pnne-condition pnne-stream)))
  (:default-initargs :datum 'nil :expected-type 'proper-plist-not-null)
  (:documentation 
   #.(format nil
             "A type-error condition with all slots of `mon:simple-error-mon' and `mon:error'~%~@
Like `mon:plist-error' condition but for use when plist is the empty-list.~%~@
Unlike `mon:plist-error' the :datum key of the `cl:type-error' slot is defaulted to 'nil.~%~@
Slot initarg/reader W-OBJ-LOCUS and PLIST-ERROR-LOCUS access the locus of the error.
Signaled by function `mon:plist-not-null-error'.~%~@
:report is as per `mon:plist-error-report'.~%~@
Report `cl:type-error-datum' and `cl:type-of' for `cl:type-error-datum' as if by
the `mon:error-got' and `mon:error-type-of' readers for `mon:simple-error-mon'.
:SEE-ALSO `mon:plist-proper-p', `mon:proper-plist'.~%▶▶▶")))
;;
;; (plist-not-null-error  

(define-condition open-stream-output-stream-error (type-error simple-error-mon)
  ((pathname-arg-locus
    :initarg :pathname-arg-locus
    :reader pathname-arg-locus))
  (:report (lambda (osose-condition osose-stream)
             (declare (type condition osose-condition)
                      (type stream osose-stream))
             (open-stream-output-stream-error-report osose-condition osose-stream)))
  (:default-initargs :expected-type 'stream)
  (:documentation
   #.(format nil "A type-error condition with all slots of `mon:simple-error-mon' and `mon:error'~%~@
                  Signaled by function `mon:open-stream-output-stream-error'.~%~@
                  :report is as per `mon:open-stream-output-stream-error'.~@
                  Report `cl:type-error-datum' and `cl:type-of' for `cl:type-error-datum' as if by~@
                  the `mon:error-got' and `mon:error-type-of' readers for `mon:simple-error-mon'.~%~@
                  :SEE-ALSO `mon:open-stream-output-stream-p', `mon:stream-or-boolean',~@
                  `cl:streamp', `cl:open-stream-p', `cl:output-stream-p'.~%▶▶▶")))

(define-condition file-error-wild-pathname (file-error simple-error-mon)
  ((pathname-wild-arg-locus
    :initarg :pathname-wild-arg-locus
    :reader pathname-wild-arg-error-locus))
  ;; 
  (:report  (lambda (fewp-condition fewp-stream )
              (declare (condition fewp-condition)
                       (stream fewp-stream))
              (file-error-wild-pathname-report fewp-condition fewp-stream)))
  ;; (:default-initargs :w-spec "Arg was `cl:wild-pathname-p'")
  (:documentation 
   #.(format nil "A condition for objects which are `cl:wild-pathname-p'.~@
                  Value of slot :PATHNAME designates the object of the condition,~@
                  it may be accessed with the reader `cl:file-error-pathname'.~@
                  Value of slot :PATHNAME-WILD-LOCUS designates the locus of the~@
                  condition, e.g. if a signaller is a function with an argument~@
                  WANTS-NON-WILD-PATHNAME this is its locus.~@
                  It may be accessed with the reader `mon:pathname-wild-arg-error-locus'.~%~@
                  :NOTE The convenience function `mon:file-error-wild-pathname'
                  is the established interface to this condition as it will take
                  care to pass the correct arguments for a reasonable invocation
                  of `file-error-wild-pathname-report'.~%~@
                  :SEE-ALSO `<XREF>'.~%▶▶▶")))


;;; ==============================
;;; :CONDITION-FUNCTIONS
;;; ==============================

;; :SOURCE sbcl/src/code/fd-stream.lisp `sb-impl::ensure-one-of'
;; :MODIFICATIONS 
;;  :ADDED arg signal-or-only :REMOVED arg of-type 
;;  :CHANGED intitargs :expected-type :format-arguments
(defun ensure-one-of (ensure-item in-list &key signal-or-only)
  (or 
   (and (member ensure-item in-list))
   (let ((eoo-sys-type-err 
          (make-condition 'simple-type-error
                          :datum            ensure-item
                          :expected-type   `(member ,@in-list)
                          :format-control   "~@<Can not satisfy test of form:~_ \(member ~S ~S\)~
                                             ~_Need ~S to be `cl:eql' one of:~_ ~A~:>"
                          :format-arguments  (list ensure-item
                                                   in-list
                                                   ensure-item
                                                   (format nil (format-delimited-english-list "~S" "or") in-list)))))
     (declare (type condition eoo-sys-type-err))
     (funcall (function signal-error-or-condition) signal-or-only eoo-sys-type-err))))

(defun ensure-signal-or-only (&optional signal-or-only)
  (when signal-or-only
    (let ((s-or-o-chk (list 'signal 'signal-only 'condition 'condition-only t)))
      (case (car (ensure-one-of signal-or-only s-or-o-chk))
        ((signal signal-only)         'signal)
        ((condition condition-only t)  t)))))

(defun signal-error-or-condition (chk-signal-or-only condition-obj)
  (let ((chkd (ensure-signal-or-only chk-signal-or-only)))
    (funcall 
     (or 
      (and (null chkd)
           #'error)
      (and (eql chkd 'signal)
           #'signal)
      (and chkd
           #'identity)
      (simple-error-mon :w-sym  'signal-error-or-condition 
                        :w-type 'function 
                        :w-spec "Shouldn't happen, something wrong with arg CHK-SIGNAL-OR-ONLY"))
     condition-obj)))

;;; ==============================
;; :NOTE Either swank or SBCL seems to be doing something non-conformat wrt
;; formatting conditions and pathnames and no amount of binding the `*print-<>*'
;; seems to help...
;; 22.1.3.11 Printing Pathnames 
;; "When printer escaping is disabled, `write' writes a pathname P by writing `(namestring P)' instead."
;;
;; (info "(ansicl)define-condition")
;; "... function is called whenever the condition is printed while `*print-escape*' is `nil'."
(defun simple-error-mon-report (condition stream)
  (declare (special *error-table*)
	   (stream stream)
	   (condition condition))
  (let ((semr-err-sym           (error-sym  condition))
	(semr-err-spec          (error-spec condition))
	(semr-err-arg           (error-args condition))
        (semr-err-sym-type      (error-sym-type condition))
        (semr-err-got           (ref-bind semr-eg (error-got condition) 
                                  (format nil "~%got: ~S" semr-eg)))
        (semr-err-sym-get-type  (ref-bind semr-eto (error-type-of condition)
                                  (format nil "~%type-of: ~S" semr-eto))))
    (setf semr-err-sym (format-error-symbol-type semr-err-sym semr-err-sym-type))
    (setf semr-err-spec (or 
                         (and semr-err-spec 
                              (or 
                               (and (stringp semr-err-spec)
                                    (concatenate 'string semr-err-sym semr-err-spec semr-err-got semr-err-sym-get-type))
                               (and (list-proper-p semr-err-spec)
                                    (mapconcat #'identity 
                                               `(,semr-err-sym ,@semr-err-spec ,semr-err-got ,semr-err-sym-get-type)
                                               (make-string 0 :initial-element #\Nul)))))
                         (make-string 0 :initial-element #\Nul)))
    (apply #'format stream semr-err-spec semr-err-arg)))

(defun format-error-symbol-type (&optional error-symbol symbol-type)
  (declare (special *error-table*))
  (let  ((fest-type-check (assoc symbol-type *error-table*)))
    (setf fest-type-check 
          (or 
           (and fest-type-check (cdr fest-type-check))
           ":LOCUS"))
    (setf fest-type-check
          (or 
           (and error-symbol 
                (concatenate 'string  fest-type-check " `" (upcase error-symbol) "' -- "))
           (concatenate 'string fest-type-check " -- ")))))

(defun simple-error-mon (&key 
                         (w-sym 'simple-error-mon)
                         (w-type 'condition)
                         w-spec w-args w-got w-type-of signal-or-only )
  (handler-case 
      (let ((sem-cndtn 
             (make-condition 'simple-error-mon
                             :w-sym w-sym
                             :w-type w-type
                             :w-got  (or 
                                      (and w-got 
                                           (not (eql w-got t))
                                           w-got)
                                      (and w-got 
                                           (eql w-got t) 
                                           (and (booleanp w-type-of)
                                                nil))
                                      (and w-type-of 
                                           (not (eql w-type-of t))
                                           w-type-of))
                             :w-type-of (and w-type-of
                                             (or 
                                              (and (eql w-type-of t)
                                                   (and w-got)
                                                   (type-of w-got))
                                              (type-of w-type-of)))
                             :w-spec (or
                                      (and w-spec 
                                           (or 
                                            (and (stringp w-spec)       w-spec)
                                            (and (list-proper-p w-spec) w-spec)
                                                 
                                            (proper-list-error :w-sym          'simple-error-mon
                                                               :w-type         'function 
                                                               :error-args     `(w-spec ,w-spec)
                                                               :signal-or-only 'signal)))
                                      (and (or w-got
                                               w-type-of)
                                           (make-string 0 :initial-element #\Nul))
                                      (list "simple-error-mon w/out format-control argument"))
                             :w-args (and w-spec 
                                          (and w-args
                                               (or 
                                                (and (list-proper-p w-args)
                                                     w-args)
                                                (and (atom w-args)
                                                     (list w-args))))))))
        (declare (type condition sem-cndtn))
        (signal-error-or-condition signal-or-only sem-cndtn))
    (proper-list-error (cnd) (error cnd))))

(defun circular-list-error (circular-list &key w-sym w-type signal-or-only) ;; 
  (let ((cle-cndtn
         (make-condition 'circular-list-error
                         :w-sym  (or 
                                  (and w-sym w-type w-sym)
                                  'circular-list-error)
                         :w-type (or 
                                  (and w-sym w-type)
                                  'condition)
                         :datum  circular-list)))
    (declare (type condition cle-cndtn))
    (signal-error-or-condition signal-or-only cle-cndtn)))

(defun proper-list-error-report (condition stream)
  (declare (type condition condition)
           (type stream stream))
  (format stream "~A~@[ ~?~]"
          (format-error-symbol-type (error-sym condition) (error-sym-type condition))
          (proper-spec   condition)
          (list (first   (proper-args condition))
                (second  (proper-args condition))
                (type-of (second (proper-args condition))))))

(defun proper-list-error (&key w-sym w-type error-args signal-or-only)
  (handler-case
      (let ((ple-cndtn
             (make-condition 
              'proper-list-error
              :w-sym  (or 
                       (and w-sym w-type w-sym)
                       'proper-list-error)
              :w-type (or 
                       (and w-sym w-type w-type)
                       'condition)
              :proper-list-error-args (or 
                                       (and (list-length-n-p error-args 2)
                                            error-args)
                                       (simple-error-mon :w-sym    'proper-list-error
                                                         :w-type   'function
                                                         :w-spec   '("with pending un-signaled condtion of type ~"
                                                                     "PROPER-LIST-ERROR~%"
                                                                     "Arg ERROR-ARGS not a 2 element proper-list")
                                                         :w-got     error-args
                                                         :w-type-of error-args
                                                         :signal-or-only 'signal)))))
        (declare (type condition ple-cndtn))
        (signal-error-or-condition signal-or-only ple-cndtn))
    (simple-error-mon (cnd) (error cnd))))

(defun symbol-not-null-or-error (chk-symbol &key w-locus
                                 signal-or-only)
  (or 
   (typep chk-symbol 'symbol-not-null)
   (simple-error-mon :w-sym         'symbol-not-null-or-error
                     :w-type        'function
                     :w-spec        "Arg ~A not of type `mon:symbol-not-null'"
                     :w-args         (or 
                                      (and w-locus 
                                           (typecase w-locus
                                             (symbol (string w-locus))
                                             (string (string-upcase w-locus))
                                             (t nil)))
                                      "CHK-SYMBOL")
                     :w-got           (or 
                                       (and (null chk-symbol) 
                                            (quote (quote nil)))
                                       chk-symbol)
                     :w-type-of        (unless (null chk-symbol) t)
                     :signal-or-only   signal-or-only)))

(defun string-empty-error (&key w-sym w-type w-locus signal-or-only)
  (handler-case 
      (let ((stree-cndtn 
             (make-condition 'simple-error-mon
                             :w-sym (or w-sym 'string-empty-error)
                             :w-type (if (and w-sym w-type) 
                                         w-type 
                                         (if w-sym 
                                             nil 
                                             'function))
                             :w-spec "arg~A was `string-empty-p'"
                             :w-args (list 
                                      (or 
                                       (and w-locus
                                            (concatenate 'string (make-string 1 :initial-element #\SPACE)
                                                         (or 
                                                          (and (symbolp w-locus)
                                                               (symbol-name w-locus)) 
                                                          (and (stringp w-locus)
                                                               (string-upcase w-locus))
                                                          (simple-error-mon  :w-sym         'string-empty-error 
                                                                             :w-type        'function
                                                                             :w-spec        "Keyword :W-LOCUS neither `stringp' nor `symbolp'"
                                                                             :w-got          w-locus
                                                                             :w-type-of      t
                                                                             :signal-or-only 'signal))))
                                       (make-string 0 :initial-element #\nul))))))
        (declare (type condition stree-cndtn))
        (signal-error-or-condition signal-or-only stree-cndtn))
    (simple-error-mon (cnd) (error cnd))))

(defun package-error-not-report (condition stream)
  (declare (type stream stream)
           (type condition condition))
  (let ((penr-err-sym      (error-sym      condition))
	(penr-err-spec     (error-spec     condition))
	(penr-err-arg      (error-args     condition))
        (penr-err-type     (error-sym-type condition))
        (penr-type-expect  (format nil "~%type-expected:~18T~S" (type-error-expected-type condition)))
        (penr-err-got      (format nil "~%got:~18T~S"           (type-error-datum condition)))
        (penr-err-get-type (format nil "~%type-of:~18T~S"       (type-of (type-error-datum condition)))))
    (setf penr-err-sym  (format-error-symbol-type penr-err-sym penr-err-type))
    (setf penr-err-spec (or 
                         (and penr-err-spec 
                              (or 
                               (and (stringp penr-err-spec)
                                    (concatenate 'string penr-err-sym penr-err-spec penr-type-expect penr-err-got penr-err-get-type))
                               (and (listp penr-err-spec)
                                    (mapconcat #'identity 
                                               `(,penr-err-sym ,@penr-err-spec ,penr-type-expect ,penr-err-got ,penr-err-get-type) 
                                               (make-string 0 :initial-element #\Nul)))))
                         (mapconcat #'identity 
                                    `(,penr-err-sym ,penr-type-expect ,penr-err-got ,penr-err-get-type) 
                                    (make-string 0 :initial-element #\Nul))))
    (apply #'format stream penr-err-spec penr-err-arg)))

(defun package-error-not (not-a-package &key w-sym w-type w-spec w-args signal-or-only)
  (handler-case
      (let ((pern-cndtn 
             (make-condition 'package-error-not
                             :w-sym  (or (and w-sym w-type w-sym) 'package-error-not)
                             :w-type (or (and w-sym w-type)       'condition)
                             :w-spec (and w-spec 
                                          (or 
                                           (and (stringp w-spec)
                                                w-spec)
                                           (and (listp w-spec)
                                                (or 
                                                 (and (list-proper-p w-spec)
                                                      w-spec)
                                                 (proper-list-error :w-sym         'package-error-not
                                                                    :w-type         'function
                                                                    :error-args     `(w-spec ,w-spec)
                                                                    :signal-or-only 'signal)))))
                             :w-args w-args
                             :datum  not-a-package)))
        (declare (type condition pern-cndtn))
        (signal-error-or-condition signal-or-only pern-cndtn))
    (proper-list-error (cnd) (error cnd))))

(defun plist-error-report (condition stream)
  (declare (type stream stream)
           (type condition condition))
  (let ((per-err-sym         (error-sym         condition))
        (per-err-spec        (error-spec        condition))
        (per-err-arg         (error-args        condition))
        (per-err-type        (error-sym-type    condition))
        (per-err-default     (plist-error-locus condition))
        (per-err-type-expect (format nil "~%type-expected:~18T~S" (type-error-expected-type condition)))
        (per-err-got         (format nil "~%got:~18T~S" (type-error-datum condition)))
        (per-err-get-type    (format nil "~%type-of:~18T~S" (type-of (type-error-datum condition)))))
    (setf per-err-sym (format-error-symbol-type per-err-sym per-err-type))
    (setf per-err-spec (or 
                        (and per-err-spec 
                             (or
                              (and (stringp per-err-spec)
                                   (concatenate 'string per-err-sym per-err-spec per-err-default per-err-type-expect per-err-got per-err-get-type))
                              (and (list-proper-p per-err-spec)
                                   (mapconcat #'identity 
                                              `(,per-err-sym ,@per-err-spec ,per-err-default ,per-err-type-expect ,per-err-got ,per-err-get-type) 
                                              (make-string 0 :initial-element #\Nul)))
                              (proper-list-error :w-sym         'simple-error-mon
                                                 :w-type        'function
                                                 :error-args    `(error-spec ,per-err-spec)
                                                 :signal-or-only nil)))
                        (mapconcat #'identity 
                                   `(,per-err-sym ,per-err-default ,per-err-type-expect ,per-err-got ,per-err-get-type) 
                                   (make-string 0 :initial-element #\Nul))))
    (apply #'format stream per-err-spec per-err-arg)))

(defun plist-not-null-error (&key w-sym w-type w-obj-locus w-spec w-args signal-or-only)
  (handler-case 
      (let ((pnne-cndtn (make-condition 
                     'plist-not-null-error
                     :w-sym  (or (and w-sym w-type w-sym) 'plist-not-null-error)
                     :w-type (or (and w-sym w-type) 'condition)
                     :plist-obj-locus 
                             (if w-obj-locus
                                 (format nil "object ~A not `plist-proper-not-null-p'"
                                         (typecase w-obj-locus
                                           (string (string-upcase w-obj-locus))
                                           (symbol (string w-obj-locus))
                                           (t      (symbol-not-null-or-string-not-empty-error
                                                    w-obj-locus
                                                    :w-spec        '("with pending un-signaled PLIST-NOT-NULL-ERROR condtion~%"
                                                                     "got invalid keyword argument~%")
                                                    :w-obj-locus    'w-obj-locus
                                                    :signal-or-only 'signal))))
                                 "arg not `plist-proper-not-null-p'")
                     :w-spec w-spec
                     :w-args w-args)))
        (declare (type condition pnne-cndtn))
        (signal-error-or-condition signal-or-only pnne-cndtn))
    (symbol-not-null-or-string-not-empty-error (cnd) (error cnd))))

(defun plist-error (not-a-plist &key w-sym w-type w-obj-locus w-spec w-args signal-or-only)
  (handler-case 
      (let ((pe-cndtn 
             (make-condition 'plist-error
                             :w-sym           (or (and w-sym w-type w-sym) 'plist-error)
                             :w-type          (or (and w-sym w-type) 'condition)
                             :plist-obj-locus (if w-obj-locus
                                                  (format nil "object ~A not `plist-proper-p'" 
                                                          (typecase w-obj-locus
                                                            (string  (string-upcase w-obj-locus))
                                                            (symbol  (string w-obj-locus))
                                                            (t       (symbol-not-null-or-string-not-empty-error
                                                                      w-obj-locus
                                                                      :w-spec '("with pending un-signaled PLIST-ERROR condtion~%"
                                                                                "got invalid keyword argument~%")
                                                                      :w-obj-locus 'w-obj-locus
                                                                      :signal-or-only 'signal))))
                                                  "arg not `plist-proper-p'")
                             :w-spec           w-spec
                             :w-args           w-args
                             :datum            not-a-plist)))
        (declare (type condition pe-cndtn))
        (signal-error-or-condition signal-or-only pe-cndtn))
    (symbol-not-null-or-string-not-empty-error (cnd) (error cnd))))

(defun symbol-not-null-or-string-not-empty-error-report (condition stream)
  (declare (type stream stream)
           (type condition condition))
  (let ((snn-err-sym         (error-sym      condition))
        (snn-err-spec        (error-spec     condition))
        (snn-err-arg         (error-args     condition))
        (snn-err-type        (error-sym-type condition))
        (snn-err-default     (ref-bind snn-ref-bind (symbol-nor-string-error-locus condition)
                               (format nil "object ~A not of type `mon:symbol-not-null-or-string-not-empty'" snn-ref-bind)
                               "arg not of type `mon:symbol-not-null-or-string-not-empty'"))
        (snn-err-type-expect (format nil "~%type-expected:~18T~S" (type-error-expected-type condition)))
        (snn-err-got         (format nil "~%got:~18T~S"           (type-error-datum condition)))
        (snn-err-get-type    (format nil "~%type-of:~18T~S"       (type-of (type-error-datum condition)))))
    (setf snn-err-sym        
          (format-error-symbol-type snn-err-sym snn-err-type))
    (setf snn-err-spec 
          (or 
           (and snn-err-spec 
                (or 
                 (and (stringp snn-err-spec)
                      (concatenate 'string snn-err-sym snn-err-spec snn-err-default snn-err-type-expect snn-err-got snn-err-get-type))
                 (and (list-proper-p snn-err-spec)
                      (mapconcat #'identity 
                                 `(,snn-err-sym ,@snn-err-spec ,snn-err-default ,snn-err-type-expect ,snn-err-got ,snn-err-get-type)
                                 (make-string 0 :initial-element #\Nul)))))
           (mapconcat #'identity 
                      `(,snn-err-sym ,snn-err-default ,snn-err-type-expect ,snn-err-got ,snn-err-get-type)
                      (make-string 0 :initial-element #\Nul))))
    (apply #'format stream snn-err-spec snn-err-arg)))

(defun symbol-not-null-or-string-not-empty-error (non-symbol-or-string 
                                                  &key w-sym w-type w-obj-locus w-spec w-args signal-or-only) 
  (handler-case 
      (let ((snnosnee-cndtn
             (make-condition 'symbol-not-null-or-string-not-empty-error
                             :w-sym  (or (and w-sym w-type w-sym) 
                                         'symbol-not-null-or-string-not-empty-error)
                             :w-type (or (and w-sym w-type) 'condition)
                             :symbol-nor-string-locus 
                             (and w-obj-locus
                                  (typecase w-obj-locus
                                    (string (string-upcase w-obj-locus))
                                    (symbol (string w-obj-locus))
                                    (t      (symbol-not-null-or-string-not-empty-error
                                             w-obj-locus
                                             :w-sym        'symbol-not-null-or-string-not-empty-error
                                             :w-type       'function
                                             :w-spec       '("with pending un-signaled condtion of type: ~
                                                              SYMBOL-NOT-NULL-OR-STRING-NOT-EMPTY-ERROR~%"
                                                             "got invalid keyword argument~%")
                                             :w-obj-locus    'w-obj-locus
                                             :signal-or-only 'signal))))
                             :w-spec (and w-spec
                                          (typecase w-spec
                                            (string w-spec)
                                            (proper-list w-spec)
                                            (t (proper-list-error :w-sym          'simple-error-mon
                                                                  :w-type         'function
                                                                  :error-args     `(error-spec ,w-spec)
                                                                  :signal-or-only 'signal))))
                             :w-args w-args
                             :datum  non-symbol-or-string)))
        (declare (type condition snnosnee-cndtn))
        (signal-error-or-condition signal-or-only snnosnee-cndtn))
    (symbol-not-null-or-string-not-empty-error (cnd) (error cnd))
    (proper-list-error (cnd) (error cnd))))
;; 
(defun open-stream-output-stream-error-report (condition stream)
  (declare (type stream stream)
           (type condition condition))
  (let ((ososer-err-sym         (error-sym      condition))
        (ososer-err-spec        (error-spec     condition))
        (ososer-err-arg         (error-args     condition))
        (ososer-err-type        (error-sym-type condition))
        (ososer-err-type-expect (format nil "~%type-expected:~18T~S" (type-error-expected-type condition)))
        (ososer-err-got         (format nil "~%got:~18T~S"           (type-error-datum condition)))
        (ososer-err-get-type    (format nil "~%type-of:~18T~S"       (type-of (type-error-datum condition)))))
    (setf ososer-err-sym  (format-error-symbol-type ososer-err-sym ososer-err-type))
    (setf ososer-err-spec (or 
                           (and ososer-err-spec 
                                (or 
                                 (and (stringp ososer-err-spec)
                                      (concatenate 'string ososer-err-sym ososer-err-spec ososer-err-type-expect ososer-err-got ososer-err-get-type))
                                 (and (list-proper-p ososer-err-spec)
                                      (mapconcat #'identity  
                                                 `(,ososer-err-sym ,@ososer-err-spec  ,ososer-err-type-expect ,ososer-err-got ,ososer-err-get-type) 
                                                 (make-string 0 :initial-element #\Nul)))
                                 (proper-list-error :w-sym         'simple-error-mon
                                                    :w-type        'function
                                                    :error-args    `(error-spec ,ososer-err-spec)
                                                    :signal-or-only nil)))
                           (mapconcat #'identity 
                                      `(,ososer-err-sym ,ososer-err-type-expect ,ososer-err-got ,ososer-err-get-type) 
                                      (make-string 0 :initial-element #\Nul)))) 
    (apply #'format stream ososer-err-spec ososer-err-arg)))

(defun open-stream-output-stream-error (not-an-open-output-stream &rest expect
                                        &key w-sym w-type w-obj-locus w-spec
                                        w-args signal-or-only &allow-other-keys)
  (handler-case 
      (let ((osose-cndtn 
             (make-condition 'open-stream-output-stream-error
                             :w-sym            (or (and w-sym w-type w-sym) 'open-stream-output-stream-error)
                             :w-type           (or (and w-sym w-type) 'condition)
                             :stream-obj-locus (if w-obj-locus
                                                   (format nil "~%object ~S has unsatisfied constraint for either: ~
                                                                 `streamp', `open-stream-p', or `output-stream-p'"
                                                           (typecase w-obj-locus
                                                             (string (string-upcase w-obj-locus))
                                                             (symbol (string w-obj-locus))
                                                             (t      (symbol-not-null-or-string-not-empty-error 
                                                                      w-obj-locus
                                                                      :w-spec        '("with pending un-signaled OPEN-STREAM-OUTPUT-STREAM-ERROR "
                                                                                       "condtion~%got invalid keyword argument~%")
                                                                      :w-obj-locus    'w-obj-locus
                                                                      :signal-or-only 'signal))))
                                                   "arg failed constraints for `streamp', `open-stream-p', or `output-stream-p'")
                             :w-spec            w-spec
                             :w-args            w-args
                             :datum             not-an-open-output-stream
                             :expected-type     (or (and expect (getf expect :expected-type)) 'stream))))
        (declare (type condition osose-cndtn))
        (signal-error-or-condition signal-or-only osose-cndtn))
    (symbol-not-null-or-string-not-empty-error (cnd) (error cnd))))

(defun file-error-wild-pathname-report (condition stream)
  (declare (stream    stream)
           (condition condition)) 
  (let ((fewpr-err-sym-and-type (format-error-symbol-type (error-sym  condition) (error-sym-type condition)))
        (fewpr-err-got          (file-error-pathname condition))
        (fewpr-err-get-type     (type-of (file-error-pathname condition)))
        (fewpr-err-arg-locus    (pathname-wild-arg-error-locus condition)))
    (setf fewpr-err-arg-locus
          (or (and fewpr-err-arg-locus
                   (or (stringp fewpr-err-arg-locus)
                       (symbolp fewpr-err-arg-locus))
                   (upcase fewpr-err-arg-locus))
              ""))
    (format stream 
            (concatenate 'string fewpr-err-sym-and-type
                         "Arg ~A was `cl:wild-pathname-p'~%~T~Tgot: ~S~%~T~Ttype-of: ~S~%")
            fewpr-err-arg-locus fewpr-err-got fewpr-err-get-type)))

(defun file-error-wild-pathname (&key w-sym w-type pathname path-arg-locus signal-or-only)
  (handler-case
      (let ((fewp-cndtn
             (make-condition 'file-error-wild-pathname
                             :w-sym  (or 
                                      (and w-sym w-type w-sym)
                                      'file-error-wild-pathname)
                             :w-type (or 
                                      (and w-sym w-type w-type)
                                      'condition)
                             :pathname (or 
                                        (and pathname pathname)
                                        (simple-error-mon :w-sym        'file-error-wild-pathname
                                                          :w-type       'function
                                                          :w-spec       '("with pending un-signaled condtion of type: ~"
                                                                          "FILE-ERROR-WILD-PATHNAME~%"
                                                                          "got: null value for keyword PATHNAME.~% ~"
                                                                          "Expected `cl:wild-pathname-p' object of type `mon:pathname-or-namestring'")
                                                          :w-got          pathname
                                                          :signal-or-only 'signal))
                             :pathname-wild-arg-locus path-arg-locus
                             ;; :w-args                  path-arg-locus
                             :w-got                   pathname
                             :w-type-of               t
                             :signal-or-only signal-or-only)))
        (declare (type condition fewp-cndtn))
        (signal-error-or-condition signal-or-only fewp-cndtn))
    (simple-error-mon (cnd) (error cnd))))

;; :SOURCE stassats/lisp-config/configs/share.lisp
(defun format-error (stream object &rest rest)
  (declare (ignore rest))
  (format stream (if (typep object 'error) "~A" "~S")
          object))

;; :SOURCE stassats/lisp-config/configs/share.lisp
(defun eval-code (code)
  (let ((result '()))
    (values (with-output-to-string (*standard-output*)
              (setf result (multiple-value-list
                            (handler-case (eval (read-from-string code))
                              (error (error) error)))))
            result)))

;; (defun signal-simple-error (format-control &rest format-args)
;;   (error 'simple-error-mon 
;;          :format-control format-control
;;          :format-arguments format-args))


;;; ==============================
;;; :CONDITIONS-DOCUMENTATION
;;; ==============================

(fundoc 'format-error-symbol-type
"Helper function for `mon:simple-error-mon-report'.~%~@
Optional ERROR-SYMBOL names a symbol originating the signalled condition as per the 
:W-SYM slot and ERROR-SYM reader for conditions of type `mon:error-mon'.~%~@
Optional arg SYMBOL-TYPE is as per its the :W-TYPE slot and ERROR-TYPE reader
name its type and should have an association in `mon:*error-table*'.
If an association is not found or SYMBOL-TYPE is null default to \":LOCUS\"~%~@
:EXAMPLE~%
 \(format-error-symbol-type\)~%
 \(format-error-symbol-type 'bubba\)~%
 \(format-error-symbol-type 'bubba 'function\)~%
 \(format-error-symbol-type 'bubba 'variable\)~%~@
:SEE-ALSO `mon:format-error-symbol-type', `mon:signal-error-or-condition',
`mon:ensure-one-of', `mon:ensure-signal-or-only'.~%▶▶▶")

(fundoc 'ensure-one-of
"ENSURE-ITEM is `cl:eql' an element IN-LIST.~%
Signal a `cl:simple-type-error'.~%~@
Keyword :SIGNAL-OR-ONLY when non-nil indicates to return a condition object as
if by make-condition and an error is not signaled.~%~@
Pass type-error initarg :datum as ENSURE-ITEM.~%~@
Pass type-error initarg :expected-type as a list of the form:~%
 `\(member ,@IN-LIST\)~%~@
:EXAMPLE~%
 \(ensure-one-of 'bubba '\(a b c\)\)~%
 \(let* \(\(the-item 'bubba\)
        \(the-list '\(a b c\)\)
        \(eoo \(ensure-one-of 'bubba '\(a b c\) :signal-or-only t\)\)\)
   \(list :datum \(type-error-datum eoo\)
         :expected-type \(type-error-expected-type eoo\)
         :format-control \(simple-condition-format-control eoo\)
         :format-arguments \(simple-condition-format-arguments eoo\)\)\)~%~@
:SEE-ALSO `mon:format-error-symbol-type', `mon:signal-error-or-condition',
`mon:ensure-signal-or-only', `sb-impl::ensure-one-of'.~%▶▶▶")

(fundoc 'ensure-signal-or-only
"Whether SIGNAL-OR-ONLY is not null and its value is a valid argument.~%~@
When SIGNAL-OR-ONLY is null retrun nil.~%~@
When SIGNAL-OR-ONLY is not null it should be and one of the following symbols:~%
 \{signal signal-only condition condition-only t\}~%~@
An error is signaled if SIGNAL-OR-ONLY if not.~%~@
When SIGNAL-OR-ONLY is either signal or signal-only return the symbol signal.
When SIGNAL-OR-ONLY is either condition, condition-only, or the symbol t return T.~%~@
The latter option is useful when u
:EXAMPLE~%
 \(ensure-signal-or-only 'signal\)~%
 \(ensure-signal-or-only 'signal-only\)~%
 \(ensure-signal-or-only 'condition\)~%
 \(ensure-signal-or-only 'condition-only\)~%
 \(ensure-signal-or-only t\)~%
 \(ensure-signal-or-only\)~%
;; Fails successfully:~%
 \(ensure-signal-or-only 'bubba\)~%~@
:SEE-ALSO `mon:format-error-symbol-type', `mon:signal-error-or-condition',
`mon:ensure-one-of', `mon:ensure-signal-or-only'.~%▶▶▶")

;; (chk-signal-or-only condition-obj)

(fundoc 'signal-error-or-condition
"Depending on value of CHK-SIGNAL-OR-ONLY do something with CONDITION-OBJ.~%~@
CONDITION-OBJ is a condition objection as per the return value of `cl:make-condition'.~%~@
invoke `cl:signal' or `cl:error', or return a condition object.~%~@
When SIGNAL-ONLY is null, CONDITION-OBJ is passed as the argument to to `cl:error'.
When SIGNAL-ONLY is either of the symbols:~%
   signal signal-only~%
CONDITION-OBJ is passed as the argument `cl:signal' and any associated handlers may
respond accordingly.~%
When SIGNAL-ONLY is any of the symbols:~%
  condition condition-only T~%~@
CONDITION-OBJ is returned without additional `cl:signal' or `cl:error' invocation.~%~@
:EXAMPLE~%
 \(let* \(\(s-or-o 'signal\)
        \(list '\(a b c\)\)
        \(item 'bubba\)
        \(ste \(make-condition 
              'simple-type-error
              :datum item
              :expected-type `\(member ,@list\)
              :format-control \"~~@<Can not satisfy test of form:~~_ \\\(member ~~S ~~S\\\)~~
                                                ~~_Need ~~S to be `cl:eql' one of:~~_ ~~A~~:>\"
              :format-arguments `(item list item 
                                       (format nil (format-delimited-english-list \"~~S\" \"or\") list)))))
   \(handler-case \(signal-error-or-condition s-or-o ste\)
     \(simple-type-error \(ste\) 
       \(list :datum            \(type-error-datum ste\) 
             :expected-type    \(type-error-expected-type ste\)
             :format-control   \(simple-condition-format-control ste\)
             :format-arguments \(simple-condition-format-arguments ste\)\)\)\)\)~%~@
:SEE-ALSO `mon:format-error-symbol-type', `mon:ensure-one-of',
`mon:ensure-signal-or-only'.~%▶▶▶")

(fundoc 'simple-error-mon-report
	"Helper function for reporting conditions of type `mon:simple-error-mon'.~%~@
:CALLED-BY `mon:simple-error-mon'.~%~@
:SEE-ALSO `mon:*error-table*'.~%▶▶▶")

(fundoc 'simple-error-mon
"Signal a condition of type `mon:simple-error-mon'.~%~@
Keyword W-SYM is a symbol or string designating the locus of the error.
Default is the symbol simple-error-mon.~%~@
Keyword W-TYPE is a symbol designating the locus type and should have an
association in `mon:*error-table*'.
If W-TYPE is explicilty provided as NIL \(or the empty list\), the signalled
condtion's :format-control is presented with default prefix \":LOCUS\"
If no value is provided defaults to the symbol CONDITION.~%~@
Keyword W-SPEC is a list of strings or a single string.
When W-SPEC is a list of strings elements are coalesced to a single string as if
by `mon:mapconcat'. 
In either format \(string or list of strings\), W-SPEC may
contain format control directives as per the :format-control initarg.~%~@
Keyword W-ARGS is a list of arguments to W-SPEC. Only relevant when W-SPEC is a
format control string. When W-SPEC is ommited, pass default :format-control
string to `cl:error' indicating that no format control was provided thereby
disregarding any arguments which may have been provided for :W-ARGS.~%~@
Keyword W-GOT is a value to report.~%~@
Keyword W-TYPE-OF says to report the `cl:type-of' for W-GOT's value.~%
Keyword SIGNAL-OR-ONLY is a quoted symbol as per the combined interaction of
`mon:signal-error-or-condition' and `mon:ensure-signal-or-only'.~%~@
When SIGNAL-OR-ONLY is not provided or the symbol nil, a condition object is
generated as if by `cl:make-condition' according to the value(s) of the
arguments outlined above and the condition object passed to `cl:error'.~%
If SIGNAL-OR-ONLY is provided it should be one of the following symbols:~%
 \{ signal signal-only condition condition-only t\}~%~@
When SIGNAL-OR-ONLY is non-nil and its value is one of the following symbols:~%
 CONDITION CONDITION-ONLY T 
the value is an indicatation to return a condition object as if by
`cl:make-condition' and no further invocation of `cl:error' or `cl:signal' will
occur.
When SIGNAL-OR-ONLY is one of the the symbols:~%~@
  SIGNAL SIGNAL-ONLY
:NOTE It is often useful to explicitly provide this arg even when null e.g. as:~%
  :signal-or-only nil~%
Doing so affords easier modification when the API of calling functions might
eventually provide restarts, handlers, etc.~%~@
:EXAMPLE~%
 \(simple-error-mon :w-spec \"some error w/out format control or args\"\)~%
 \(simple-error-mon :w-spec \"some error w/out format control keyword :W-ARGS ignored\"
                   :w-args '\(ignore me\)\)~%
 \(simple-error-mon :w-type nil :w-spec \"bubba\"\) ;; expliclitly specify nil~%
 \(simple-error-mon :w-spec \"bubba\"\)             ;; defaulting behaviour~%
 \(simple-error-mon :w-spec '\(\"some error ~~A\"\) :w-args \(list 88\)\)~%
 \(simple-error-mon :w-spec '\(\"some \" \"error, \" \"got: \" \"~~A\"\) :w-args 'an-atom\)~%
 \(simple-error-mon :w-spec '\(\"value A: ~~S~~%\" \"value B: ~~S\"\) :w-args '\(a b\)\)~%
 \(simple-error-mon :w-sym 'bubba :w-spec \"bubba ~~D\" :w-args '\(78\)\)~%
 \(simple-error-mon :w-sym 'bubba :w-type 'function :w-spec \"~~D\" :w-args \(list 88\)\)~%
 \(simple-error-mon :w-sym \"bubba\" :w-type 'variable :w-spec '\(\"~~D \" \"~~S\"\) :w-args \(list 88 #\(\)\)\)~%
 \(simple-error-mon :w-sym \"bubba\" :w-type 'variable :w-spec #\(\) :w-args \(list 88 #\(\)\)\)~%
 \(simple-error-mon :w-sym nil\)~%
 \(simple-error-mon :w-sym 'bubba :w-type 'variable\)~%
 \(simple-error-mon :w-sym 'simple-error-mon :w-type 'function :w-got 'bubba :w-type-of 'bubba\)~%
 \(simple-error-mon :w-sym 'simple-error-mon :w-type 'function :w-got 'bubba :w-type-of t\)~%
 \(simple-error-mon :w-sym 'simple-error-mon :w-type 'function :w-type-of 'bubba\)~%
;; Following fail successfully:~%
 \(simple-error-mon :w-sym nil :w-spec 8\)~%
 \(simple-error-mon :w-spec 8\)~%
 \(simple-error-mon :w-spec #\(\) :w-args \(list 88\)\)~%~@
:SEE-ALSO `mon:simple-error-mon'.~%▶▶▶")

(fundoc 'circular-list-error
"Signal a condition of type `mon:circular-list-error'.~%
CIRCULAR-LIST is an object of type `circular-list'.~%
Keywords :W-SYM and :W-TYPE are as per the like named initargs to
condition `mon:error-mon'.~%~@
If W-SYM is ommited default is the symbol 'circular-list-error.~%
If W-TYPE is ommited default is 'condition.~%
Keyword SIGNAL-OR-ONLY is a symbol as per the combined interaction of
`mon:signal-error-or-condition' and `mon:ensure-signal-or-only', when non-nil
indicates to return a condition object as if by make-condition and an error is
not signaled. Valid arguments for SIGNAL-OR-ONLY are as follows:~%
 \{signal signal-only condition condition-only t nil\}~%~@
:EXAMPLE~%
 \(let \(\(crcl \(alexandria:circular-list 'a 'b\)\)\)
  \(circular-list-error crcl
                       :w-sym 'bubba
                       :w-type 'function\)\)~%
 \(let \(\(crcl \(alexandria:circular-list 'a 'b\)\)\)
  \(circular-list-error crcl\)\)~%~@
:SEE-ALSO `mon:circular-list-p', `mon:list-dotted-p'.~%▶▶▶")

(fundoc 'proper-list-error
"Signal an error of type `mon:proper-list-error'.~%~@
Keywords :W-SYM and :W-TYPE are as per the like named initargs to
condition `mon:error-mon'.~%~@
Keyword :ERROR-ARGS are as :PROPER-LIST-ERROR-ARGS  of `mon:proper-list-error'.
It should satisfy \(list-of-length-n-p error-args 2\) signal an error if not.~%~@
Keyword SIGNAL-OR-ONLY is a symbol as per the combined interaction of
`mon:signal-error-or-condition' and `mon:ensure-signal-or-only', when non-nil
indicates to return a condition object as if by make-condition and an error is
not signaled. Valid arguments for SIGNAL-OR-ONLY are as follows:~%
 \{signal signal-only condition condition-only t nil\}~%~@
:EXAMPLE~%
 \(let* \(\(arg 'bubba\)
        \(arg-val \(make-hash-table\)\)\)
   \(proper-list-error :error-args `\(,arg ,arg-val\)\)\)
 \(let* \(\(arg 'bubba\)~%
        \(arg-val \(make-array 2\)\)\)
   \(proper-list-error :error-args `\(,arg ,arg-val\)\)\)~%
 \(let* \(\(arg 'bubba\)~%
        \(arg-val \(cons 'a 'b\)\)
        \(cond-obj \(proper-list-error :w-sym 'some-fun 
                                     :w-type 'function 
                                     :error-args `\(,arg ,arg-val\)
                                     :signal-or-only t\)\)\)
   \(list \(error-sym cond-obj\) 
         \(error-sym-type cond-obj\)
         \(proper-args cond-obj\)\)\)~%~@
:SEE-ALSO `mon:circular-list-error'.~%▶▶▶")

(fundoc 'symbol-not-null-or-error
"If CHK-SYMBOL is not of type `mon:symbol-not-null' signal an error.~%~@
Keyword W-LOCUS is a string or symbol naming the locus of the error.~%~@
Keyword SIGNAL-OR-ONLY is a symbol as per the combined interaction of
`mon:signal-error-or-condition' and `mon:ensure-signal-or-only', when non-nil
indicates to return a condition object as if by make-condition and an error is
not signaled. Valid arguments for SIGNAL-OR-ONLY are as follows:~%
 \{ signal signal-only condition condition-only t nil \}~%~@
:EXAMPLE~%
 \(symbol-not-null-or-error nil\)~%
 \(symbol-not-null-or-error nil :w-locus 'bubba :signal-or-only nil\)~%~@
 \(symbol-not-null-or-error 8\)~%
 \(symbol-not-null-or-error 'bubba\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'package-error-not
"Signal an error of type `mon:package-error-not'~%~@
:EXAMPLE~%
 \(package-error-not \"bubba\"
                    :w-sym  'package-error-not-EXAMPLE
                    :w-type 'function
                    :w-spec \"Arg PACKAGE provided but `mon:find-package*'~~
                             doesn't find it\"
                    :signal-or-only nil\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'string-empty-error
        "Signal an `simple-mon-error'.~%
Keyword args W-SYM W-TYPE SIGNAL-OR-ONLY are as per `mon:simple-mon-error'~%~@
When W-SYM is ommitted default is 'string-empty-error.~%
When W-SYM is ommitted W-TYPE default to 'function.~%
Arg W-LOCUS is the locus of the value that was `mon:string-empty-p'.~%~@
:EXAMPLE~%
 \(string-empty-error :w-sym   'function-wanting-string
                     :w-type  'function
                     :w-locus 'some-value\)~%
 \(string-empty-error\)~%~@
:SEE-ALSO `mon:string-no-whitespace-p', `mon:string-all-whitespace-p',
`mon:string-contains-whitespace-p', `mon:string-trim-whitespace'.~%▶▶▶")

(fundoc 'symbol-not-null-or-string-not-empty-error-report
"Helper function for reporting `symbol-not-null-or-string-not-empty-error' conditions.~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'symbol-not-null-or-string-not-empty-error
        "Error for objects not of type `mon:symbol-not-null-or-string-not-empty'.~%~@
NON-SYMBOL-OR-STRING is the object originating the error.~%~@
Keyword W-OBJ-LOCUS is the locus of the error.  When supplied it is a symbol or
string of type `mon:symbol-not-null-or-string-not-empty', an error is signalled
if not.~%~@
:EXAMPLE~%
 \(symbol-not-null-or-string-not-empty-error \(make-array 1\)\)~%
 \(symbol-not-null-or-string-not-empty-error \(make-array 1\)
                                            :w-sym 'bubba 
                                            :w-type 'function 
                                            :w-obj-locus \"bubba\"\)~%
 \(symbol-not-null-or-string-not-empty-error \(make-array 1\)
                                            :w-obj-locus \"bubba\"\)~%~@
;; Following fail successfully:~%
 \(symbol-not-null-or-string-not-empty-error \(make-array 1\)
                                            :w-obj-locus \(cons 'a 'b\)\)~%
 \(symbol-not-null-or-string-not-empty-error \(make-array 1\) 
                                           :w-spec \(cons \"a~~S\" \"b~~A\"\)\)~%
:SEE-ALSO `string-empty-error'.~%▶▶▶")

(fundoc 'plist-error-report
"Helper function for reporting `mon:plist-error' conditions.~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'plist-error
"Error for objects not of type `mon:proper-plist'.~%~@
NOT-A-PLIST is the object originating the error.~%~@
Keyword W-OBJ-LOCUS is the locus of the error.  When supplied it is a symbol or
string of type `mon:symbol-not-null-or-string-not-empty', an error is signalled
if not.~%~@
:EXAMPLE~%
 \(plist-error \(make-array 1\)\)~%
 \(plist-error \(make-array 1\)
              :w-sym 'bubba 
              :w-type 'function 
              :w-obj-locus \"bubba\"\)~%
 \(plist-error '\(1 . b\)
              :w-obj-locus \(make-array 1\)\)~%~@
:SEE-ALSO `mon:plist-not-null-error', `mon:plist-proper-p', `mon:proper-plist'.~%▶▶▶")

(fundoc 'plist-not-null-error
"Error for objects not of type `mon:proper-plist-not-null'.~%~@
Like `mon:plist-error' but without the NOT-A-PLIST arg b/c we assume value of
the object originating the error is null.~%~@
Keyword W-OBJ-LOCUS is the locus of the error.  When supplied it is a symbol or
string of type `mon:symbol-not-null-or-string-not-empty', an error is signalled
if not.~%~@
:EXAMPLE~%
 \(plist-not-null-error\)~%
 \(plist-not-null-error :w-sym 'bubba 
                        :w-type 'function 
                        :w-obj-locus \"bubba\"\)~%
:SEE-ALSO `mon:plist-proper-p', `mon:proper-plist'.~%▶▶▶")

(fundoc 'open-stream-output-stream-error-report
"Helper function for reporting `mon:open-stream-output-stream-error' conditions.~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'open-stream-output-stream-error
"Signal an `open-stream-output-stream-error' condition.~%~@
The NOT-AN-OPEN-OUTPUT-STREAM object originating the error is not `streamp',
`open-stream-p', or `output-stream-p'.~%~@
When the keyword :expected-type is non-nil it is as per `type-error-expected-type'.
The default is 'stream.
:EXAMPLE~%
 \(open-stream-output-stream-error \"bubba\"\)~%
 \(open-stream-output-stream-error \"bubba\" :expected-type 'stream-or-boolean\) 
 \(open-stream-output-stream-error
  *standard-input* 
  :w-sym \"some-stream-frobbing-fncn\"
  :w-type 'function
  :w-obj-locus \"SOME-ARG\"
  :expected-type 'stream-or-boolean\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")


;;   Slot initarg/readers for simple-error-mon and subclasses are as follows:
;; - :W-SYM     error-sym
;; - :W-TYPE    error-sym-type  ;; key with an association in `mon:*error-table*'
;; - :W-SPEC    error-spec      ;; a format-control string or list of strings to be concatenated into one 
;; - :W-ARGS    error-args      ;; format-arguments in the form of a list
;; - :W-GOT     error-got       ;; 
;; - :W-TYPE-OF w-type-of       ;; a boolean
;; 


(fundoc 'file-error-wild-pathname-report
"Report CONDITION of type `mon:file-error-wild-pathname' to STREAM.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'file-error-wild-pathname
        "Signal a condition of type `mon:file-error-wild-pathname'.~%~@
Keyword W-SYM is a string or symbol identifying the signaller. ~%
Keyword W-TYPE is a symbol identifying signallers type it should be a member of `mon:*error-table*'.~%~@
Keyword PATHNAME is the pathname-or-namestring object which is the locus of the condition.~%
This is the initarg to the slot PATHNAME for the condition `cl:file-error'.
Its value may be accessed with the reader `cl:file-error-pathname'.~%~@
Keyword PATH-ARG-LOCUS is the argument \(if any\) to signaller originating the condition.
When provided, it is a string or symbol.~%~@
This is the initarg to the slot PATHNAME-WILD-ARG-LOCUS for the condition `mon:file-error-wild-pathname'.
Its value may be accessed with the reader `mon:pathname-wild-arg-error-locus'.~%~@
Keyword SIGNAL-ONLY is one of the following values:~%
  \{ signal signal-only condition condition-only t nil \}~%~@
When SIGNAL-ONLY is either of the symbols:~%
  signal signal-only~%~@
A condition object of type `mon:file-error-wild-pathname' is passed to
`cl:signal' and associated handlers can respond accordingly.~%~@
When SIGNAL-ONLY is any of the symbols:~%
 condition condition-only T~%~@
A condition object of type `mon:file-error-wild-pathname' is returned and no
further direct invocation of `cl:error' or `cl:signal' occurs.~%~@
:EXAMPLE~%
 \(funcall #'\(lambda \(tt-pathname\)
              \(when \(wild-pathname-p tt-pathname\)
                \(file-error-wild-pathname :w-sym        \"pathname-as-directory\" 
                                          :w-type        'function
                                          :pathname       tt-pathname
                                          :path-arg-locus \"tt-pathname\"
                                          :signal-or-only nil\)\)\)
          #P\"/some/wild/pathname/*.*\"\)~%
 \(let \(\(condition-obj
        \(funcall #'\(lambda \(tt-pathname\)
                     \(when \(wild-pathname-p tt-pathname\)
                       \(file-error-wild-pathname :w-sym        \"pathname-as-directory\" 
                                                 :w-type        'function
                                                 :pathname       tt-pathname
                                                 :path-arg-locus \"tt-pathname\"
                                                 :signal-or-only 'condition\)\)\)
                 \"/some/wild/namestring*.*\"\)\)\)
   \(list \(file-error-pathname           condition-obj\)
         \(pathname-wild-arg-error-locus condition-obj\)\)\)~%~@
:SEE-ALSO `file-error-wild-pathname-report'.~%▶▶▶")


;;; ==============================  


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
