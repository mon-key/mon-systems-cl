;;; :FILE mon-systems/class-utils.lisp
;;; ==============================

;;; ==============================
;;; MOP related stuff is in the SB-MOP package.
;;; Relevant MOP related files:
;;; :FILE sbcl/src/pcl/generic-functions.lisp
;;; :FILE sbcl/src/pcl/defs.lisp
;;;
;;; :SEE-ALSO The closer-mop system provided by Pascal Costanza.
;;; (ql:quickload :closer-mop)
;;; `subclassp', `classp'
;;;
;;; ==============================
;;; The class PCL-CLASS is an implementation-specific common
;;; superclass of all specified subclasses of the class CLASS.
;;;
;;; The class STD-CLASS is an implementation-specific common
;;; superclass of the classes STANDARD-CLASS and
;;; FUNCALLABLE-STANDARD-CLASS.
;;; 
;;; (sb-mop:class-direct-subclasses (class-of 'find-class))
;;; (sb-mop:class-direct-superclasses (class-of 'find-class))
;;; 
;;; ==============================
;;; Usefull symbols, functions, etc.
;;;
;;; object-plist ;; ;; SB-PCL class plist-mixin (standard-object) 
;;;
;;; class-p
;;; standard-class-p
;;; class-precedence-list
;;; class-slots
;;; class-direct-slots
;;; class-slot-value
;;;
;;; slots-fetcher
;;; slots-to-inspect
;;;
;;; :FILE sbcl/src/pcl/env.lisp
;;; trace-method, untrace-method,  *traced-methods*, traced-method <CLASS> 
;;;
;;; ==============================
;; :SEE (info "(sbcl)Metaobject Protocol")
;;
;; (mon:where-is "class-finalized-p") => (SB-MOP:CLASS-FINALIZED-P)
;; 
;; `ensure-class', `ensure-class-using-class', `frob-ensure-class-args',
;; `legal-class-name-p', `classp', `metaclassp'
;;
;; (mon:where-is "ensure-class")  => (SB-MOP:ENSURE-CLASS)
;;
;;  ../sbcl/src/pcl/std-class.lisp
;;
;;; ==============================

;;; ==============================
;;;
;;; `cl:find-method' usage:
;;;  SIGNATURE (<generic-function-spec> qualifiers* (specializers*) &optional errorp)
;;;
;;;  Getting at <generic-function-spec> requires finding the fdefinition of the
;;;  generic function symbol. This is accomplished with either: 
;;;  (fdefinition '<SYMBOL>) or (fdefinition '(setf <SYMBOL>))
;;;  or alternatively (find-method #'<SYMBOL>) 
;;;
;;; :NOTE That when the generic-function is of type (setf <SYMBOL>)
;;; It is a "writer specializer"  and  has the form '(t <OBJECT>)
;;; Else, it is a "reader-specializer" of the form  '(slot-object)
;;;
;;; (find-method #'(setf <GF-SYMBOL>) {...} ) nil '(t dbc-system-subdir) nil)
;;; (find-method #'(setf dbc-var-binding) nil '(t dbc-system-subdir) nil)
;;; (find-method (function (setf dbc-var-binding) nil '(t dbc-system-subdir) nil))
;;; (find-method (function (setf dbc-var-binding)) nil '(t dbc-system-subdir) nil)
;;; (sb-mop:generic-function-methods (fdefinition 'dbc-system-described))
;;; (sb-mop:generic-function-methods (fdefinition '(setf dbc-var-binding)))
;;; (find-method (fdefinition 'dbc-var-binding) nil '(dbc-system-subdir))
;;; (find-method (fdefinition (setf dbc-var-binding)) nil '(dbc-system-subdir t) nil)
;;; (find-method (fdefinition '(setf dbc-var-binding)) nil '(t dbc-system-subdir) nil)
;;; (find-method #'(setf <SYMBOL>) nil '(<SPECIALIZER(S)>) nil) 
;;; (find-method (function <SYMBOL>) nil '(<CLASS>) nil)
;;; (find-method (fdefinition '(setf <SYMBOL>)) nil '(<SPECIALIZER(S)>) nil)
;;;
;;; :NOTE How to remove-method on generic-function:
;;;
;;; Find methods specialized on class
;;; (slime-who-specializes 'my-class)
;;;
;;; Find callers of generic:
;;;  (slime-who-calls 'my-generic)
;;;
;;; (remove-method (fdefinition 'my-generic) (find-method #'my-generic nil '(my-class) '(t)))
;;; (remove-method (fdefinition 'my-generic) (find-method #'my-generic nil '(t) '(t)))
;;; (remove-method (fdefinition 'my-generic) (find-method #'my-generic nil '(t) '(eql <THING>)))
;;;
;;; ==============================


(in-package #:mon)

;;; :COURTESY bknr-datastore-20100901-git/src/utils/utils.lisp
(defun class-subclasses (class)
  (labels ((collect-subclasses (class)
	     (let ((subclasses
		    #+allegro
		     (aclmop:class-direct-subclasses class)
		     #+cmu
		     (pcl:class-direct-subclasses class)
		     #+openmcl
		     (openmcl-mop:class-direct-subclasses class)
		     #+sbcl
		     (sb-mop:class-direct-subclasses class)))
	       (apply #'append subclasses
		      (mapcar #'collect-subclasses subclasses)))))
    (mapcar #'class-name (remove-duplicates (collect-subclasses 
					     (if (symbolp class)
						 (find-class class) class))))))

;; :SOURCE clocc/cllib/port/sys.lisp
(macrolet ((class-slots* (class)
	     `(sb-pcl::class-slots ,class))
	   (class-slots1 (obj)
             `(class-slots*
               (typecase ,obj
                 (class ,obj)
                 (symbol (find-class ,obj))
                 (t (class-of ,obj)))))
	   (slot-name (slot)
	     `(slot-value ,slot 'sb-pcl::name))
	   (slot-initargs (slot)
	     `(slot-value ,slot 'sb-pcl::initargs))
	   (slot-one-initarg (slot) 
	     `(car (slot-initargs ,slot)))
	   (slot-alloc (slot)
	     `(sb-pcl::slot-definition-allocation ,slot)))
;;	   
(defun class-slot-list (class &optional (all t))
  (mapcan (if all 
	      (%compose list slot-name)
              (lambda (slot)
                (when (eq (slot-alloc slot) :instance)
                  (list (slot-name slot)))))
          (class-slots1 class)))
;;
(defun class-slot-list-direct (class)
  (class-slots1 class))
;;
(defun class-slot-initargs (class &optional (all t))
  (mapcan (if all 
	      (%compose list slot-one-initarg)
              (lambda (slot)
                (when (eq (slot-alloc slot) :instance)
                  (list (slot-one-initarg slot)))))
          (class-slots1 class)))
;;
(defun structure-slots (struct)
  (class-slot-list (find-class struct)))

) ;; :CLOSE macrolet


;; (defun slot-val (obj slot &optional default)
;;   (or (when (slot-boundp obj slot)
;; 	(slot-value obj slot))
;;       default))

;;; :SOURCE mcclim/Apps/Scigraph/dwim/extensions.lisp
;;; Zetalisp function. :WAS `instancep'
(defgeneric class-instance-p (object)
  (:documentation "Is OBJECT an instance of the class standard-object."))
(defmethod class-instance-p ((object t)) nil) 
(defmethod class-instance-p ((object standard-object)) t)

(defun slot-value-or (obj slot &optional default)
  (or (when (slot-boundp obj slot)
	(slot-value obj slot))
      default))

(defun class-name-of (object)
  (if (class-instance-p object)
      (class-name (class-of object))
      (values nil (class-of object))))

(defun find-class-name-as-string (class &optional package)
  ;; (format nil "~S" (class-name (find-class 'parsed-ref)))
  (declare (optimize (speed 0) (safety 0) (compilation-speed 0) (debug 3)))
  (symbol-not-null-or-error class :w-locus "CLASS" :signal-or-only nil)
  (let* ((pkg (or (and package 
                       (or (find-package* package)
                           (package-error-not package
                                              :w-sym  'find-class-name-as-string
                                              :w-type 'function
                                              :w-spec "Arg PACKAGE provided but~
                                                      `mon:find-package*' doesn't find it"
                                              :signal-or-only nil)))
                  #+sbcl (sb-int:sane-package)
                  #-sbcl *package*))
         (class-sym-p (and 
                       (ref-bind gotit (multiple-value-list 
                                        (where-is-local (symbol-name class) pkg))
                         ;; (progn (break "refbind got: ~S" gotit)
                         (and (car gotit)
                              ;; (memq (cadr gotit) (list :internal :external))
                              gotit)    ;)
                         )))
         (fnd-cls (ref-bind fc (car class-sym-p)
                    ;; This is still searching for non-existent classes.
                    ;; (progn 
                    ;;   (break "refbind got: ~S" fc)
                    ;;   (find-class fc)) )))
                    (find-class fc) )))
    (and fnd-cls (values (string (class-name fnd-cls))
                         class (car class-sym-p)
                         (cadr class-sym-p)))))

;;; ==============================
;;; :COURTESY Stas Boukarev :SEE (URL `http://paste.lisp.org/+2KXX')
;;; :PASTE 120453 :TITLE shallow-copy-object :WAS `shallow-copy-object'
;;; :NOTE Following requires closer-mop
;;; 
;;; (defun shallow-copy-object (object)
;;;   (let* ((class (class-of object))
;;;          (new (allocate-instance class)))
;;;     (loop for slot in (c2mop:class-slots class)
;;;          when (c2mop:slot-boundp-using-class class object slot)
;;;        do (setf (c2mop:slot-value-using-class class new slot)
;;;                 (c2mop:slot-value-using-class class object slot)))
;;;     new))
;;; 
;;; Attempt to do it with sb-mop
;;; (where-is "class-slots")             ;=> (SB-MOP:CLASS-SLOTS :CLASS-SLOTS SB-KERNEL::CLASS-SLOTS) c2mop imports-from sb-mop
;;; (where-is "slot-boundp-using-class") ;=> (SB-MOP:SLOT-BOUNDP-USING-CLASS) c2mop imports-from sb-mop
;;; (where-is "slot-value-using-class")  ;=> (SB-MOP:SLOT-VALUE-USING-CLASS) c2mop imports-from sb-mop
#+sbcl
(defun copy-instance-of-class-shallowly (instance-of-class)
  (let* ((class (class-of instance-of-class))
         (new   (allocate-instance class)))
    (loop 
       for slot in (sb-mop:class-slots class)
       when (sb-mop:slot-boundp-using-class class instance-of-class slot)
       do (setf (sb-mop:slot-value-using-class class new slot)
                (sb-mop:slot-value-using-class class instance-of-class slot)))
    new))

;;; ==============================
;; #lisp 2011-01-27
;; <Kruppe> Is there any way to look at the slots of a class (short of using
;; 	 inspect or looking at the source)?  [12:00]
;; <stassats> sb-mop:class-slots
;;; ==============================

(defun slot-definition-and-name (class slot-name-or-def)
  (let (;; Don't signal directly when find-class doesn't 
        ;; so we may provide a handler later if desired.
        (fc (find-class class nil))) 
    (if fc
        (if (sb-mop:class-finalized-p fc)
            (values 
             (find slot-name-or-def
                   (sb-mop:class-slots fc)
                   :key #'sb-mop:slot-definition-name)
             slot-name-or-def)
            ;; Don't finalize if it isn't yet.
            (values nil nil (format nil ";; class ~S not `sb-mop:class-finalized-p'" fc)))
        (error 'simple-error
               :format-control "class ~S not found with `cl:find-class'" 
               :format-arguments `(,class)))))


;; :SOURCE roflcopter/rtracker.lisp :WAS `bound-slot-names'
(defun class-bound-slot-names (object)
  ;; :EXAMPLE (class-bound-slot-names (ql-dist:find-system "closer-mop"))
  (let ((class (class-of object)))
    (loop 
       :for slotd in (closer-mop:class-slots class) 
       :when (closer-mop:slot-boundp-using-class class object slotd)
       :collect (closer-mop:slot-definition-name slotd))))



;;; ==============================
;; (sb-mop:class-slots (find-class 'dbc:base-description))
;; (sb-mop:class-finalized-p (find-class 'dbc:base-description)) (where-is "base-entity")
;; (slot-definition-and-name 'dbc:entity-regexp 'dbc::match-entity-class)
;; (slot-definition-and-name 'bubba nil) 
;;; ==============================



;;; ==============================
;;; :CLASS-UTILS-DOCUMENTATION
;;; ==============================

(fundoc 'slot-definition-and-name
        "Return SLOT-DEFINITION-NAME object of CLASS.~%~@
Return value is as if by `cl:values': 
 SLOT-DEFINITION-NAME
 SLOT-NAME-OR-DEF
If `class-finalized-p' fails to return nth-value 0 and 1 are nil and a third
value is a string indicating that class was not yet finalized.~%~@
Signal an error if CLASS is not found with `cl:find-class'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'class-subclasses
"Return a list of the names of all subclasses of a given CLASS.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `class-slot-list', `class-slot-initargs',
`structure-slots'.~%►►►")

(fundoc 'class-name-of 
"Return the `cl:class-name' that OBJECT is `cl:class-of'.~%~@
When OBJECT is an instance of `cl:standard-object' and satisfies
`class-instance-p' return value is the name of the class it belongs to..~%~@
When OBJECT is an instance of some other class e.g. built-in-class
return value is as if by `cl:values' with the first value nil and the second the class that OBJECT is class-of.
:EXAMPLE~%~@
 (class-name-of (class-of 'find-class))~%~@
 (class-name-of (fdefinition 'fdefinition))
:NOTE The C-P-L of the class standard-class is:~%~@
  \(standard-class class standard-object t\)
The class standard-class is  default class of classes defined by `cl:defclass'.~%~@
The C-P-L of standard-object is:~%~@
 \(standard-object t\)
However, the class `standard-object' _is_ an instance of `standard-class'
It is also a superclass of every class that is an instance of `standard-class'
_except itself_.~%~@
:SEE-ALSO `mon:find-class-name-as-string'.~%►►►")

(fundoc 'find-class-name-as-string
"Find the class-name of CLASS in PACKAGE.~%~@
When found return-value is as if by `cl:values'.
Returned values have the form:~%
 \"<CLASS>\" 
  <CLASS>
  <PKG>[:|::]<CLASS>
  { :INTERNAL | :EXTERNAL | :INHERITED | :PRESENT }~%~@
 - First value is the `cl:class-name' of CLASS;
 - Second value is CLASS;
 - Third value is CLASS with package qualified name;
 - Fourth value inidicates the symbol status of CLASS in package;
:EXAMPLE~%~@
 \(find-class-name-as-string 'parsed-ref :dbc\)
 \(find-class-name-as-string 'parsed-ref \"DBC\"\)
 \(find-class-name-as-string 'parsed-ref\)~%
:SEE-ALSO `cl:find-class', `cl:class-name', `mon:class-name-of',
`mon:where-is-local'.~%►►►")

(fundoc 'slot-value-or
        "Like `cl:slot-value' but return DEFAULT if value of OBJ's SLOT is not `slot-boundp'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

#+sbcl
(fundoc 'copy-instance-of-class-shallowly
"Return value is a shallow copy of INSTANCE-OF-CLASS.~%~@
The copy is \"shallow\" int that the copy of INSTANCE-OF-CLASS does not
allocate a new object for each slot-value contained of INSTANCE-OF-CLASS
and therefor each slot-value of the instance returned will share structure
with the original INSTANCE-OF-CLASS.~%~@
IOW, if one of the slots of INSTANCE-OF-CLASS holds a list, then the slot-value
of returned instance points to the same list, i.e. it's not \"a deep copy\".~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

#+sbcl
(setf (documentation 'class-slot-initargs 'function)
      #.(format nil
 "Return the list of initargs of a CLASS.~%~@
CLASS can be a symbol, a class object (as returned by `class-of') or an instance
of a class.~%~@
If the second optional argument ALL is non-nil \(the default\), initargs for all
slots are returned, otherwise only the slots with :allocation type :instance are
returned.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `class-subclasses', `class-slot-list', `structure-slots'.~%►►►"))

#+sbcl
(setf (documentation 'class-slot-list 'function)
      #.(format nil
 "Return the list of slots of a CLASS.~%~@
CLASS can be a symbol, a class object \(as returned by `class-of'\) or an
instance of a class.~%~@
If the second optional argument ALL is non-NIL \(default\), all slots are
returned.~%~@
Otherwise, only the slots with :allocation type :instance are returned.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `class-subclasses', `class-slot-initargs', `structure-slots'.~%►►►"))

#+sbcl
(setf (documentation 'class-slot-list-direct 'function)
      #.(format nil
 "Return the list of slots of a CLASS with :allocation type :instance.~%~@
CLASS can be a symbol, a class object \(as returned by `class-of'\) or an
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `class-subclasses', `class-slot-initargs', `structure-slots'.~%►►►"))

#+sbcl
(setf (documentation 'structure-slots 'function)
      #.(format nil
"Return the list of slot names of structure STRUCT.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `class-subclasses', `class-slot-list', `class-slot-initargs'.~%►►►"))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
