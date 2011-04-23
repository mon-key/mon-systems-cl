;;; :FILE-CREATED <Timestamp: #{2011-03-24T21:38:51-04:00Z}#{11124} - by MON>
;;; :FILE mon-systems/class-doc.lisp
;;; ==============================


;;; ==============================
;;; :NOTE At compile/load time evaluation of classdoc forms must occur as
;;;  :documentation #.(classdoc <NAMED-CLASS> <:SLOT-NAME>)
;;; This in turn implies that the hashtable should already have a key/value pair
;;; for the documentation present...
;;;
;;; E.g. at loadtime
;;; (make-documented-class 
;;;  'tt--my-class
;;;  :my-slot-a "docstring for MY-SLOT-A"
;;;  :my-slot-b "docstring for MY-SLOT-B"
;;;  :class-doc "docstring for MY-CLASS"
;;;  :doc-hash-table *default-class-documentation-table*)
;;;
;;;     (defclass tt--my-class ()
;;;       ((my-slot-a 
;;;         :documentation #.(classdoc 'tt--my-class :my-slot-a))
;;;        (my-slot-b 
;;;         :documentation #.(classdoc 'tt--my-class :my-slot-b)))
;;;       (:documentation  #.(classdoc 'tt--my-class :class-doc)))
;;;
;;; ---
;;; Example usage with tests:
;;; ---
;;; (make-documented-class 
;;;  'my-class
;;;  :my-slot-a "docstring for MY-SLOT-A"
;;;  :my-slot-b "docstring for MY-SLOT-B"
;;;  :class-doc "docstring for MY-CLASS"
;;;  :doc-hash-table *default-class-documentation-table*)
;;;    
;;; (let ((gthr '()))
;;;   (dolist (cd '(:my-slot-a
;;;                 :my-slot-b
;;;                 :class-doc)
;;;            (progn (setf gthr (nreverse gthr)) gthr))
;;;     (push (classdoc 'my-class cd) gthr)))
;;; ---
;;; (progn 
;;;   (setf (gethash 'my-class *default-class-documentation-table*) nil)
;;;   (make-documented-class 
;;;    'my-class
;;;    :my-slot-a "docstring for MY-SLOT-A"
;;;    :my-slot-b "docstring for MY-SLOT-B"
;;;    :class-doc "docstring for MY-CLASS"
;;;    :doc-hash-table *default-class-documentation-table*)
;;;   (equal
;;;    (list 
;;;     (equal (nth 2 (documented-class-doc 'my-class)) '(:CLASS-DOC . "docstring for MY-CLASS"))
;;;     (equal (documented-class-slot-doc 'my-class :my-slot-a) "docstring for MY-SLOT-A")
;;;     (equal (documented-class-slot-doc 'not-a-slot :my-slot-a :slot-doc-default "bubba") "bubba")
;;;     (equal (documented-class-slot-doc 'not-a-slot :class-doc  :class-doc-default "bubba") "bubba")
;;;     (equal (documented-class-slot-doc 'my-class :class-doc) "docstring for MY-CLASS")
;;;     (eq (documented-class-slot-doc 'my-class :class-doc) nil) 
;;;     (eq (gethash 'my-class *default-class-documentation-table*) nil))
;;;    '(T T T T T T T)))
;;; ---
;;; Following error successfully:
;;; (documented-class-doc :doc-hash-table "bubba")
;;; (documented-class-slot-doc 'my-class :my-slot-a  :doc-hash-table "bubba")
;;;
;;; ==============================

;;; ==============================
;;; Symbols required for implementation:
;;
;;; list-proper-p, plist-proper-not-null-p, hash-or-symbol-p, plist-to-alist, memq,
;;; symbol-not-null-or-error, proper-list-error, proper-list-error, simple-error-mon,
;;; plist-not-null-error,
;;;
;;; ==============================



(in-package #:mon)
;; *package*

(defgeneric documented-class-doc (named-class &key)
  (:documentation 
   #.(format nil
   "Return a class documentation structure for NAMED-CLASS.~%~@
:SEE-ALSO `mon:classdoc', `mon:documented-class-slot-doc',
`mon:documented-class-with-docs', `mon:make-documented-class',
`mon:documented-class-verify-init',
`mon:*default-class-documentation-table*'.~%►►►")))

(defgeneric documented-class-slot-doc (named-class documented-slot &key)
(:documentation 
   #.(format nil
"Return docstring for DOCUMENTATED-SLOT from documentation structure of NAMED-CLASS.~%~@
:SEE-ALSO `mon:classdoc', `mon:documented-class-slot-doc',
`mon:documented-class-with-docs', `mon:make-documented-class',
`mon:documented-class-verify-init',
`mon:*default-class-documentation-table*'.~%►►►")))

(defclass documented-class-with-docs ()
  ((documented-class
    :initarg :documented-class
    :initform (error ":CLASS `documented-class-with-docs' ~
                      -- slot DOCUMENTED-CLASS must not be null"))))
;; initialize-instance
(defmethod initialize-instance :after ((named-class documented-class-with-docs) &key doc-hash-table)
  (let ((verify (when (and (slot-boundp named-class 'documented-class))
                  (slot-value named-class 'documented-class))))
    (and verify
         (setf (gethash (car verify) doc-hash-table) (plist-to-alist (cadr verify))))))

(defmethod documented-class-doc ((named-class t) &key doc-hash-table)
  (gethash named-class (%verify-hash-table-for-documented-class doc-hash-table)))
      
(defmethod documented-class-slot-doc ((named-class t) documented-slot 
                                      &key doc-hash-table slot-doc-default)
  (declare ((or null string) slot-doc-default))
  (multiple-value-bind  (if-docs present) (documented-class-doc named-class :doc-hash-table doc-hash-table)
    (if present 
        (or (cdr (assoc documented-slot if-docs)) slot-doc-default)
        slot-doc-default)))

(defmethod documented-class-slot-doc ((named-class t) (documented-slot (eql :class-doc))
                                      &key doc-hash-table class-doc-default)
  (declare ((or null string) class-doc-default))
  (let ((chk-hash (%verify-hash-table-for-documented-class doc-hash-table)))
    (multiple-value-bind (if-docs present) (documented-class-doc named-class :doc-hash-table chk-hash)
      (if present 
          (prog1 
              (or (cdr (assoc :class-doc if-docs)) class-doc-default)
            ;; Access to class-doc should occur last in a class' definition.
            ;; So, we can now remove the key from the hash-table.
            (remhash named-class chk-hash))
          class-doc-default))))

(defun documented-class-verify-init (initform)
  (and (or (consp initform)
           (error 'type-error :datum initform :expected-type 'list))
       (or (list-proper-p initform)
           (proper-list-error :w-sym 'documented-class-verify-init
                              :w-type 'function 
                              :error-args `(initform ,initform)
                              :signal-or-only nil))
       (or (= (length initform) 2) 
           (simple-error-mon :w-sym 'documented-class-verify-init
                             :w-type 'function
                             :w-spec "arg INITFORM not of length 2"
                             :w-got initform
                             :w-type-of t
                             :signal-or-only nil))       
       (symbol-not-null-or-error (car initform) :w-locus 'initform :signal-or-only nil)
       (or (plist-proper-not-null-p (cadr initform))
           (plist-not-null-error
            :w-sym 'documented-class-verify-init
            :w-type 'method
            :w-spec "arg INITFORM not suitable for plist construction~%~%~
                     INITFORM must contain at least one key/value pair of the form:~% ~
                      <:SLOT-NAME>/<DOCSTRING> or :CLASS-DOC/<DOCSTRING>~%~%"
            :w-obj-locus 'initform 
            :signal-or-only nil))
       initform))
       
(defun %verify-hash-table-for-documented-class (&optional doc-hash-table)
  (or (and doc-hash-table (hash-or-symbol-p doc-hash-table))
      (or (and (hash-table-p *default-class-documentation-table*)
               *default-class-documentation-table*)
          (setq *default-class-documentation-table* (make-hash-table)))))

(defun make-documented-class (named-class 
                              &rest key-val-pairs
                              &key doc-hash-table
                              &allow-other-keys)
  (declare ((or null hash-table) doc-hash-table))
  (let ((chk-name-pairs (progn 
                          (remf key-val-pairs :doc-hash-table)
                          (documented-class-verify-init (list named-class key-val-pairs))))
        (chk-hash (%verify-hash-table-for-documented-class doc-hash-table)))
    (make-instance 'documented-class-with-docs 
                   :documented-class chk-name-pairs 
                   :doc-hash-table  chk-hash)))

(defun classdoc (class slot-or-class &key doc-hash-table slot-doc-default)
  (declare (type symbol class slot-or-class)
           ((or null symbol hash-table) doc-hash-table)
           ((or null string) slot-doc-default))
  (documented-class-slot-doc class 
                             (or (and (memq slot-or-class '(:class :class-doc)) :class-doc)
                                 slot-or-class)
                             :doc-hash-table doc-hash-table
                             :slot-doc-default slot-doc-default))


;;; ==============================
;;; :CLASS-DOC-DOCUMENTATION
;;; ==============================

(fundoc 'make-documented-class
"Add the documentation data structure to default documentation table.~%~@
NAMED-CLASS is a symbol naming the class documented.~%~@
Rest arg KEY-VAL-PAIRS are any number of key/val pairs where key is
colon-prefixed keyword corresponding to class direct-slot defined by NAMED-CLASS
and val is a string documenting the direct-slot e.g.:~%
 :<SLOT-NAME-A> \"<DOCSTRING>\"~%
 :<SLOT-NAME-B> \"<DOCSTRING>\"~%~@
As a special case, when the key of a key/val pair is the colon-prefixed keyword
:class-doc the val will be assigned as the docstring for class NAMED-CLASS, e.g.:~%
 :class-doc \"<CLASS-DOCSTRING-FOR-NAMED-CLASS>\"~%~@
:NOTE Because KEY-VAL-PAIRS is passed as an &rest arg they need not \(indeed
should not\) be supplied as the quoted list.~%~@
Keyword DOC-HASH-TABLE names a symbol or hash-table to store NAMED-CLASS's
documentation data structure to. 
Default value is the hash-table at the value cell of special variable
`mon:*default-class-documentation-table*'.
If supplied, DOC-HASH-TABLE should satisfy `mon:hash-or-symbol-p', a condition
of type `cl:type-error' is signaled if not.~%~@
:EXAMPLE~%
 \(make-documented-class 
  'my-class
  :my-slot-a \"docstring for MY-SLOT-A\"
  :my-slot-b \"docstring for MY-SLOT-B\"
  :class-doc \"docstring for MY-CLASS\"
  :doc-hash-table *default-class-documentation-table*\)~%
 \(class-doc 'my-class :my-slot-a\)~%
 \(class-doc 'my-class :class-doc\)~%
 (remhash 'my-class 'mon:*default-class-documentation-table*)~%~@
:SEE-ALSO `mon:classdoc', `mon:documented-class-slot-doc',
`mon:documented-class-with-docs', `mon:make-documented-class',
`mon:documented-class-verify-init',
`mon:*default-class-documentation-table*'.~%►►►")

(fundoc 'classdoc 
"Retrieve documentation for CLASS.~%~@
CLASS is a symbol naming a class.~%~@
SLOT-OR-CLASS is a symbol names the slot to retrieve a documentation string for.
When SLOT-OR-CLASS is either the keyword :class or :class-doc retrieve the
documentation for the class.~%~@
Keyword DOC-HASH-TABLE is symbol or hash-table the values of which contain an
alist of documentation data for CLASS.~%~@
Keyword SLOT-DOC-DEFAULT is a string to return if a documentation string is not found.~%~@
:EXAMPLE~%
 \(make-documented-class 
  'my-class
  :my-slot-a \"docstring for MY-SLOT-A\"
  :my-slot-b \"docstring for MY-SLOT-B\"
  :class-doc \"docstring for MY-CLASS\"
  :doc-hash-table *default-class-documentation-table*\)~%
 \(let \(\(gthr '\(\)\)\)
   \(dolist \(cd '\(:my-slot-a
                 :my-slot-b
                 :class-doc\)
            \(progn \(setf gthr \(nreverse gthr\)\) gthr\)\)
     \(push \(classdoc 'my-class cd\) gthr\)\)\)~%
:SEE-ALSO `mon:documented-class-slot-doc', `mon:documented-class-with-docs',
`mon:make-documented-class', `mon:documented-class-verify-init',
`mon:*default-class-documentation-table*', `mon:fundoc', `mon:vardoc',
`mon:typedoc'.~%►►►")

(fundoc 'documented-class-verify-init
"Verify that INITFORM is structured suitably for intializing instances of class
`mon:documented-class-with-docs'.~%~@
INITFORM is a two element list of the form:~%
 \(<NAMED-CLASS> \( { <:SLOT-NAME> <DOCSTRING> }* { :CLASS-DOC <DOCSTRING> } \)\)~%~@
The <NAMED-CLASS> at the car of INITFORM is a symbol naming the class documented. 
<NAMED-CLASS> need not reference a defined or finalized class but should be of
type `mon:symbol-not-null', an error is signaled if not.~%~@
The cadr of <INITFORM> should be a plist of type `mon:proper-list-not-null' an
error is signaled if not.~%~@
The plist at cadr of <INITFORM> should be contained of at least one key/value
pairs with having one of the forms:~%
 <:SLOT-NAME>/<DOCSTRING>~%
 :CLASS-DOC/<DOCSTRING>~%~@
When provided, any number of <:SLOT-NAME>/<DOCSTRING> key/value pairs may be
present where <:SLOT-NAME> is a keyword naming a class slot with the same
identity and <DOCSTRING> is a string documenting <:SLOT-NAME>.~%~@
When at least one <:SLOT-NAME>/<DOCSTRING> key/value pair is present inclusion
of a :CLASS-DOC/<DOCSTRING> key/value pair is optional. When provided, the key
:CLASS-DOC must occur as :CLASS-DOC, <DOCSTRING> is a string documenting
<NAMED-CLASS>.~%~@
:EXAMPLE~%
 \(documented-class-verify-init 
  '\(my-class
    \(:my-slot-a \"docstring for MY-SLOT-A\"
     :my-slot-b \"docstring for MY-SLOT-B\"
     :class-doc \"docstring for MY-CLASS\"\)\)\)~%~@
;; Following successfully signal errors:~%
 \(documented-class-verify-init 
  '\(nil '\(:my-slot-a \"docstring for MY-SLOT-A\"\)\)\)~%
 \(documented-class-verify-init '\(my-class\)\)~%
 \(documented-class-verify-init '\(my-class . :my-slot-a\)\)~%
 \(documented-class-verify-init '\(my-class \(\)\)\)~%
:SEE-ALSO `mon:classdoc', `mon:documented-class-slot-doc',
`mon:documented-class-with-docs', `mon:make-documented-class',
`mon:documented-class-verify-init',
`mon:*default-class-documentation-table*'.~%►►►")

(fundoc '%verify-hash-table-for-documented-class
"If DOC-HASH-TABLE is ommitted return hash-table of special variable
`*default-class-documentation-table*'.~%~@
If DOC-HASH-TABLE is non-nil check if it is `hash-or-symbol-p' if so return a
hash-table, else signal a `cl:type-error'.~%~@
Helper function for `mon:make-documented-class' when making instances of class
`documented-class-with-docs'.~%~@
:SEE-ALSO `mon:classdoc', `mon:documented-class-slot-doc',
`mon:documented-class-with-docs', `mon:make-documented-class',
`mon:documented-class-verify-init',
`mon:*default-class-documentation-table*'.~%►►►")


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:


;;; ==============================
;;; EOF
