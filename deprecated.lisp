;;; :FILE-CREATED <Timestamp: #{2011-03-23T16:26:15-04:00Z}#{11123} - by MON>
;;; :FILE mon-systems/deprecated.lisp
;;; ==============================


;;; ==============================
;; :NOTES
;; Features which are:
;; - no longer in use
;; - have been superseded
;; - have been proven faulty
;;; ==============================


(in-package #:mon)
;; *package*

;;; ==============================
;; Following deprecated in favor of doc-set:
;; #:fundoc-fmt
;; #:fundoc-set
;; #:vardoc-set
;; #:typedoc-set 
;;
;; :NOTE Use `doc-set' instead of the commented functions 
;;  fundoc-set vardoc-set typedoc-set
;;
;; :SOURCE SICL/Code/Docstrings/docstrings-en.lisp :WAS `fundoc'
;; (defun fundoc-set (name string)
;;   (declare (type symbol name) ((or null string) string))
;;   ;; :WAS
;;   ;; (setf (documentation name 'function) string)
;;   ;; (setf (documentation (fdefinition name) 'function) (documentation name 'function)) 
;;   (setf (documentation (fdefinition name) 'function) 
;;         (setf (documentation name 'function) string)))
;;
;; (defun vardoc-set (name string)
;;   (declare (type symbol name) 
;;            (special name)
;;            ((or null string) string))
;;   (setf (documentation name 'variable) string))
;;
;; (defun typedoc-set (name string)
;;   (declare (type symbol name) ((or null string) string))
;;   (setf (documentation name 'type) string))
;;
;; (defun fundoc-fmt (&rest args)
;;   (apply #'format nil (or args (list ""))))

;;; ==============================
;;; 
(deftype bit-convertable ()
  `(and proper-list-not-null
	(satisfies bit-convertable-p)))

;; (first (list 'seq "bubba" 'string 'null 'list 'cons 'vector))
(defun bit-convertable-p (seq)
  (declare (type proper-list seq))
  (and
   (case (length seq)
     ((= 4)  (and (typep (nth 3 seq) '(unsigned-byte 5)) t))
     ((0 1 2 3) t)
     (t nil))
   (loop 
      :for chk-ub8 :in seq 
      :unless (typep chk-ub8 'fixnum-0-or-over)
      :return nil
      :finally (return t))))

;; #b11111
;; (let ((chk-ub8 255))
;;   (or(typep chk-ub8 'unsigned-byte-8)
;;      (typep chk-ub8 '(unsigned-byte 5))))


;; (number-to-bits most-positive-fixnum)
;; (255 255 255 31), 4


;; #(192 79 212 48 200)
;; 5
;; (number-to-bits 825973027016)
;; (200 48 212 79 192)
;; 
;; (defun number-to-bits (number)
;; (etypecase number
;; (fixnum (number
;; (number-to-bits 825973027016)

;;; :NOTE Following is the deconstruction of the loop form below:
;;; (number-to-bits 511) ;=> (255 1), 2
;;; (expt 2 0) ;=> 1
;;; (* 255 (expt 2 0));=> 255
;;; (expt 2 8) ;=> 256
;;; (* 1 (expt 2 8)) ;=> 256
;;; (+ 255 256) ;=> 511
;;; :SOURCE cl-store/default-backend.lisp :WAS `bits->num'
;;; :ADDED type declarations, check for length 1
;;;
;;; :NOTE Specialized on bit-sequences where BITS represent and unsigned-byte-29.
(defun bits-to-number (bits) ;; (255 1) 
  (declare (type (or bit-convertable null) bits))
  (unless (null bits)
    (if (eql (length bits) 1) ;; (bits-to-number (number-to-bits 255))
        (car bits)
        (loop
           :with sum = 0
           :for pos :from 0 :by 8 ;;
           :for bit :in bits      ;; 255, 1
           ;; finally (return (the fixnum-0-or-over sum)) ;; 255, 256 => 511
           :do (incf (the fixnum-0-or-over sum) (* bit (expt 2 pos))) ;; 255, 256 
           :finally (return (the fixnum-0-or-over sum)))))) 

;;; :NOTE This expects a sequence of bits with the low-bit 
;;; :SOURCE cl-store/default-backend.lisp :WAS `num->bits'
;;; :ADDED type declaration and if/zerop/values
(defun number-to-bits (number)
  ;;  (declare (type fixnum-0-or-over number))
  (declare (unsigned-byte-29 number))
  (if (zerop number)
      (values (list 0) 1)
      (loop
	 :for val = (abs number) :then (ash val -8)
	 :for count :from 0 :until (zerop val)
	 :collect (logand val #XFF) :into bits ;; #XFF -> 255
	 :finally (return (values bits count)))))

;;; ==============================
;;;
;;; ==============================

(typedoc 'bit-convertable
"A list which can be converted by `mon:bits-to-number'.~%~@
Objects are of this type if they satisfy `mon:bit-convertable-p'.~%
An object will satisfy the test when it is `mon:proper-list-not-null' and of
length three or less with every elt is of type `mon:octet'. Or, if it is of
length four and its first three elts are of type `mon:octet' and its last elt is
of type \(unsigned-byte 5\).~%~@
:EXAMPLE~%~@
 \(typep '\(255 255 255\) 'bit-convertable\)~%
 \(typep '\(255 255 255 31\) 'bit-convertable\)~%
 \(typep '\(0\) 'bit-convertable\)~%
 \(typep '\(0 0 0 0\) 'bit-convertable\)~%
 \(typep '\(255 255 255 32\) 'bit-convertable\)~%~@
:SEE-ALSO `mon:number-to-bits', `mon:bits-to-number'.~%▶▶▶")

(fundoc 'bit-convertable-p
"Whether SEQ can be converted by `mon:bits-to-number'.~%~@
SEQ should be of type `mon:proper-list', signal an error if not.~%~@
Return non-nil when SEQ is of length three or less and every elt is of type
`mon:fixnum-0-or-over'. Or, if SEQ is of length four and its first three elts are
of type `mon:fixnum-0-or-over' and its last is of type \(unsigned-byte 5\).~%~@
:EXAMPLE~%
 \(bit-convertable-p '\(255 255 255\)\)~%
 \(bit-convertable-p '\(255 255 255 31\)\)~%
 \(bit-convertable-p '\(0\)\)~%
 \(bit-convertable-p '\(0 0 0 0\)\)~%
 \(not \(bit-convertable-p '\(255 255 255 32\)\)\)~%~@
;;  Following successfully signals an error:
 \(bit-convertable-p       \(\)\)~%~@
:SEE-ALSO `mon:number-to-bits', `mon:bits-to-number'.~%▶▶▶")

(fundoc 'number-to-bits
"Return as if by value a list of octets representing the bits of NUMBER.~%~@
Return value is in big-endian with most-significant-bit appearing as last element in list.~%~@
NUMBER is a fixnum of type `mon:unsigned-byte-29', signal an error if not.~%~@
:EXAMPLE~%
 \(number-to-bits 511\)
 ;=> \(255 1\), 2~%
 \(number-to-bits most-positive-fixnum\)~%
 \(bits-to-number \(number-to-bits most-positive-fixnum\)\)~%~@
;; Following errors successfully:
 \(number-to-bits -513\)~%~@
:SEE-ALSO `mon:bits-to-number', `mon:bit-convertable-p' `mon:coerce-int-float',
`mon:bytes-to-int', `mon:parse-integer-list', `mon:number-to-string'.~%▶▶▶")

(fundoc 'bits-to-number
"Return an integer of type `mon:unsigned-byte-29'.~%~@
BITS is a sequence of bytes of type `mon:bit-convertable' or null.
Signal an error if not.~%~@
BITS should be in big-endian format most-significant-bit appearing as last element in list.~%~@
:EXAMPLE~%
 \(number-to-bits 511\)
 ;=> \(255 1\), 2~%
 \(bits-to-number '\(255 1\)\)~%
 \(bits-to-number '\(0\)\)~%
 \(number-to-bits \(bits-to-number '\(255 255 255 31\)\)\)~%~@
:SEE-ALSO `mon:number-to-bits', `mon:bit-convertable-p', `mon:coerce-int-float',
`mon:bytes-to-int', `mon:parse-integer-list', `mon:number-to-string'.~%▶▶▶")


;;; ==============================
;;; EOF
