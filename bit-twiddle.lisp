;;; :FILE-CREATED <Timestamp: #{2011-03-22T11:46:04-04:00Z}#{11122} - by MON>
;;; :FILE mon-systems/bit-twiddle.lisp
;;; ==============================

;;; ==============================
;; :NOTE
;; #xfe #x01 <- (254 001)
;; #x01 #xfe <- (254 001)
;;
;; Say you read two bytes, #xfe and #x01 from a file.  If you
;; interpret the #xfe as the least-significant byte, then your 16-bit
;; number would be #x01fe.  That's little-endian.  If the #xfe is the
;; most-significant byte, the 16-bit number is #xfe01.  That's big-endian.
;;
;; Little-endian CPUs usually employ "LSB 0" bit numbering, however both bit
;; numbering conventions can be seen in big-endian machines. Some architectures
;; like SPARC and Motorola 68000 use "LSB 0" bit numbering, while S/390, PowerPC
;; and PA-RISC use "MSB 0".
;; 
;; The recommended style for Request for Comments documents is "MSB 0" bit numbering.
;; LSB-0 
;; 7 6 5 4 3 2 1 0
;; 0 0 0 0 0 0 0 1
;; 
;; MSB-0
;; 7 6 5 4 3 2 1 0
;; 0 0 0 0 0 0 0 1 => #b10000000 => 128
;;
;; #b11111111111111111111111111111111 0-4294967295 32 bits  4 octets
;;           4       3      2       1
;; #b1111111111111111111111111111111111111111111111111111111111111111
;;           8       7      6       5        4       3      2       1
;;
;;  Following strikes me as a more lucid explanation than is given in the spec:
;; ,----
;; | The first thing we need when manipulating a field of bits (called a byte in
;; | Common Lisp) is a way of specifying its bounds. The BYTE function constructs a
;; | byte specifier from a size (number of bits) and a position (the number of the
;; | rightmost bit of the byte within the containing integer, where the LSB is bit 0).
;; `---- ;; :SOURCE (URL `http://www.psg.com/~dlamkins/sl/chapter18.html')
;;
;;; ==============================


(in-package #:mon)
;; *package*


;; 
(defun number-to-bit-list (unsigned-integer) 
  (declare (type (integer 0 *) unsigned-integer)
           (optimize (speed 3)))
  (let ((int-len (integer-length unsigned-integer)))
    (loop 
       for i below int-len
       collect (or (and (logbitp i unsigned-integer) 1) 0) into rslt
       finally (return (nreverse rslt)))))
;;
;; Variant of above with tail-call.
;; (defun number-to-bit-list (unsigned-integer) 
;;   (declare (type (integer 0 *) unsigned-integer)
;;            (optimize (speed 3)))
;;   (if (or (zerop unsigned-integer)
;;           (eq unsigned-integer 1))
;;       (list unsigned-integer)
;;       (let ((mod-ui-list (list (mod unsigned-integer 2))))
;;         (nconc (number-to-bit-list (ash unsigned-integer -1))
;;                mod-ui-list))))


(defun number-to-bit-vector (unsigned-integer)
  (declare ((integer 0 *) unsigned-integer)
           (optimize (speed 3)))
  (flet ((number-to-bit-vector-fixnum (fixnum-int)
           (declare (fixnum-0-or-over fixnum-int))
           (let* ((mk-len  (the fixnum-bit-width (integer-length fixnum-int)))
                  (bv-29   (make-array (the fixnum-bit-width mk-len)
                                       :element-type    'bit
                                       :initial-element 0
                                       :adjustable      nil)))
             (declare (fixnum-bit-width mk-len)
                      (simple-bit-vector bv-29))
             (loop 
                for i-lb from 0 below mk-len
                do (and (logbitp i-lb fixnum-int)
                        (setf (sbit bv-29 i-lb) 1))
                finally (return (nreverse bv-29)))))
         (number-to-bit-vector-bignum (bignum-int)
           (declare (bignum-0-or-over bignum-int))
           (let* ((mk-big-len  (the bignum-bit-width (integer-length bignum-int)))
                  (bv-big      (make-array (the bignum-bit-width mk-big-len)
                                           :element-type    'bit
                                           :initial-element 0
                                           :adjustable      nil)))
             (declare (bignum-bit-width mk-big-len)
                      (simple-bit-vector bv-big))
             (loop 
                for i-lb from 0 below mk-big-len
                do (and (logbitp i-lb bignum-int)
                        (setf (sbit bv-big i-lb) 1))
                finally (return (nreverse bv-big))))))
    (etypecase unsigned-integer
      (fixnum-0-or-over (the simple-bit-vector 
                          (number-to-bit-vector-fixnum
                           (the fixnum-0-or-over unsigned-integer))))
      (bignum-0-or-over  (the simple-bit-vector
                           (number-to-bit-vector-bignum
                            (the bignum-0-or-over unsigned-integer)))))))

;; :SOURCE (URL `http://www.lispforum.com/viewtopic.php?f=2&t=1205#p6269')
;; with modifications
(defun %bit-vector-to-integer.mon (bit-vector)
  "Return BIT-VECTOR's representation as a positive integer.
MON version using `cl:flet' and `cl:reduce'."
   ;; (= (bit-vector-to-integer #*01011100000100010011100100110101001111001100000001011110100000001010010001011110001001100011000101010010111000101101101101011010)
   ;; 122378404974049034400182615604361091930)
  (declare (bit-vector bit-vector)
           (optimize (speed 3)))
  ;; :NOTE We ought to be able to optimize around the size of expected return
  ;; value by taking the length of the bv which should not exceed the
  ;; integer-length of final return value.
  (flet ((bit-adder (first-bit second-bit)
           (+ (ash first-bit 1) second-bit)))
    (etypecase bit-vector
      (simple-bit-vector 
       (locally (declare (simple-bit-vector bit-vector))
         (reduce #'bit-adder bit-vector)))    
      (bit-vector
       (reduce #'bit-adder bit-vector)))))

;; :PASTE-DATE 2011-08-10
;; :PASTE-TITLE "Annotation number 2: a faster version"
;; :PASTED-BY stassats
;; :PASTE-URL (URL `http://paste.lisp.org/+2NN1/2')
(defun %bit-vector-to-integer.stassats (bit-vector)
  "Return BIT-VECTOR's representation as a positive integer.
Stas version using `cl:flet' and `cl:loop'."
  (let* ((word-size 64)
         (length (length bit-vector))
         (result 0)
         (index -1))
    (flet ((build-word ()
             (loop 
                repeat word-size
                for j = 0 then (logior (bit bit-vector (incf index))
                                       (ash j 1))
                finally (return j))))
      (loop 
         repeat (floor length word-size)
         do (setf result (logior (build-word)
                                 (ash result (1- word-size)))))
      (loop while (< index (1- length))
         do (setf result (logior (bit bit-vector (incf index))
                                 (ash result 1)))))
    result))
;;
;; :PASTE-DATE 2011-08-10
;; :PASTE-TITLE "Annotation number 1: another version"
;; :PASTED-BY Xach
;; :PASTE-URL (URL `http://paste.lisp.org/+2NN1/1')
(defun bit-vector-to-integer (bit-vector)
  "Return BIT-VECTOR's representation as a positive integer.
Xach version using `cl:dotimes'."
  (declare (bit-vector bit-vector)
           (optimize (speed 3)))
  (let ((j 0))
    (dotimes (i (length bit-vector) j)
      (setf j (logior (bit bit-vector i) (ash j 1))))))

(defun boolean-to-bit (boolean &optional no-error)
  (declare (optimize (speed 3)))
  (multiple-value-bind (boolp boolval) (booleanp boolean)    
    (if no-error
        (values
         (or (and boolp (or (and boolp 1) 0))
             (and (typep boolval '(integer 0 1)) boolval))
         boolval)
        (values-list
         (ecase boolval
           (0      '(0 0))
           (1      '(1 1))
           ((nil)  '(0 (quote nil)))
           ((t)    '(1 t)))))))

(defalias 'bit-from-boolean 'boolean-to-bit)


;;; ==============================
;; SBCL's definition of `cl:logbitp' checks the follwing props of  INDEX 
;; sb-vm:n-word-bits => 32 sb-vm:n-lowtag-bits => 3 (- 32 3) => 29
;; If INDEX is greater than 29 it checks it sign else it does as below:
(declaim (inline octet-logbitp-1-or-0))
(defun octet-logbitp-1-or-0 (index unsigned-byte-8)
  (declare ((mod 8) index)
           (type unsigned-byte-8 unsigned-byte-8)
           (optimize (speed 3) ))
  ;; (or (and (logbitp index unsigned-byte-8) 1) 0) ==
  ;; (or (and (zerop (logand byte-int (ash 1 i))) 0) 1) ==
  ;; (if (= (logand byte-int (ash 1  i)) 0) 0 1) == 
  (or (and (= (logand unsigned-byte-8 (ash 1 index)) 0) 0) 1))

(declaim (inline octet-set-bit-vector-index))
(defun octet-set-bit-vector-index (bit-vector index 1-or-0)
  (declare ((simple-bit-vector 8) bit-vector)
           ((mod 8) index)
           ((mod 2) 1-or-0)
           (optimize (speed 3)))
  (setf (sbit bit-vector index) 1-or-0))

;;; ==============================
;; :NOTE the logxor on index lets us set the bits of bit-vector in reverse of
;; the stepping iterator.
;; (loop for i from 0 below 8 collect (list i (logxor i 7)))
;;  => ((0 7) (1 6) (2 5) (3 4) (4 3) (5 2) (6 1) (7 0))
;; 7 (3 bits, #x7, #o7, #b111) 0
;; 6 (3 bits, #x6, #o6, #b110) 1
;; 5 (3 bits, #x5, #o5, #b101) 2
;; 4 (3 bits, #x4, #o4, #b100) 3
;; 3 (2 bits, #x3, #o3, #b011) 4
;; 2 (2 bits, #x2, #o2, #b010) 5
;; 1 (1 bit,  #x1, #o1, #b001) 6
;; 0 (0 bits, #x0, #o0, #b000) 7
(declaim (inline octet-set-bit-vector-index-xor))
(defun octet-set-bit-vector-index-xor (bit-vector index 1-or-0)
  (declare ((simple-bit-vector 8) bit-vector)
           ((mod 8) index)
           ((mod 2) 1-or-0)
           (optimize (speed 3)))
  (setf (sbit bit-vector (logxor index 7)) 1-or-0))

;;(if (octet-set-bit-vector-index-xor bv i (octet-logbitp-1-or-0 i byte-int)
(declaim (inline octet-set-bit-vector-index-xor-if))
(defun octet-set-bit-vector-index-xor-if (bit-vector index octet)
  (declare ((simple-bit-vector 8) bit-vector)
           ((mod 8) index)
           (unsigned-byte-8 octet)
           (optimize (speed 3)))
  (unless (= (logand octet (ash 1 index)) 0)
    (setf (sbit bit-vector (logxor index 7)) 1)))

(declaim (inline octet-bit-vector-zeroed))
(defun octet-bit-vector-zeroed ()
  (declare (optimize (speed 3) (safety 1)))
  (make-array 8 :element-type 'bit :initial-element 0))

(declaim (inline octet-bit-vector-initialize))
(defun octet-bit-vector-initialize (initial-contents)
  (declare (list initial-contents)
           (optimize (speed 3) (safety 1)))
  (make-array 8 :element-type 'bit :initial-contents initial-contents))

(declaim (inline octet-to-bit-vector))
(defun octet-to-bit-vector (octet)
  (declare (type unsigned-byte-8 octet)
           (inline octet-bit-vector-zeroed)
           (optimize (speed 3)))
  (let ((bv8 (octet-bit-vector-zeroed)))
    (declare (bit-vector-octet bv8))
    (dotimes (i 8 bv8)
      (setf (sbit bv8 i) (ldb (byte 1 7) octet))
      (setf octet (logand #xFF (ash octet 1))))))

;; Consider caching all bits 0 to 255
;; (make-array-of-octet-bit-vectors)
(defun make-array-of-octet-bit-vectors ()
  (declare (inline octet-to-bit-vector)
           (optimize (speed 3) (safety 1)))
  (loop
     with ba-array = (make-array 256)
     for x upfrom 0 below 256
     do (setf (aref ba-array x) 
              (the bit-vector-octet (octet-to-bit-vector x)))
     finally (return ba-array)))

;; :SOURCE usenet-legend/io.lisp
(defun bit-vector-octets (bv)
  (declare (type simple-bit-vector bv)
           (optimize speed))
  (let ((octets (make-array (ceiling (length bv) 8)
                            :element-type 'octet
                            :initial-element 0)))
    (loop for bit across bv
       for i from 0 below (length bv)
       do (multiple-value-bind (j k) (floor i 8)
            (setf (aref octets j)
                  (logior (ash bit k) (aref octets j)))))
    (values octets
            (length bv))))


;;; ==============================
;;; :NOTE An alternative version using `cl:read-from-string':
;;;  (funcall #'(lambda (x) (read-from-string (format nil "#*~2R" x))) 254)
;;;
;;; ==============================
;;; :SOURCE (URL `http://paste.lisp.org/+2LKZ/1')
;;; :PASTED-BY Zach Beane :DATE 2011-04-08
;;; An alterative using cl:do
;;; (defun octet-bit-vector (octet)
;;;   (do ((vector (octet-bit-vector-zeroed))
;;;        (i 0 (1+ i))
;;;        (j 7 (1- j)))
;;;       ((= i 8) vector)
;;;     (setf (sbit vector j) (ldb (byte 1 i) octet))))
;;;
;;; :SOURCE (URL `http://paste.lisp.org/+2LKZ/2')
;;; :PASTED-BY Zach Beane :DATE 2011-04-08
;;; With shift & constant ldb byte spec
;;; Likely this is fastest...
;; (defun octet-bit-vector (octet)
;;   (declare (type unsigned-byte-8 octet)
;;            (inline octet-bit-vector-zeroed)
;;            (optimize (speed 3)))
;;   (let ((vector (octet-bit-vector-zeroed)))
;;     (dotimes (i 8 vector)
;;       (setf (sbit vector i) (ldb (byte 1 7) octet))
;;       (setf octet (logand #xFF (ash octet 1))))))
;;
;; Which traces to something like below:
;; (let ((byte-int 255)
;;       (gthr '()))
;;   (dotimes (i 8 (setf gthr (nreverse gthr)))
;;     (push (list 1 byte-int (ldb (byte 1 7) byte-int)
;;                 (setf byte-int (logand 255 (ash byte-int 1))))  gthr)))
;;
;;                             255 => #b11111111
;; (logand 255 (ash 255 1)) => 254 => #b11111110
;; (logand 255 (ash 254 1)) => 252 => #b11111100
;; (logand 255 (ash 252 1)) => 248 => #b11111000
;; (logand 255 (ash 248 1)) => 240 => #b11110000
;; (logand 255 (ash 240 1)) => 224 => #b11100000
;; (logand 255 (ash 224 1)) => 192 => #b11000000
;; (logand 255 (ash 192 1)) => 128 => #b10000000
;; (logand 255 (ash 128 1)) => 0   => #b00000000
;;; ==============================
;;
;; Looks like Xach's paste above is the fastest.
;; Following is the best alternative I can find and it still takes roughly twice
;; as many cycles to complete with both consing about the same.
;; I was surprised to find that the extra LOGXOR and IF conditional on zerop
;; to be so costly...

;;; ==============================
;; :NOTE The original version letf room for modifications on non
;; `unsigned-byte-8' integers by instead take the (integer-length <BYTE-INT>)
;; such that we could create alternative versions capable of generating any
;; length bv we need for any integer. These notes and the various
;; implementations are kept here with that in mind.
;;
;; (defun octet-to-bit-vector-0 (byte-int)
;;   (declare (type unsigned-byte-8 byte-int)
;;            (inline octet-bit-vector-zeroed
;;                    octet-logbitp-1-or-0
;;                    octet-set-bit-vector-index)
;;            (optimize (speed 3)))
;;   (loop 
;;      with ba = (octet-bit-vector-zeroed)
;;      for fld upfrom 0 below 8
;;      for bit downfrom 7 to 0 ;; MSB is at 0, LSB is at 7 --> little-endian
;;      do (octet-set-bit-vector-index ba bit (octet-logbitp-1-or-0 fld  byte-int))
;;      finally (return (the bit-vector-octet ba))))
;;
;; (defun octet-to-bit-vector-2 (byte-int)
;;   (declare (type unsigned-byte-8 byte-int)
;;            (inline octet-bit-vector-zeroed 
;;                    octet-logbitp-1-or-0
;;                    octet-set-bit-vector-index)
;;            (optimize (speed 3)))
;;   (let ((bv (octet-bit-vector-zeroed)))
;;     (declare (bit-vector-octet bv))
;;     (dotimes (i 8 bv)
;;       (octet-set-bit-vector-index bv 
;;                                   (logxor i 7) 
;;                                   (octet-logbitp-1-or-0 i byte-int)))))
;;
;; (defun octet-to-bit-vector-2-5 (byte-int)
;;     (declare (type unsigned-byte-8 byte-int)
;;      (inline octet-bit-vector-zeroed
;;              octet-logbitp-1-or-0
;;              octet-set-bit-vector-index-xor
;;              octet-set-bit-vector-index-xor-if)
;;            (optimize (speed 3)))
;;   (loop 
;;      with bv = (octet-bit-vector-zeroed)
;;      for i upfrom 0 below 8
;;      do (octet-set-bit-vector-index-xor bv i (octet-logbitp-1-or-0 i byte-int))
;;      finally (return bv)))
;;
;; (defun octet-to-bit-vector-3 (byte-int)
;;   (declare (type unsigned-byte-8 byte-int)
;;            (inline octet-bit-vector-zeroed
;;                    octet-set-bit-vector-index-xor-if)
;;            (optimize (speed 3)))
;;   (let ((bv (octet-bit-vector-zeroed)))
;;     (declare (bit-vector-octet bv))
;;     (dotimes (i 8 bv)
;;       (octet-set-bit-vector-index-xor-if bv i byte-int))))
;;; ==============================

(defun byte-request-integer (array offset length &key little-endian sign-extend)
  (declare (byte-array array)
           ((integer  0 15)      offset)
           ((integer  1 16)     length)
           (optimize (speed 3)))
  (let ((value (loop
                  for i from 0 below length
                  for octet = (aref array (+ offset
                                             (if little-endian
                                                 i
                                                 (- length i 1))))
                  sum (ash octet (* i 8)))))
    (if (and sign-extend
             (logbitp (1- (* length 8)) value))
        (logior (lognot (1- (ash 1 (1- (* length 8))))) value)
        value)))

(define-compiler-macro byte-request-integer (&whole form array offset length &key little-endian sign-extend)
  (if (and (member length '(1 2 4)) 
           (member little-endian '(t nil))
           (member sign-extend '(t nil)))
      `(let* (,@(loop
                   for i from 0 below length
                   for var in '(byte-0 byte-1 byte-2 byte-3)
                   collect `(,var (aref ,array (+ ,offset
                                                  ,(if little-endian
                                                       i
                                                       (- length i 1))))))
              (value ,(elt '(#1=byte-0
                             #2=(dpb byte-1 (byte 8 8) #1#)
                             #3=(dpb byte-2 (byte 8 16) #2#)
                             (dpb byte-3 (byte 8 24) #3#))
                           (1- length))))
         ,(if sign-extend
              `(if (logbitp ,(1- (* length 8)) value)
                   (logior ,(lognot (1- (ash 1 (1- (* length 8))))) value)
                   value)
              'value))
      form))

;; (multiple-value-bind (arr len) (tt--number-to-byte-array 825973027016)
;;   ;; (typep arr 'byte-array))
;;   ;; (type-of arr))
;;   (byte-request-integer arr 0 len))
;;   (type-of (tt--number-to-byte-array 825973027016))
;; (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (5))
;; (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (5))
;;  (byte-request-integer arr 0 5)


;; :SOURCE monkeylib-binary-data/common-datatypes.lisp :WAS `swap-bytes'
(defun byte-swap (code)
  (declare (unsigned-byte-16 code))
  #-sbcl (assert (<= code #xffff))
  (rotatef (ldb (byte 8 0) code) (ldb (byte 8 8) code))
  code)

;; :NOTE Better to use byte-request-integer
(defun bytes-to-int (bytes start end)
  ;; (reduce (lambda (x y) (+ (* 256 x) y)) bytes :start start :end end))
  (reduce (lambda (x y) (logior (ash x 8) y)) bytes :start start :end end))

;;
(defun number-to-byte-array (num)
  ;; (number-to-byte-array 825973027016)
  ;; (logand 825973027016 255)          ;=> 200
  ;; (logand (ash 825973027016 -8) 255) ;=> 48
  ;; (logand (ash 3226457136 -8) 255)   ;=> 212
  ;; (logand (ash 12603348 -8) 255)     ;=> 79
  ;; (logand (ash 49231 -8) 255)        ;=> 192
  ;; (ash 192 -8)                       ;=> 0
  (declare ((integer 0 *) num))
  (if (zerop num)
      (values (make-array 1 :element-type 'unsigned-byte-8 :initial-element 0) 1)
      (let* ((type-cnt (byte-octets-for-integer num))
             (byte-arr (make-array type-cnt :element-type 'unsigned-byte-8 :initial-element 0)))
        (declare ((mod 17) type-cnt) ;; bail on any number bigger than 128bits
                 ((integer 1 *) num)
                 (byte-array byte-arr))
        (loop
           :for val = num :then (ash val -8)
           :for count downfrom (1- type-cnt) downto 0
           ;; Knock down all 1 bits above 255 to 0
           :do (setf (aref byte-arr count) (logand val #XFF))
           :finally (return (values byte-arr type-cnt))))))

;; (number-to-byte-array most-positive-fixnum)
; #(31 255 255 255)
; => 4
;
;; (multiple-value-bind (byts len) (number-to-byte-array 825973027016)
;;   (bytes-to-int byts 0 len))

;;; ==============================
(defun string-to-sha1-byte-array (string)
  ;; `uuid-digest-uuid-string'
  (declare (type string string))
  (let ((digester (ironclad:make-digest :sha1)))
    (declare (ironclad:sha1 digester))
    (ironclad:update-digest digester 
                            ;; :WAS (ironclad:ascii-string-to-byte-array string)
                            #-sbcl (flexi-streams:string-to-octets string :external-format :UTF-8)
                            #+sbcl (sb-ext:string-to-octets string :external-format :UTF-8))
    (ironclad:produce-digest digester)))


;;; ==============================
;;; :BIT-TWIDDLE-DOCUMENTATION
;;; ==============================

(fundoc 'boolean-to-bit
"Convert BOOLEAN to a bit \(an integer either 0 or 1\).~%~@
BOOLEAN should satisifty `mon:booleanp' or be of type \(integer 0 1\), signal an error if not.~%~@
Return value is as if by `cl:values':~%
 - when boolean is T or 1 return: 1,T or 1,1.~%
 - when boolean is null or 0 return: 0,'NIL or 0,0~%~@
When optional arg NO-ERROR is non-nil allow non \"boolean\" arguments without signalling.~%~@
If BOOLEAN is not T, NIL, 0, or 1 return: NIL,<BOOLEAN>~%~@
:EXAMPLE~%
 \(boolean-to-bit t\)~%
 \(boolean-to-bit nil\)~%
 \(boolean-to-bit \(\)\)~%
 \(boolean-to-bit 'nil\)~%
 \(boolean-to-bit '\(\)\)~%
 \(boolean-to-bit \(list\)\)~%
 \(boolean-to-bit 1\)~%
 \(boolean-to-bit 0\)~%
 \(boolean-to-bit \"not-a-boolean\"\)~%
 \(boolean-to-bit \"not-a-boolean\" t\)~%
 \(boolean-to-bit t t\)~%
 \(boolean-to-bit nil t\)~%
 \(boolean-to-bit 0 t\)~%
 \(boolean-to-bit 1 t\)~%~@
:SEE-ALSO `mon:digit-char-0-or-1', `mon:not-null', `mon:integer-or-null',
`mon:string-all-digit-char-0-or-1', `symbol-not-null'.~%▶▶▶")

(fundoc 'boolean-to-bit
"Convert BOOLEAN to a bit \(an integer either 0 or 1\).~%~@
BOOLEAN should satisifty `mon:booleanp' or be of type \(integer 0 1\), signal an error if not.~%~@
Return value is as if by `cl:values':~%
 - when boolean is T or 1 return: 1,T or 1,1.~%
 - when boolean is null or 0 return: 0,'NIL or 0,0~%~@
When optional arg NO-ERROR is non-nil allow non \"boolean\" arguments without signalling.~%~@
If BOOLEAN is not T, NIL, 0, or 1 return: NIL,<BOOLEAN>~%~@
:EXAMPLE~%
 \(boolean-to-bit t\)~%
 \(boolean-to-bit nil\)~%
 \(boolean-to-bit \(\)\)~%
 \(boolean-to-bit 'nil\)~%
 \(boolean-to-bit '\(\)\)~%
 \(boolean-to-bit \(list\)\)~%
 \(boolean-to-bit 1\)~%
 \(boolean-to-bit 0\)~%
 \(boolean-to-bit \"not-a-boolean\"\)~%
 \(boolean-to-bit \"not-a-boolean\" t\)~%
 \(boolean-to-bit t t\)~%
 \(boolean-to-bit nil t\)~%
 \(boolean-to-bit 0 t\)~%
 \(boolean-to-bit 1 t\)~%~@
:SEE-ALSO `mon:digit-char-0-or-1', `mon:not-null', `mon:integer-or-null',
`mon:string-all-digit-char-0-or-1', `symbol-not-null'.~%▶▶▶")

(fundoc 'byte-swap
"Return the CODE with its bytes rotated of CODE.~%~@
CODE is an integer value of type mon:unsigned-byte-16.~%~@
:EXAMPLE~%
 \(byte-swap #xF0FF\)~% ;=> 65520~%
 \(byte-swap #xFFF0\)~% ;=> 61695~%
 \(byte-swap \(byte-swap #xF0FF\)\)~% ;=> 61695
 \(byte-swap #XFFFF\)~% => 65535~%~@
e.g~%
 65535 \(16 bits, #xFFFF, #o177777, #b1111111111111111\)
 61695 \(16 bits, #xF0FF, #o170377, #b1111000011111111\)
 65520 \(16 bits, #xFFF0, #o177760, #b1111111111110000\)~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'string-to-sha1-byte-array
        "Return string as a SHA1 byte-array as if by `ironclad:make-digest'.~%~@
Arg STRING is a string and may contain UTF-8 characters.~%~@
:EXAMPLE~%
 \(let \(\(target-str \(mon-test:make-random-string 16\)\)\)
   \(values \(string-to-sha1-byte-array target-str\) target-str\)\)~@
:NOTE we can compare the output of `string-to-sha1-byte-array' with output
Emacs lisp' `sha1-binary':
 CL>  \(string-to-sha1-byte-array \"bubba\"\)
        => #\(32 193 148 189 4 164 89 163 52 78 106 202 121 61 200 118 132 25 134 11\)
 elisp> \(vconcat \(sha1-binary \"bubba\"\)\)
         => [32 193 148 189 4 164 89 163 52 78 106 202 121 61 200 118 132 25 134 11]~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'number-to-byte-array
"Return NUMBER as if by `cl:values' a byte-array and the count of its elements.~%~@
BYTE-ARRAY is in big-endian format with LSB as first elt and MSB as last elt.~%~@
The number represented by BYTE-ARRAY may be any positive integer representable
in 128 bits.~%~@
:EXAMPLE~%
 825973027016
 40 bits #xC04FD430C8 #o14011765030310 #b110000000100111111010100001100001100100~%
 \(number-to-byte-array 825973027016\)
 => #\(192 79 212 48 200\)
      LSB...........MSB~%
 (elt #(192 79 212 48 200) 4) => MSB~%
 (elt #(192 79 212 48 200) 0) => LSB~%
 (byte-request-integer (number-to-byte-array 825973027016) 0 5)~%
 => 825973027016~%~@
;; Roundtrip it:~%
 \(multiple-value-bind \(ba len\) \(number-to-byte-array 825973027016\)
   \(byte-request-integer ba 0 len\)\)
 => 825973027016~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'byte-request-integer
  "Decode an integer of LENGTH octets from ARRAY starting at OFFSET.~%~@
INTEGER may be any positive integer representable in 128 bits.
The OFFSET is effectively :start and 
LENGTH is :end where :end => \(+ offset length\)
x86-32 OSs are LITTLE-ENDIAN but RFC's want network-byte-order e.g. big-endian...~%~@
The SIGN-EXTEND is as per the following explanation snarfed from interwebs:~%~@
 Sign extension is the operation, in computer arithmetic, of increasing the
number of bits of a binary number while preserving the number's sign
\(positive/negative\) and value. This is done by appending digits to the most
significant side of the number, following a procedure dependent on the
particular signed number representation used.

For example, if six bits are used to represent the number \"00 1010\" \(decimal
positive 10\) and the sign extend operation increases the word length to 16 bits,
then the new \(big endian, i.e. the left-most bit is the most significant bit\)
representation is simply \"0000 0000 0000 1010\". Thus, both the value and the
fact that the value was positive are maintained.

If ten bits are used to represent the value \"11 1111 0001\" \(decimal negative 15\)
using two's complement, and this is sign extended to sixteen bits, the new
representation is \"1111 1111 1111 0001\". Thus, by padding the left side with
ones, the negative sign and the value of the original number are maintained.

In the x86 instruction set, used by most home PCs, there are two ways of doing
sign extension:

    * using the instructions cbw, cwd, cwde, and cdq (convert byte to word,
      doubleword, extended doubleword, and quadword, respectively; in the x86
      context, a byte has 8 bits, a word 16 bits, a doubleword and extended
      doubleword 32 bits, and a quadword 64 bits);

    * using one of the sign extended moves, accomplished by the movsx \(\"move
      with sign extension\"\) family of instructions.~%~@
:EXAMPLE~%~@
  \(byte-request-integer \(byte-number-to-byte-array 281474976710654\) 0 6\)~%
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'bytes-to-int
"Like request-integer but likely slower as results are accumulated as if by
`cl:reduce'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'octet-to-bit-vector
"Convert an unsigned-byte-8 integer to an array of type (SIMPLE-BIT-VECTOR 8).~%~@
The returned array containins the bits of integer set 1/0 according to the
`cl:logbitp' index into BYTE-INT's.~%~@
:EXAMPLE~%
 192 
 => (8 bits, #xC0, #o300, #b11000000~%
 \(octet-to-bit-vector 192\)
 => #*11000000
 \(loop
    for x upfrom 0 below 256
    collect \(octet-to-bit-vector x\)\)
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'octet-set-bit-vector-index
"Set the value of INDEX in BIT-VECTOR to 1-or-0.
BIT-VECTOR is an object of type: \(simple-bit-vector 8\).~%
INDEX is an integer of type: \(mod 8\).~%~@
1-or-0 is an integer of type: \(mod 2\).~%
:EXAMPLE~%
 \(let \(\(bv \(make-array 8 :element-type 'bit\)\)\)
   \(octet-set-bit-vector-index bv 7 \(octet-logbitp-1-or-0 0 255\)\)
   bv\)
:SEE-ALSO `octet-logbitp-1-or-0'.~%▶▶▶")

(fundoc 'octet-logbitp-1-or-0
        "Like `cl:logbitp' but return 1 or 0 instead of T or NIL.~%~@
Arg INDEX is as `cl:logbitp' but must be of type (mod 8).
Arg UNSIGNED-BYTE-8 is an integer of type `mon:unsigned-byte-8'.~%
:EXAMPLE~%
 \(= \(octet-logbitp-1-or-0 3 247\) 0\)~%
 \(= \(octet-logbitp-1-or-0 4 247\) 1\)~%
 \(loop 
    with int = 254 
    for idx downfrom 7 to 0 collect \(octet-logbitp-1-or-0 idx int\)\)~%
 \(loop 
    with int = 254 
    for idx downfrom 7 to 0 collect \(logbitp idx int\)\)~%
  #b11110111 => 247
  1 1 1 1 0 1 1 1
  7 6 5 4 3 2 1 0
  0 1 2 3 4 5 6 7~%
 \(loop 
    with int = 247
    for x downfrom 7 to 0
    for y upfrom 0 below 8
    collect \(cons x \(octet-logbitp-1-or-0 x int\)\) into x-rslt
    collect \(cons y \(octet-logbitp-1-or-0 y int\)\) into y-rslt
    finally \(return \(list :downfrom x-rslt :upfrom \(nreverse y-rslt\)\)\)\)
 => \(:DOWNFROM \(\(7 . 1\) \(6 . 1\) \(5 . 1\) \(4 . 1\) \(3 . 0\) \(2 . 1\) \(1 . 1\) \(0 . 1\)\)
     :UPFROM   \(\(7 . 1\) \(6 . 1\) \(5 . 1\) \(4 . 1\) \(3 . 0\) \(2 . 1\) \(1 . 1\) \(0 . 1\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'octet-bit-vector-zeroed
        "Return an object of type `mon:bit-vector-octet' with all bits zeroed.~%~@
:EXAMPLE~%
 \(octet-bit-vector-zeroed\)~%
 \(typep \(octet-bit-vector-zeroed\) 'bit-vector-octet\)~%
 \(loop for bit across \(octet-bit-vector-zeroed\) always \(zerop bit\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'make-array-of-octet-bit-vectors
"Return an array of 256 elts each containing an array of type: \(SIMPLE-BIT-VECTOR 8\)~%~@
The elts of array are indexed by their octet value as generated with `mon:octet-to-bit-vector'.~%~@
:EXAMPLE~%
 (make-array-of-octet-bit-vectors)~%
 (aref (make-array-of-octet-bit-vectors) 247)
  => #*11110111~%
 (octet-to-bit-vector 247)
  => #*11110111~%
 (equal (aref (make-array-of-octet-bit-vectors) 247) 
        (octet-to-bit-vector 247))
 => T~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'number-to-bit-list
        "Convert UNSIGNED-INTEGER \(a non-negative integer\) into a list of 1's and 0's.~%~@
:EXAMPLE~%
 \(number-to-bit-list most-positive-fixnum\)
 \(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1\)~%
 \(number-to-bit-list \(1+ most-positive-fixnum\)\)
 => \(1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0\)~%~@
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
