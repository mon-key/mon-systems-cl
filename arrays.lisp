;;; :FILE mon-systems/arrays.lisp
;;; ==============================

;;; ==============================
;;
;; `alexandria:write-byte-vector-into-file', `read-file-into-byte-vector'
;;
;;; ==============================


(in-package #:mon)
;; *package*

(defun bool-vector-p (object)
  (typep object 'bool-vector))

(defun make-bool-vector (length init)
  (declare (array-length length)
           ((or bit boolean) init))
  (make-array length 
              :element-type 'bit 
              :initial-element (etypecase init
                                 (bit (or (and (= init 1) 1) 0))
                                 (boolean (or (and init 1) 0)))))

(defun bit-vector-to-string (input-bit-vector)
  (declare (type simple-bit-vector input-bit-vector)
           (optimize (speed 3) (space 0) (safety 1)))
  (let* ((len    (length input-bit-vector))
         (result (make-string len :initial-element #\0)))
    `(declare (type array-index len)
              (type (simple-array character (,len)) result))
    (dotimes (i len result)
      (and (= (sbit input-bit-vector i) 1)
           (setf (char result i) #\1)))))

(defun string-to-bit-vector (input-string)
  (declare (type simple-string-or-null input-string)
           (optimize (speed 3) (space 0) (safety 1)))
  (unless (simple-string-null-or-empty-p input-string)
    (let* ( ;; :WAS (s (string-left-trim " " 
           ;;                    (string-right-trim " "  (the simple-string input-string))))
           (s (string-trim-whitespace (the simple-string input-string)))
           (len (1- (length (the simple-string s))))
           (result (make-array (1+ len) 
                               :element-type 'bit 
                               :initial-element 0)))
      ;; On SBCL we won't get to the second type decl. when `s` is the empty
      ;; string b/c of the first declaration and NIL can't happen per the
      ;; `unless' form above. But, even if we did, we wouldn't get further b/c
      ;; where `s` is "" `len` is -1 and the lower bounds of mon:index is 0.
      `(declare (type string-all-digit-char-0-or-1 s)
                (type array-length len) 
                (type (simple-bit-vector ,len) result))
      (loop
         :for flip0 :from 0 :to len
         :do (or (eq (schar s flip0 ) #\0)
                 (setf (bit result flip0) 1))
         :finally (return result)))))

;; :SOURCE ironclad/src/util.lisp :WAS `byte-array-to-hex-string'
(defun byte-array-to-hex-string (vector &key (start 0) end (element-type 'base-char))
  (declare ((simple-array (unsigned-byte 8) *) vector)
           ;;(type fixnum start)
	   (type array-index start)
           (type (or null array-length) end)
	   (type array-index start)
           (optimize (speed 3) (safety 1)))
  (let* ((end (or end (length vector)))
         #+sbcl (length (- end start))
	 #-sbcl (or (and (minusp start) (error "keyword START should not be `minusp'"))
		  (length (- end start)))
         (hexdigits #.(coerce "0123456789abcdef" 'simple-base-string)))
    (declare (array-length end length))
    (loop :with string = (ecase element-type
                           ;; so that the compiler optimization can jump in
                           (base-char (make-string (* length 2)
                                                   :element-type 'base-char))
                           (character (make-string (* length 2)
                                                   :element-type 'character)))
       :for i :from start :below end
       :for j :from 0 :below (* length 2) :by 2
       :do (let ((byte (aref vector i)))
	     `(declare (optimize (safety 1))
		       (,element-type string))
	     (setf (aref string j)
		   (aref hexdigits (ldb (byte 4 4) byte))
		   (aref string (1+ j))
		   (aref hexdigits (ldb (byte 4 0) byte))))
       :finally (return string))))

;; :SOURCE Ironclad/src/util.lisp :WAS `ascii-string-to-byte-array'
;; What prompted me to play with that implementation were its original declarations
;; esp. wrt cl:null and fixnum. These struck me as wrong headed in lieu of speed/safety.
(defun string-ascii-to-byte-array (string &key (start 0) end)
  (declare (type array-index start)
	   (type (or null array-length) end)
	   (optimize (speed 3) (safety 1) (debug 0)))
  (let* ((str-len
          (and 
           (or (string-not-empty-p string)
               (string-empty-error :w-sym 'string-ascii-to-byte-array 
                                   :w-type 'function
                                   :w-locus 'string
                                   :signal-or-only nil))
           (or (and (latin-1-simple-string-p (the string string))
                    (ref-bind len-if (length (the simple-string string))
                      (or (and (typep len-if 'array-length) (the array-length len-if))
                          (error 'type-error :datum len-if :expected-type 'array-length))))
               (simple-error-mon :w-sym 'string-ascii-to-byte-array
                                 :w-type 'function
                                 :w-spec "arg STRING not `latin-simple-string-p'"
                                 :w-got string
                                 :w-type-of t
                                 :signal-or-only nil))))
         (vec ;;(or (and (typep str-len 'array-length)
          (make-array (the array-length str-len) :element-type '(unsigned-byte 8)))
         ;;(make-array str-len :element-type '(unsigned-byte 8))))
         (end (or end (the array-length str-len))))
    (declare (type simple-string string)
             (type array-length end))
    (loop 
       :for i :from start :below end 
       :do (setf (aref vec i) (char-code (schar string i)))
       :finally (return vec))))
;;
;; 
#+sbcl
(defun byte-array-to-string (vector &key (start 0) end (external-format :default))
  (declare ((vector (unsigned-byte 8)) vector))
  (sb-ext:octets-to-string vector 
                           :start start 
                           :end end 
			   ;;sb-impl::*external-formats*
                           :external-format external-format)) 

#+sbcl
(defun string-to-byte-array (string &key (start 0) end null-terminate
                             (external-format :default))
  (declare (type string string))
  (sb-ext:string-to-octets string 
                           :start start 
                           :end end 
			   ;;sb-impl::*external-formats*
                           :external-format external-format 
                           :null-terminate null-terminate))

;;  Destructively truncate simple vector VECTOR to LENGTH.
#+sbcl
(defun vector-shrink (vector length)
  (sb-kernel:shrink-vector vector length))

#+sbcl
(define-compiler-macro vector-shrink (vector length)
  `(sb-kernel:shrink-vector ,vector ,length))

;; :SOURCE sbcl/src/compiler/target-disassem.lisp :WAS `grow-vector'
(defun vector-grow (vec new-len &optional initial-element)
  (declare (type vector vec)
           (type fixnum new-len))
  (let ((new
         (make-sequence `(vector ,(array-element-type vec) ,new-len)
                        new-len
                        :initial-element initial-element)))
    (dotimes (i (length vec))
      (setf (aref new i) (aref vec i)))
    new))

;; SB-SYS:FD-STREAM
;; :COURTESY cllib/math.lisp :WAS `remove-elements'
(defun vector-remove-elts (psn-lst vec)
  (let* ((vec-size (length vec))
         (ret (make-array (- vec-size (length psn-lst)))))
    (loop 
       :with ret-pos = 0 
       :with rem = (pop psn-lst)
       :for vec-pos :upfrom 0 :while rem
       :if (= rem vec-pos) 
        :do (setq rem (pop psn-lst))
       :else 
        :do (setf (aref ret ret-pos) (aref vec vec-pos)) (incf ret-pos)
       :end
       :finally (replace ret vec :start1 ret-pos :start2 vec-pos))
    ret))

;; :NOTE sbcl/src/code/target-extensions.lisp
(defun vector-binary-search (value vector &key (key #'identity))
  ;; Binary search for simple vectors
  (declare (simple-vector vector))
  #+sbcl (sb-impl::binary-search value vector key)
  #-sbcl (labels ((recurse (start end)
                    (when (< start end)
                      (let* ((i (+ start (truncate (- end start) 2)))
                             (elt (svref vector i))
                             (key-value (funcall key elt)))
                        (cond ((< value key-value)
                               (recurse start i))
                              ((> value key-value)
                               (recurse (1+ i) end))
                              (t
                               elt))))))
           (recurse 0 (length vector))))

;;; :SOURCE garnet-20030525/kr/kr.lisp :WAS `copy-extend-array'
(defun vector-copy-extend (oldarray oldlen newlen)
  (let ((result (make-array newlen)))
    (dotimes (i oldlen)
      (setf (svref result i) (svref oldarray i)))
    result))

;; :SOURCE PCL Chapter 23 p 305
(defun nshuffle-vector (vector)
  (declare ((simple-array *) vector))
  (loop for idx downfrom (1- (length vector)) to 1
     for other = (random (1+ idx))
     do (unless (= idx other)
          (rotatef (aref vector idx) (aref vector other))))
  vector)

(defun shuffle-vector (vector)
  (declare ((simple-array *) vector))
  (nshuffle-vector (copy-seq vector)))

;; #+sbcl (defun dovector (elt vector &optional result &rest body)
;;       (sb-int::dovector elt vector result) ;body))

;;; :SOURCE ltk-0.91/ltk.lisp
(defun make-string-adjustable (&optional (string ""))
  (declare (string string))
  (make-array (or (and string  (length string)) 0)
              :element-type 'character
              :initial-contents string 
              :adjustable t 
              :fill-pointer t))

;; :SOURCE (URL `http://groups.google.com/group/comp.lang.lisp/msg/f527904bd5167a83')
;; :COURTESY Erik Naggum comp.lang.lisp :DATE 2004/01/17 
;; :SUBJECT Re: simple-array vs displaced-to
(defun array-get-undisplaced (array)  
  (let ((length (length array))
        (start 0))
    (declare (array-length length))
    (loop
       (multiple-value-bind (to offset) (array-displacement array)
         (if to
             (setq array to
                   start (+ start offset))
             (return (values array start (+ start length))))))))

;;; ==============================
;; :SOURCE sbcl/src/compiler/bit-util.lisp
;; :NOTE These are all declaimed inline
;; SBCL uses these in conjunction with constraints via the sparse sets of :FILE sset.lisp
;; 
;; ,----
;; | Historically, CMUCL and SBCL have used a sparse set implementation
;; | for which most operations are O(n) (see sset.lisp), but at the
;; | cost of at least a full word of pointer for each constraint set
;; | element.  Using bit-vectors instead of pointer structures saves a
;; | lot of space and thus GC time (particularly on 64-bit machines),
;; | and saves time on copy, union, intersection, and difference
;; | operations; but makes iteration slower.  Circa September 2008,
;; | switching to bit-vectors gave a modest (5-10%) improvement in real
;; | compile time for most Lisp systems, and as much as 20-30% for some
;; | particularly CP-dependent systems.
;; `---- :SOURCE :FILE sbcl/src/compiler/constraint.lisp
;;     
;; ,----
;; | Bit-vectors win over lightweight hashes for copy, union,
;; | intersection, difference, but lose for iteration if you iterate
;; | over the whole vector.  
;; `---- :SOURCE comments for structure def of `conset'
;;
;;
;; `clear-bit-vector' and `bit-vector-copy' rely on
;; `sb-c::fill'/`sb!sequence:fill' which in turn relies on
;; `sb-kernel:vector-fill*' via an indirection through `sb-impl::seq-dispatch'
;;; ==============================

;;; :SOURCE sbcl/src/compiler/bit-util.lisp :WAS `clear-bit-vector'
#+sbcl
(defun bit-vector-clear (bool-vec)
  (declare (type simple-bit-vector bool-vec))
  ;; :WAS (fill vec 0))
  (sb-kernel:vector-fill* bool-vec 0 0 (length bool-vec)))
;;
;;; :SOURCE sbcl/src/compiler/bit-util.lisp :WAS `set-bit-vector'
#+sbcl
(defun bit-vector-set (bool-vec)
  (declare (type simple-bit-vector bool-vec))
  ;; :WAS (fill vec 1))
  (sb-kernel:vector-fill* bool-vec 1 0 (length bool-vec)))
;;
;;; :SOURCE sbcl/src/compiler/bit-util.lisp :WAS `bit-vector-copy'
(defun bit-vector-copy (bool-vec)
  (declare (type simple-bit-vector bool-vec))
  (copy-seq bool-vec))
;;
;; :SOURCE sbcl/src/compiler/bit-util.lisp :WAS `bit-vector-replace'
(defun bit-vector-replace (in-bool-vec w-bool-vec)
  (declare (type simple-bit-vector in-bool-vec w-bool-vec))
  ;; :NOTE Maybe use this instead?:
  ;; (sb-impl::vector-replace-from-vector* in-bool-vec w-bool-vec 0 nil 0 nil)
  (replace in-bool-vec w-bool-vec))

;; (defun vector-fill* (sequence item start end)
;;   (with-array-data ((data sequence)
;;                     (start start)
;;                     (end end)
;;                     :force-inline t
;;                     :check-fill-pointer t)
;;     (let ((setter (!find-data-vector-setter data)))
;;       (declare (optimize (speed 3) (safety 1)))
;;       (do ((index start (1+ index)))
;;           ((= index end) sequence)
;;         (declare (index index))
;;         (funcall setter data index item)))))

;;; ==============================
;;; Follwing functions are GPLv3 Copyright (c) 2008-2011 Keith James. All rights reserved.
;;; :SOURCE uk.co.deoxybyte-utilities/vector-utilities 
;;; `vector-positions', `vector-split-indices', `vector-split'
;;; ==============================
(defun vector-positions (elt vector &key (start 0) end (test #'eql))
  (declare (optimize (speed 3) (debug 0)))
  (declare (type vector vector)
           (type function test))
  (let ((end (or end (length vector))))
    (declare (type array-index start end))
    ;;(let ((vector #(a b c d)))
    (assert (<= 0 start end (length vector))
            (start end)
            "bad index into VECTOR, START and END must satisfy (<= 0 start end ~d)~% ~
             got-start: ~d~% ~
             got-end: ~D~% ~
             vector-spec: ~S~%"
            (length vector)
            start
            end 
            (type-of vector))
    (loop 
       for i from start below end
       when (funcall test elt (aref vector i))
       collect i)))
;; *print-readably*
(defun vector-split-indices (elt vector &key (start 0) end (test #'eql))
  "Returns two values, a list of start indices and a list of end
indices into VECTOR between START and END such that if used as
start/end arguments to subseq, VECTOR will be split on ELT. ELT is
compared with elements in VECTOR using TEST, which defaults to EQL."
  (declare (optimize (speed 3) 
                     ;;(safety 1)
                     (debug 0)))
  (declare (type vector vector))
  (let ((end (or end (length vector))))
    (declare (type array-index start end))
    (assert (<= 0 start end (length vector)) (start end)
            "must satisfy (<= 0 start end ~d)" (length vector))
    (let ((positions (vector-positions elt vector
                                       :start start :end end :test test)))
      (if positions
          (loop
             with starts = ()
             with ends = ()
             for pos of-type array-index in positions
             and prev = start then (the array-index (1+ pos))
             maximize pos into last-pos
             do (progn
                  (push prev starts)
                  (push pos ends))
             finally (progn
                       (push (the array-index (1+ last-pos)) starts)
                       (push end ends)
                       (return
                         (values (nreverse starts) (nreverse ends)))))
          nil))))
;;;; split-sequence:split-sequence
(defun vector-split (elt vector &key (start 0) end (test #'eql)
                     remove-empty-subseqs displace-to-vector)
  (let ((end (or end (length vector)))
        (elt-type (array-element-type vector)))
    (assert (<= 0 start end (length vector)) (start end)
            "must satisfy (<= 0 start end ~d)" (length vector))
    (multiple-value-bind (starts ends)
        (vector-split-indices elt vector :start start :end end :test test)
      (cond ((and starts ends)
             (loop
                for i in starts
                for j in ends  
                when (not (and remove-empty-subseqs
                               (= i j)))
                collect (if displace-to-vector
                             (make-array (- j i)
                                         :element-type elt-type
                                         :displaced-to vector
                                         :displaced-index-offset i)
                          (subseq vector i j))))
            (displace-to-vector
             (make-array (- end start)
                         :element-type elt-type
                         :displaced-to vector
                         :displaced-index-offset start))
            (t
             (list (subseq vector start end)))))))







;;; ==============================
;;; :ARRAYS-DOCUMENTATION
;;; ==============================

(fundoc 'vector-grow
"Return a vector with same contents as VEC plus new cells and total size NEW-LEN.~%~@
Any additional elements are initialized to INITIAL-ELEMENT.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:vector-shrink'.~%▶▶▶")

(fundoc 'make-bool-vector
"Return a new bool-vector \(a simple-bit-vector\) of length LENGTH, using INIT for
each element.~%~@
LENGTH must be an integer greater than or equal 0.~%~@
Init should be either 1 or 0. If it is  `t' or `nil' it will be coereced to 1 or 0.
Any other value for init will signal an error.~%~@
The return ed object will be of type `bool-vector' and will satisfy `bool-vector-p'.~%~@
:EXAMPLE~%
 \(make-bool-vector 8 1\)~%
 \(equal \(make-bool-vector 8 1\) \(make-array 8 :element-type 'bit :initial-element 1\)\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `bool-vector-p', `bit-vector-replace', `bit-vector-copy',
`bit-vector-set', `bit-vector-clear'.~%▶▶▶")

(fundoc 'bool-vector-p
"Return non-nil if object is a bool-vector \(a simple-bit-vector\).~%~@
Object is a bool-vector when its type is `bool-vector', that is a
simple-bit-vector with element-type bit.~%~@
:EXAMPLE~%
 \(bool-vector-p \(make-bool-vector 8 1\)\)~%
 \(bool-vector-p  \(make-array 8 :element-type 'bit :initial-element 1\)\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `make-bool-vector', `bit-vector-replace', `bit-vector-copy',
`bit-vector-set', `bit-vector-clear'.~%▶▶▶")

(fundoc 'bit-vector-copy
"Return a copy of the `simple-bit-vector' BOOL-VEC.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `bool-vector-p', `make-bool-vector', `bit-vector-replace',
`bit-vector-set', `bit-vector-clear'.~%▶▶▶")

(fundoc 'bit-vector-replace
"Replace the bits in IN-BOOL-VEC with the bits in W-BOOL-VEC.~%~@
Both IN-BOOL-VEC and W-BOOL-VEC are of type `simple-bit-vector'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `bool-vector-p', `make-bool-vector',. `bit-vector-copy',
`bit-vector-set', `bit-vector-clear'~%▶▶▶")

(fundoc 'string-to-bit-vector
"Convert the simple-string INPUT-STRING to a bit-vector.~%~@
:EXAMPLE~%
 \(string-to-bit-vector \"  000101001   \"\)
 ;=> #*000101001~%
 \(type-of \(string-to-bit-vector \"  000101001   \"\)\)~%
 ;=>\(SIMPLE-BIT-VECTOR 9\)~%
 \(string-to-bit-vector \(\)\)~%
 \(string-to-bit-vector \(bit-vector-to-string #*0000010101\)\)~%~@
:SEE-ALSO `mon:bit-vector-to-string', `mon:bit-vector-set',
`mon:string-ascii-to-byte-array', `mon:byte-array-to-string',
`mon:string-to-byte-array', `mon:bit-vector-to-string', `mon:string-to-bit-vector',
`mon:char-char-length', `mon:code-point'.~%▶▶▶")

(fundoc 'bit-vector-to-string
"Convert INPUT-BIT-VECTOR to a simple-string with each char either #\\1 or #\\0.~%~@
INPUT-BIT-VECTOR should satisfy `cl:simple-bit-vector-p', signal an error if not.~%~@
:EXAMPLE~%
 \(bit-vector-to-string #*0000010101\)~%
 \(bit-vector-to-string \(string-to-bit-vector \"0000010101\"\)\)~%~@
:SEE-ALSO `mon:string-to-bit-vector', `mon:string-to-number', `mon:string-to-char'
`mon:string-ascii-to-byte-array', `mon:byte-array-to-string',
`mon:string-to-byte-array', `mon:bit-vector-to-string', `mon:string-to-bit-vector',
`mon:char-char-length', `mon:code-point'.~%▶▶▶")

#+sbcl
(fundoc 'bit-vector-set
";; Fill the `simple-bit-vector' BOOL-VEC with ones.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `bool-vector-p', `make-bool-vector', `bit-vector-replace',
`bit-vector-copy', `bit-vector-clear'.~%▶▶▶")

#+sbcl
(fundoc 'bit-vector-clear
"Clear the bits `simple-bit-vector' BOOL-VEC to zeros.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `bool-vector-p', `make-bool-vector', `bit-vector-replace',
`bit-vector-copy', `bit-vector-set'.~%▶▶▶")

(fundoc 'vector-remove-elts
  "Remove the elements in PSN-LST from the vector VEC.~%~@
:EXAMPLE~%
 \(vector-remove-elts '\(0 1 3 5 7\) #\(a b c d e f g h\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'nshuffle-vector
"Destructively shuffle VECTOR in place using Fisher-Yates algorithm.
Return VECTOR.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'shuffle-vector
"Return a shuffled copy of vector.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `nshuffle-vector'.~%▶▶▶")

(fundoc 'array-get-undisplaced
"Return the fundamental array and the start and end positions into
a displaced array.~%
 \"One common optimization when working with non-simple arrays is to dig out the
  underlying simple-array and the start and end positions that a displaced array
  makes into one convenient object.~%
  The function `cl:array-displacement' returns the values of the arguments
  :DISPLACED-TO and and :DISPLACED-INDEX-OFFSET given to `cl:make-array' or
  `cl:adjust-array', or NIL and 0 if it was not displaced.\"
    -- Erik Naggum, C.L.L :DATE 2004/01/17 Re: simple-array vs displaced-to~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'make-string-adjustable 
"Convenience feature for `cl:make-array' specialized for strings.~%~@
Optional arg STRING is when non-nil is a string to use as :initial-contents for returned array.~%~@
Specs of returned array are as follows:~%
 :element-type     'character
 :initial-contents [<STRING> | \"\"]
 :fill-pointer     [0 | \(length <STRING>\)]
 :adjustable       t~%~@
:EXAMPLE~%
 \(array-has-fill-pointer-p \(make-string-adjustable\)\)~%
 \(adjustable-array-p \(make-string-adjustable\)\)~%
 \(fill-pointer \(make-string-adjustable \"bubba\"\)\)~%
 \(fill-pointer \(make-string-adjustable\)\)~%
 \(let \(\(v-p-e-string \(make-string-adjustable\)\)\)~%
   \(vector-push-extend #\\a v-p-e-string\)~%
   \(values v-p-e-string \(fill-pointer v-p-e-string\)\)\)~%~@
:SEE-ALSO `cl:make-string', `cl:with-output-to-string', `cl:format'.~%▶▶▶")

;; #+sbcl (setf (documentation 'dovector 'function)
;;       #.(format nil
;; "like `dolist', but with one-dimensional arrays~%~@
;; :EXAMPLE~%~%~@\(let \(gthr\)
;;   \(dovector \(v #\(#\\v #\\e #\\c #\\t #\\o #\\r\)
;;             \(print \(nreverse gthr\)\)\)
;;     \(push \(string v\) gthr\)\)\)~%~@
;; :SEE-ALSO `<XREF>'.~%▶▶▶"))

(fundoc 'string-ascii-to-byte-array
  "Convert STRING to an array of type \(simple-array \(unsigned-byte 8\) *\).~%
Each character of STRING should  have char-codes in the range of ISO-latin-1 (e.g. 0-255)
Signal an error if STRING contains any character whose CHAR-CODE is greater than 255.~%~@
:EXAMPLE~%
 \(string-ascii-to-byte-array \"àéÉíóü\"\)
 ;=> #(224 233 201 237 243 252)~%
 \(type-of \(string-ascii-to-byte-array \"aeiou\"\)\)~%
 \(string-ascii-to-byte-array \"\(string-ascii-to-byte-array \\\"áéíóü\\\"\)\"\)~%
 \(byte-array-to-string 
  \(string-ascii-to-byte-array \"\(string-ascii-to-byte-array \\\"áéíóü\\\"\)\"\) 
  :external-format :iso-8859-1\)~%~@
Following fails successfully:~%
 \(string-ascii-to-byte-array \"aeiou►\"\)~%
:SEE-ALSO `mon:string-ascii-to-byte-array', `mon:byte-array-to-string',
`mon:string-to-byte-array', `mon:bit-vector-to-string', `mon:string-to-bit-vector',
`mon:char-char-length', `mon:code-point'.~%▶▶▶")
 
(fundoc 'byte-array-to-hex-string
"Convert subsequence of VECTOR between START and END to string
Returnded String is a hexadecimal representation of the bytes of VECTOR.~%~@
VECTOR is a vector of type \(unsigned-byte 8\).~%~@
ELEMENT-TYPE controls the element-type of the returned string.
Default is `cl:base-char'.~%~@
:EXAMPLE~% 
 \(byte-array-to-hex-string \(string-ascii-to-byte-array \"áíéÉó\"\)\)~%
 \(parse-integer 
  \(subseq \(byte-array-to-hex-string \(string-ascii-to-byte-array \"áíéÉó\"\)\) 0 2\)
  :radix 16\)~%
:NOTE When sb-unicode is in *features*, following may not return as expected:~%~@
 \(byte-array-to-hex-string \(string-to-byte-array \"á\"\)\)
 ;=> \"c3a1\"~%
 \(parse-integer \"c3a1\":radix 16\)
 ;=> 50081, 4~%
 (code-char 50081) 
 ;=> #\HANGUL_SYLLABLE_SSYEOG~%
 \(byte-array-to-hex-string 
  \(string-to-byte-array \"á\" :external-format :iso-8859-1\)\)~%~@
 ;=> \"e1\"
 (parse-integer \"e1\" :radix 16)
 ;=> 225, 2~%
 \(code-char 255\)
 ;=> #\LATIN_SMALL_LETTER_Y_WITH_DIAERESIS~%~@
:SEE-ALSO `mon:string-ascii-to-byte-array', `mon:byte-array-to-string',
`mon:byte-array-to-string', `mon:bit-vector-to-string', `mon:string-to-bit-vector',
`mon:char-char-length', `mon:code-point'.~%▶▶▶")

#+sbcl 
(fundoc 'byte-array-to-string
"Convert octets of VECTOR to string.~%~@
VECTOR is an array of element-type \(unsigned-byte 8\). Signal an error if not.~%~@
Keyword EXTERNAL-FORMAT is element in hash-table `sb-impl::*external-formats*'.
Keyword START is an index into VECTOR to begin from. Default is 0.~%~@
Keyword END is an index into VECTOR to end at.~%~@
:EXAMPLE~%
 \(byte-array-to-string \(make-array 3 :element-type '\(unsigned-byte 8\)
                                   :initial-contents '\(32 59 102\)\)\)~%
 \(loop for kk being each hash-key of sb-impl::*external-formats* collect kk\)~%~@
:SEE-ALSO `mon:string-to-byte-array', `mon:string-ascii-to-byte-array'.~%▶▶▶")

#+sbcl
(fundoc 'string-to-byte-array
"Convert STRING from START to END to a vector of octets.~%~@
Return value is of type (simple-array (unsigned-byte 8) *).~%~@
Keyword EXTERNAL-FORMAT defaults to `sb-impl::*default-external-format*'.
This is :utf-8 when (member :sb-unicode *features*) else default is :latin-1.~%~@
Keyword NULL-TERMINATE when non-nil says to append the integer 0 as last element
of returned vector which may be useful when passing data between C oriented
data structures/strings.~%~@
:EXAMPLE~%
 \(string-to-byte-array \"áíéÉó\"\)
 ;=> #\(195 161 195 173 195 169 195 137 195 179\)~%
 \(string-to-byte-array \"áíéÉó\" :external-format :iso-8859-1\)
 ;=> #\(225 237 233 201 243\)~%
 \(string-to-byte-array \"áíéÉó\" :null-terminate t\)
 ;=> #\(195 161 195 173 195 169 195 137 195 179 0\)~%
:SEE-ALSO `<XREF>'.~%▶▶▶")

;;; ==============================
(fundoc 'vector-split
 "Return a list of vectors splitting VECTOR at ELT, between START and END.~%~@
ELT is compared with elements in VECTOR using 
Keyword START is and END are as with `cl:position'.~%~@
Keyword TEST is a function, default is EQL.~%~@
Keyword REMOVE-EMPTY-SUBSEQS when non-nil indicates empty subsequences should be
omitted from returned list.~%~@
Keyword DISPLACE-TO-VECTOR when non-nil indicates returned subsequences should
be displaced to the actual subsequences within VECTOR. In such case subsequences
will share structure with VECTOR.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")
;;
(fundoc 'vector-positions
"Return list indices into VECTOR from START to END where ELT satisfies TEST.~%~@
VECTOR is an sequece object of type `cl:array'.~%~@
Keywords START and END ara as `cl:position'~%~@
Keyword TEST defaults to `cl:eql'
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'vector-binary-search
 "Search for VALUE in VECTOR by binary search.~%~@
VECTOR is a sorted vector to search.~%~@
VALUE is an object to be find.~%~@
Keyword KEY is a function function with which to transform VECTOR elements.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
