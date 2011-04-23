;;; :FILE-CREATED <Timestamp: #{2011-01-29T17:51:24-05:00Z}#{11046} - by MON>
;;; :FILE tests/test.lisp
;;; ==============================


(in-package #:mon-test)
;; *package*

(defvar *random-chars* 
  ;; SBCL specific
  (make-array 282 :element-type 'character :initial-contents
              (loop 
                 for ascii upfrom 33 below 127 ;; (* (- 127 33) 3) =>282
                 for latin upfrom 161
                 for higher-latin upfrom 7680
                 collect (code-char ascii)
                 nconc (list (code-char latin) (code-char higher-latin)))))

(defun make-random-inverted-number-array ()
  (let ((arr (make-array 320))
        (inv-cons '((128  #XFF)
                    (64   #XFFFF)
                    (48   #XFFFFFF)
                    (32   #XFFFFFFFF)
                    (24   #XFFFFFFFFFFFF)
                    (16   #XFFFFFFFFFFFFFFFF)
                    (8    #XFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF))))
    (loop 
       with idx = -1  
       for (a  b) in inv-cons
       do (loop
             repeat a
             for x = (random b)
             ;;collect x)))
             do  (setf (aref arr (incf idx)) x))
       finally (return (mon:nshuffle-vector arr)))))

(defun make-random-char-array (n)
  (loop 
     with rnd-array = (make-array n :element-type 'character :initial-element #\Nul)
     with rnd-char-bnd = (1- (length *random-chars*))
     for rnd-char = (aref *random-chars* (random rnd-char-bnd))
     for idx upfrom 0 below n
     do (setf (aref rnd-array idx) rnd-char)
     finally (return  rnd-array)))

(defun make-random-string (&optional str-len)
  (declare ((or null (integer 0 *)) str-len))
  (loop 
     with randlen = (or (and str-len (not (zerop str-len)) str-len)
                        (loop 
                           for i = (random 16)
                           until (plusp i)
                           finally (return i)))
     with str = (make-array randlen :element-type 'character :initial-element #\Nul)
     with rnd-str = (make-random-char-array randlen)
     for put from 0 below randlen
     do (setf (aref str put) (aref rnd-str put))
     finally (return str)))

(defun string-insert-char-test-VALS ()
  (let* ((len (loop
                 for x = (random 16)
                 until (plusp x)
                 finally (return x)))
         (rnd-idx (random len)) 
         (rnd-str (make-random-string len))
         (rnd-char (char rnd-str rnd-idx)))
    (list rnd-str rnd-char rnd-idx)))

(defun string-insert-char-test-VALS (&optional str-len)
  (when str-len
    (assert (and (integerp str-len) (> str-len 4))
            (str-len)
            "str-len must be > 4, got: ~S"
            str-len))
  (let* ((len (loop
                 for x = (random (or str-len 16))
                 until (plusp x)
                 finally (return x)))
         (rnd-idx (random len)) 
         (rnd-str (make-random-string len))
         (rnd-char (char rnd-str rnd-idx)))
    (list rnd-str rnd-char rnd-idx)))

(defun string-insert-char-TEST (ntimes w-fun1 &optional w-fun2 w-str-len)
  (declare ((integer 1 *) ntimes)
           ((or (integer 4 *) null) w-str-len))
  (let ((vals (make-hash-table :size ntimes)))
    (loop 
       for key from 0 below ntimes
       do (setf (gethash key vals) (string-insert-char-test-VALS w-str-len)))
    (format t "~%with-function: ~S~%iterations: ~D~%" w-fun1 ntimes)
    (sb-ext:gc)
    (time
     (loop 
        for k being each hash-key of vals
        using (hash-value value)
        do (apply w-fun1 value)))
    (when w-fun2
      (format t "~%with-function: ~S~%iterations: ~D~%" w-fun2 ntimes)
      (sb-ext:gc)
      (time
       (loop 
          for k being each hash-key of vals
          using (hash-value value)
          do (apply w-fun2 value)))))
  (sb-ext:gc))

;; (string-insert-char-TEST 100000 #'mon:string-insert-char)
;; (string-insert-char-TEST 1000000 #'mon:string-insert-char)


;;; ==============================
;;; :TEST-VARIABLES-DOCUMENTATION
;;; ==============================

(mon:vardoc '*random-chars*
"An array of 282 characters for use with `make-random-char-array' and `make-random-string'.~%~@
Array contains the ASCII chars in the range 33,127~%~@
The latin-1 chars in the range 161,255~%~@
UTF-8 chars in the range 7680,7774~%~@
:EXAMPLE~%
 \(aref *random-chars* 0\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")


;;; ==============================
;;; :TEST-FUNCTION-DOCUMENTATION
;;; ==============================

(mon:fundoc 'make-random-char-array
"Return an array of length N containing random characters selected from `mon-test:*random-chars*'.~%~@
:EXAMPLE~%
 \(make-random-char-array 3\)~%
 \(make-random-char-array 8\)~%~@
:SEE-ALSO `mon-test:make-random-string'.~%►►►")

(mon:fundoc 'make-random-string
"Return a string of up to sixteen random characters from the value of `mon-test:*random-chars*'.~%~@
Optional arg STR-LEN is a positive integer value. When ommitted defaults to 16.~%~@
Strings generated as if by `mon-test:make-random-char-array' ~%~@
:EXAMPLE~%
 \(loop repeat 3 collect \(make-random-string\)\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(mon:fundoc 'make-random-inverted-number-array
"Return array of 320 randomly selected integers with a distribution inverted
over the byte size of the most-significant number in the following set:~%~@
 \(128 255\)   
 \(64  65535\)
 \(48  16777215\)
 \(32  4294967295\)
 \(24  281474976710655\)
 \(16  18446744073709551615\)
 \(8   340282366920938463463374607431768211455\)~%~@
IOW, for the range 0,255 select 128 integers at random for the range 0,65535
select 64 integers at random etc.~%~@
No effort is made to guarantee the returned array will not contain duplicated entries.
Return value is shuffled as if by `mon:nshuffle-vector'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon-test
;; End:

;;; ==============================
;;; EOF
