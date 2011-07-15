;;; :FILE mon-systems/hash.lisp
;;; ==============================

;;; ==============================
;; Example usage of `sb-ext:define-hash-table-test':
;;
;; (defun node-eql (x y)
;;   (eql x y))
;;
;; (defun hash-node-eql (x)
;;   (if (typep x 'node-base)
;;       (hash x)
;;       (sxhash x)))
;;
;; (define-hash-table-test node-eql hash-node-eql)
;;
;; (make-hash-table :test #'node-eql)
;;
;;
;; `alexandria:copy-hash-table', `alexandria:maphash-keys',
;; `alexandria:maphash-values', `alexandria:hash-table-keys',
;; `alexandria:hash-table-values', `alexandria:hash-table-alist',
;; `alexandria:hash-table-plist', `alexandria:plist-hash-table',
;; `alexandria:ensure-gethash',
;;
;;; ==============================



(in-package #:mon)
;; *package*

;; :SOURCE sbcl/src/code/early-extensions.lisp :WAS `positive-primep'
#+sbcl 
(defun prime-plusp (putative-prime)
  (unless (fixnump putative-prime)
    (simple-error-mon :w-sym 'prime-plusp
                      :w-type 'function
                      :w-spec "Arg PUTATIVE-PRIME must be `fixnump'"
                      :w-got putative-prime
                      :w-type-of t))
  (sb-int:positive-primep putative-prime))

#-sbcl
(defun prime-plusp (x)
  (declare (fixnum x))
  (if (<= x 5)
      (and (>= x 2) (/= x 4))
      (and (not (evenp x))
           (not (zerop (rem x 3)))
           (do ((q 6)
                (r 1)
                (inc 2 (logxor inc 6)) ;; 2,4,2,4...
                (d 5 (+ d inc)))
               ((or (= r 0) (> d q)) (/= r 0))
             (declare (fixnum inc))
             (multiple-value-setq (q r) (truncate x d))))))

;; :SOURCE sbcl/src/compiler/globaldb.lisp:WAS  `primify'
#+sbcl 
(defun prime-or-next-greatest (fixnum)
  (declare (type unsigned-byte-29  fixnum))
  (sb-c::primify fixnum))

#-sbcl
(defun prime-or-next-greatest (fixnum)
  (declare (type unsigned-byte-29 fixnum))
  (unless (fixnump 'fixnum)
    (simple-error-mon :w-sym 'prime-or-next-greatest
                      :w-type 'function
                      :w-spec "Arg fixnum must be `fixnump'"
                      :w-got fixnum
                      :w-type-of t))
  (do ((n (logior x 1) (+ n 2)))
      ((prime-plusp n) n)))

(defun hash-get-keys (hash-table)
  (declare (hash-table hash-table))
  ;; (with-collect (co)
  ;;     (with-hash-table-iterator (iter ht)
  ;;       (loop (multiple-value-bind (re kk) (iter)
  ;;               (unless re (return))
  ;;               (co kk))))))
  (loop :for kk :being :each :hash-key :of hash-table :collect kk))

;;; ==============================
;;; :COURTESY Rob Warnock C.L.L
;;; "I don't know why I find myself needing this so often, but I do..."
;;; Date: Sat, 05 May 2007 21:52:51 -0500
;;; Subject: Re: How Lisp's Nested Notation Limits The Language's Utility
;;; :SEE (URL `http://groups.google.com/group/comp.lang.lisp/msg/6e91e20f2f371b52')
;;; (defun hash-to-alist (hash-table &key (test (constantly t)))
;;;   (declare (hash-table hash-table))
;;;   (loop for key 
;;;      being each hash-key in hash-table 
;;;      using (hash-value value)
;;;      when (funcall test key value)
;;;      collect (cons key value)))
;;; ==============================
;; :COURTESY fare-utils/base/hash-tables.lisp
(defun hash-to-alist (hash-table &key (test (constantly t)))
  (declare (type hash-table hash-table))
  (loop 
     :for key :being :each :hash-keys :in hash-table 
     :using   (:hash-value value)
     :when    (funcall test key value)
     :collect (cons key value)))

;;; ==============================
;;; :COURTESY alexandria/hash-tables.lisp :WAS `alist-hash-table'
(defun hash-from-alist (alist &rest hash-table-initargs)
  (declare (list alist))
  (let* ((size (unless (memq :size hash-table-initargs) (list :size (length alist))))
         (table (apply #'make-hash-table (append size hash-table-initargs))))
    (dolist (cons alist)
      (setf (gethash (car cons) table) (cdr cons)))
    table))

;;; ==============================
;;; :SOURCE cllib/miscprint.lisp :WAS `alist->hash-table'
;; (defun alist-to-hash (alist)
;;   (declare (list alist))
;;   (let ((ht (make-hash-table :test (car alist))))
;;     (dolist (co (cdr alist) ht)
;;       (setf (gethash (car co) ht) (cdr co)))))
;;; ==============================
;;
;;; :SOURCE fare-utils/base/hash-tables.lisp
;; (defun alist-to-hash (alist &key (test #'eql))
;;   (declare (list alist))
;;   (loop
;;      :with h = (make-hash-table :test test :size (length alist))
;;      :for (k . v) :in alist
;;      :do (setf (gethash k h) v)
;;      :finally (return h)))
;;
;; (setf (documentation   'alist-to-hash 'function)
;;       #.(format nil
;;  "Return a new hash-table based on this alist.~%~@
;;  The inverse is `hash-to-alist'.~%~@
;; Keyword test is the a `hash-table-test'. Default is `eql'.~%~@
;; :EXAMPLE~%~@
;;  { ... <EXAMPLE> ... }~%~@
;; :SEE-ALSO `hash-to-alist', `plist-to-alist'.~%▶▶▶"))

;; hash-from-alist
;; hash-car

;; :COURTESY cllib/miscprint.lisp :WAS `pop-hash'
(defun hash-pop (object hash-table)
  (declare (type hash-table hash-table))
  (multiple-value-bind (value present-p) (gethash object hash-table)
    (when present-p (remhash object hash-table))
    (values value present-p)))

;; :COURTESY GBBopen/source/tools/tools.lisp :WAS `sorted-maphash'
(defun hash-map-sorted (function hash-table predicate &key key)
  (declare (type hash-table hash-table))
  (let ((function (coerce function 'function))
        (vector (make-array (hash-table-count hash-table)
                            :fill-pointer 0)))
    (declare (dynamic-extent vector))
    (flet ((push-entry (key value)
             (vector-push (cons key value) vector)))
      (declare (dynamic-extent #'push-entry))
      (maphash #'push-entry hash-table)
      (flet ((do-fn (cons)
               (funcall function (car cons) (cdr cons))))
        (declare (dynamic-extent #'do-fn))
        (map nil #'do-fn
             (if key 
                 (let ((key (coerce key 'function)))
                   (declare (function key))
                   (flet ((pred (a b)
                            (funcall (the function predicate) 
                                     (funcall key (car a))
                                     (funcall key (car b)))))
                     (declare (dynamic-extent #'pred))
                     (sort vector #'pred)))
                 (flet ((pred (a b)
                          (funcall (the function predicate) (car a) (car b))))
                   (declare (dynamic-extent #'pred))
                   (sort vector #'pred))))))))

;;; :COURTESY iosketch/prototypes.lisp :WAS `merge-hashes'
(defun hash-merge (hash-a hash-b)
  (declare (type hash-table hash-a hash-b))
  (prog1 hash-a
    (maphash #'(lambda (k v)
		 (setf (gethash k hash-a) v))
	     hash-b)))

;; :COURTESY GBBopen/source/tools/tools.lisp :WAS 
#+sbcl 
(defun hash-resize (hash-table new-size)
  (sb-thread::with-recursive-system-spinlock
      ((sb-impl::hash-table-spinlock hash-table) :without-gcing t)
    (when (> new-size (hash-table-size hash-table))
      (let ((old-rehash-size (hash-table-rehash-size hash-table))
	    (old-size (length (sb-impl::hash-table-next-vector hash-table))))
	;; SBCL's compiler (starting ~10.0.35) has problems compiling the
	;; (setf slot-value) with the UNWIND-PROTECT.  This FLET addresses
	;; that:
	(flet ((set-rehash-size (hash-table size)
		 (setf (slot-value hash-table 'sb-impl::rehash-size) size)))
	  (unwind-protect
	       (progn (set-rehash-size
		       hash-table 
		       ;; Compute the incremental value (to be added back to
		       ;; old-size in REHASH):
		       ;;
		       ;; :NOTE :WAS (-& new-size old-size))
		       ;; This was an inline type coercion around fixnum using the `defdn' macro.
		       ;; Lets just use `-' and assume that `-&' was an
		       ;; optimization that won't affect/impact thread
		       ;; spinlocking/concurrency
		       (- new-size old-size))
		      (sb-impl::rehash hash-table))
	    (set-rehash-size hash-table old-rehash-size))))
      't)))

;; (make-list size &key initial-element)
;; (function ((integer 0) &key (:initial-element t)) list)
(defun hash-mapcar (fn hash-table)
  (let ((accum nil))
    (labels ((mapfn (key val)
               (push (funcall fn key val) accum)))
      (declare (type hash-table hash-table))
      (maphash #'mapfn hash-table))
    accum))

;; :SOURCE epigraph/utilities.lisp :WAS `carhash' ;; hash-car
(defun hash-car (hash-table)
  (declare (type hash-table hash-table))
  (with-hash-table-iterator (next-entry hash-table)
    (next-entry)))

;;; ==============================
;; cllib/miscprint.lisp :WAS `make-ht-readtable'
;; (defun hash-make-readtable (&optional (rt (copy-readtable)))
;;   (set-dispatch-macro-character
;;    #\# #\h
;;    (lambda (st char arg)
;;      (declare (ignore char arg))
;;      (alist-to-hash (read st t nil t)))
;;    rt)
;;   rt)

;;; ==============================
;; cllib/miscprint.lisp :WAS `make-ht-readtable'
;;; beware that some lisps (e.g., CLISP and CMUCL) will not use this
;;; method for hash-tables.  it does work with Allegro though.
;;; since CLISP externalizes hash-tables on its own, only CMUCL is a problem.
;;; (unlock-package :common-lisp)
;; (defmethod print-object ((ht hash-table) (out stream))
;;   (if *print-readably*
;;       (format out "#h~s" (hash-to-alist ht))
;;       (call-next-method)))
;;; (restore-package-lock :common-lisp)
;;; ==============================

(defun hash-print (hash-table &optional stream)
  (declare (type hash-table hash-table))
  (let ((chk-strm (open-stream-output-stream-p stream
                                               :allow-booleans t
                                               :allow-fill-pointer-strings t
                                               :w-error t)))
    (maphash #'(lambda (key value)
                 (format (or chk-strm t) "~S ~S~%" key value))
             hash-table)))

;; :SOURCE :PASTE-NUMBER 118435: :PASTE-TITLE print-hash-table :PASTED-BY stassats  #lisp
(defun hash-pprint (hash-table)
  (declare (type hash-table hash-table))
  (let (previous)
    (pprint-logical-block (nil nil :prefix "(" :suffix ")")
      (maphash (lambda (key value)
                 (pprint-pop) ;; `pprint-exit-if-list-exhausted'
                 (if previous
                     (write-char #\Space)
                     (setf previous t))
                 (write key)
                 (write-char #\Space)
                 (write value)
                 (pprint-newline :fill))
               hash-table))))
;; (export '(hash-table-or-symbol))
;; (open-stream-output-stream-p nil)
;; (open-stream-output-stream-p nil :allow-booleans t :w-error t)
(defun hash-print-key-value-pairs (hash-table &optional stream)
  (declare (type hash-table hash-table))
  (let ((chkd (or (open-stream-output-stream-p stream 
                                               :allow-booleans t 
                                               :allow-fill-pointer-strings t
                                               :w-error t)
                  t)))
    (declare (stream-or-boolean chkd))
    (flet ((maphash-hpkvp (key val)
             (format chkd "key: ~S value: ~S~%" key val)))
      (maphash #'maphash-hpkvp hash-table))))

(defun hash-or-symbol-p (maybe-hash-table &key w-no-error)
  (when (null maybe-hash-table)
    (return-from hash-or-symbol-p (values nil 'NULL nil)))
  (or (and (hash-table-p maybe-hash-table)
           (return-from hash-or-symbol-p (values maybe-hash-table 'HASH-TABLE)))
      (and (symbolp maybe-hash-table)
           (boundp  maybe-hash-table)
           (ref-bind mht (symbol-value maybe-hash-table)
             (and (hash-table-p mht)
                  (return-from hash-or-symbol-p (values mht 'SYMBOL maybe-hash-table)))))
      (or (and w-no-error
               (return-from hash-or-symbol-p (values nil (type-of maybe-hash-table) maybe-hash-table)))
          (error 'type-error :datum maybe-hash-table :expected-type '(or hash-table symbol null)))))

(defun %hash-or-symbol-p-no-error (maybe-hash-table)
  (hash-or-symbol-p maybe-hash-table :w-no-error t))

(defun hash-or-symbol-ensured (maybe-hash-table &rest hash-args &key &allow-other-keys)
  (declare (hash-table-or-symbol maybe-hash-table))
  (or (and (booleanp maybe-hash-table)
           (apply #'make-hash-table :allow-other-keys t hash-args))
      (and (hash-or-symbol-p maybe-hash-table :w-no-error t))
      (or (and (not (boundp maybe-hash-table))
               (apply #'make-hash-table :allow-other-keys t hash-args))
          (setf (symbol-value maybe-hash-table)
                (apply #'make-hash-table :allow-other-keys t hash-args)))))

(defun hash-invert-key-val (src-hash &optional (dest-hash (make-hash-table 
                                                           :size (hash-table-size src-hash))))
  (declare (hash-table src-hash dest-hash))
  (maphash #'(lambda (dest-key src-key)
               (setf (gethash src-key dest-hash) dest-key))
           src-hash)
  dest-hash)

;; :COURTESY hexstream #lisp 2011-05-25
(defun hash-found-p (key hash-table &optional default)
  (declare (hash-table hash-table))
  (multiple-value-bind (value foundp) (gethash key hash-table default)
    (values 
     (if foundp (lambda () value) 
         default)
     foundp)))



;;; ==============================
;;; :HASH-DOCUMENATION
;;; ==============================

;; `alexandria:copy-hash-table', `alexandria:maphash-keys',
;; `alexandria:maphash-values', `alexandria:hash-table-keys',
;; `alexandria:hash-table-values', `alexandria:hash-table-alist',
;; `alexandria:hash-table-plist', `alexandria:plist-hash-table',
;; `alexandria:ensure-gethash',

(fundoc 'prime-plusp
"Is PUTATIVE-PRIME positive prime integer in range of `cl:most-positive-fixnum'?~%~@
:EXAMPLE~%
 \(prime-plusp 104723\)~%
 \(prime-plusp \(1+ most-positive-fixnum\)\)~%~@
:SEE-ALSO `mon:prime-or-next-greatest', `mon:number-power-of-two-ceiling'.~%▶▶▶")

(fundoc 'prime-or-next-greatest
"Given any non-negative integer, return it or its next nearest prime.~%~@
:EXAMPLE~%
 \(prime-or-next-greatest 104723\)~%
 \(prime-or-next-greatest 104724\)~%~@
:SEE-ALSO `mon:prime-plusp'.~%▶▶▶")

(fundoc 'hash-pop
  "Remove OBJECT from hash-table HT and return it.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:hash-map-sorted', `mon:hash-resize', `mon:hash-merge',
`mon:hash-get-keys', `mon:hash-car', `mon:hash-to-alist', `mon:hash-print',
`mon:hash-mapcar', `alexandria:copy-hash-table', `alexandria:maphash-keys',
`alexandria:maphash-values', `alexandria:hash-table-keys',
`alexandria:hash-table-values', `alexandria:hash-table-alist',
`alexandria:hash-table-plist', `alexandria:plist-hash-table',
`alexandria:ensure-gethash'.~%▶▶▶")

(fundoc 'hash-get-keys
 "Return the list of all the keys HASHTABLE.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `cl:with-hash-table-iterator', `hash-map-sorted', `hash-resize',
`hash-merge', `hash-pop', `hash-car', `hash-print', `hash-mapcar',
`alexandria:copy-hash-table', `alexandria:maphash-keys',
`alexandria:maphash-values', `alexandria:hash-table-keys',
`alexandria:hash-table-values', `alexandria:hash-table-alist',
`alexandria:hash-table-plist', `alexandria:plist-hash-table',
`alexandria:ensure-gethash'.~%▶▶▶")

(fundoc 'hash-merge
"Merge HASH-A with key/value pairs from HASH-B, return HASH-A.~%~@
Set or add all key/value pairs of HASH-B in HASH-A.~%~@
If HASH-A has key/value pair key-1/bubba and hash-b hash key/value pair
key-1/other-bubba on return the value at key-1 in HASH-A will be
other-bubba.~%~@
:EXAMPLE~%~@
  { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:hash-map-sorted', `mon:hash-resize', `mon:hash-pop',
`mon:hash-get-keys', `hash-car', `mon:hash-to-alist', `mon:hash-print',
`mon:hash-mapcar', `alexandria:copy-hash-table', `alexandria:maphash-keys',
`alexandria:maphash-values', `alexandria:hash-table-keys',
`alexandria:hash-table-values', `alexandria:hash-table-alist',
`alexandria:hash-table-plist', `alexandria:plist-hash-table',
`alexandria:ensure-gethash'.~%▶▶▶")

(fundoc 'hash-invert-key-val
"Return a new hash-table with values of src-hash as keys of new hash.~%~@
When optional arg DEST-HASH is non-nil it should satisfy `cl:hash-table' and
values of SRC-HASH are added as keys of DEST-HASH.~%~@
:NOTE No effort is made to verify if the `cl:hash-table-test' of SRC-HASH is
congruent with that of DEST-HASH.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'hash-to-alist
"Return the key/value pairs of HASH-TABLE as an alist of consed pairs.~%~@
Keyword :TEST is a `cl:hash-table-test'. Default is `cl:constantly' t.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `hash-map-sorted', `hash-resize', `hash-merge', `hash-pop',
`hash-get-keys', `hash-car'.~%▶▶▶")

(fundoc 'hash-from-alist
  "Returns a hash-table containing key/value pairs of association list ALIST.~%~@
Hash-table is initialized using HASH-TABLE-INITARGS.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:hash-to-alist'.~%▶▶▶")

(fundoc 'hash-map-sorted
"Map HASH-TABLE with FUNCTION sorting by PREDICATE, return value is a vector.~%~@
FUNCTION is a function which accepting a consed pair as its argument.~%~@
PREDICATE is a function accepting two args as used by `cl:sort' to order return value.~%~@
Keyword KEY, when non-nil, is a function accepting two args as used by `cl:sort' to
order return value. When KEY is non-nil arg PREDICATE is ignored.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... }~%~@
:SEE-ALSO `with-hash-table-iterator', `hash-resize', `hash-merge', `hash-pop',
`hash-get-keys', `hash-car', `hash-to-alist', `hash-print', `hash-mapcar'.~%▶▶▶")

(fundoc 'hash-car
"Return first hash key/value pair in HASH-TABLE.~%~@
Return value is as if by `cl:with-hash-table-iterator'
:EXAMPLE~%
 \(let \(\(tt-hsh \(make-hash-table\)\)\)
   \(loop
      :for k in '\(bubba0 bubba1 bubba2\)
      :for v in '\(\"val0\" \"val2\" \"val2\"\)
      :do \(setf \(gethash k tt-hsh\) v\)\)
   :finally \(multiple-value-bind \(t/nil kk vv\) \(hash-car tt-hsh\) \(and t/nil `\(,kk ,vv\)\)\)\)~%~@
:SEE-ALSO `hash-map-sorted', `hash-resize', `hash-merge', `hash-pop',
`hash-get-keys', `hash-car', `hash-to-alist', `hash-print',
`hash-mapcar'.~%▶▶▶")

(fundoc 'hash-mapcar
  "Like `cl:maphash' except it accumulates the result in a list as if by mapcar.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `hash-map-sorted', `hash-resize', `hash-merge', `hash-pop',
`hash-get-keys', `hash-car', `hash-to-alist', `hash-print'.~%▶▶▶")

(fundoc 'hash-print
"Print hash-table as if by `cl:format'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:hash-pprint', `cl:print-unreadable-object'.~%▶▶▶")

(fundoc 'hash-pprint
"Print HASH-TABLE as if by `cl:pprint'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:hash-print', `cl:pprint-newline', `cl:pprint-pop',
`cl:pprint-logical-block', `cl:print-unreadable-object', `cl:*print-length*', `cl:*print-circle*'.~%▶▶▶")

(fundoc 'hash-print-key-value-pairs
        "Print key/value pairs of HASH-TABLE to STREAM as if by `cl:format'.~%~@
STREAM is a boolean or a destination stream object satisfying `cl:output-stream-p'.
Mapped key/values are returned one pair per line with the form:~%
 key: ~~S value ~~A~%~@
:EXAMPLE~%
 \(let \(\(ht \(make-hash-table\)\)\)
   \(dotimes \(i 8 \(with-output-to-string \(ht-str\)
                   \(hash-print-key-value-pairs ht ht-str\)\)\)
     \(setf \(gethash i ht\) \(format nil \"~~D\" 8\)\)\)\)~%
 \(let \(\(ht \(make-hash-table\)\)\)  
   \(dotimes \(i 8 \(setf ht \(with-output-to-string \(ht-str\)
                            \(hash-print-key-value-pairs ht ht-str\)\)\)\)
     \(setf \(gethash i ht\) \(format nil \"~~D\" i\)\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

;; (setf (documentation   'hash-make-readtable 'function)
;;       #.(format nil
;; "Make a readtable which will be able to read hash-tables with #h\(\).~%~@
;; :EXAMPLE~%~%~@
;; :SEE-ALSO `<XREF>'.~%▶▶▶"))


(fundoc 'hash-or-symbol-p
"Whether MAYBE-HASH-TABLE is `cl:hash-table-p' or a symbol with `cl:symbol-value' evaluating to one.~%~@
Return as if by `cl:values'.
When MAYBE-HASH-TABLE is a `cl:hash-table-p' return:~%
 \(the hash-table MAYBE-HASH-TABLE\),HASH-TABLE~%~@
When MAYBE-HASH-TABLE is a symbol evaluating to a hash-table return:~%
 \(the hash-table \(symbol-value MAYBE-HASH-TABLE\)\),SYMBOL,MAYBE-HASH-TABLE~%~@
When MAYBE-HASH-TABLE is null return:~%
 nil,NULL
When MAYBE-HASH-TABLE is none of the above and keyword W-NO-ERROR is non-nil return:~%
 MAYBE-HASH-TABLE, \(type-of MAYBE-HASH-TABLE\)~%~@
When keyword W-NO-ERROR is ommited and MAYBE-HASH-TABLE is none of the above
signal a `cl:type-error'.~%~@
:EXAMPLE~%
 \(hash-or-symbol-p *default-class-documentation-table*\)~%
 \(hash-or-symbol-p '*default-class-documentation-table*\)~%
 \(hash-or-symbol-p \(make-hash-table\)\)~%
 \(hash-or-symbol-p nil\)~%
 \(hash-or-symbol-p t :w-no-error t\)~%
 \(hash-or-symbol-p \(list 8\) :w-no-error t\)~%
;; Following both signal `cl:type-error's:~%
 \(hash-or-symbol-p t\)~%
 \(hash-or-symbol-p \(list 8\)\)~%~@
:SEE-ALSO `hash-or-symbol-ensured', `mon:hash-table-or-symbol',
`mon:hash-table-or-symbol-with-hash'.~%▶▶▶")

(fundoc 'hash-or-symbol-ensured
"If MAYBE-HASH-TABLE is `cl:hash-table-p' return it.~%~@
If MAYBE-HASH-TABLE is a symbol evaluating to a hash-table object return it.~%~@
If MAYBE-HASH-TABLE is a symbol but not a hash-table object set its `cl:symbol-value' to a
newly generated hash-table object and return it.~%~@
If MAYBE-HASH-TABLE is NULL or T return a newly generated hash-table object.~%~@
:EXAMPLE~%
 \(hash-or-symbol-ensured nil\)~%
 \(hash-or-symbol-ensured \(gensym\)\)~%
 \(let \(\(Q 'nil\)\)
  \(progn \(hash-or-symbol-ensured Q\) Q\)\)~%
 \(defparameter *tt--hash* nil\)~%
 \(progn \(hash-or-symbol-ensured *tt--hash*\) *tt--hash*\)~%
 \(progn \(hash-or-symbol-ensured '*tt--hash*\) *tt--hash*\)~%
 \(setf *tt--hash* nil\)~%
 \(progn 
   \(hash-or-symbol-ensured '*tt--hash*
                           :rehash-size 1.1
                           :test 'equal\)
   *tt--hash*\)~%~@
 \(prog1 
     \(progn 
       \(setf *tt--hash* \(make-array 3\)\)
       \(hash-or-symbol-ensured '*tt--hash*
                               :rehash-size 1.1
                               :test 'equal\)
       *tt--hash*\)
   \(unintern '*tt--hash*\)\)~%~@
:SEE-ALSO `hash-or-symbol-p', `mon:hash-table-or-symbol',
`mon:hash-table-or-symbol-with-hash'.~%▶▶▶")

#+sbcl 
(setf (documentation 'hash-resize 'function)
      #.(format nil
"Resize \(grow\) a hash-table per NEW-SIZE.~%~@
As with SIZE argument to `cl:make-hash-table', NEW-SIZE's actual value may
be larger than value supplied.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `hash-map-sorted', `hash-merge', `hash-pop', `hash-get-keys',
`hash-car', `hash-to-alist', `hash-print', `hash-mapcar'.~%▶▶▶"))

(fundoc 'hash-found-p
"Like `cl:gethash' but `cl:nth-value' 0 is a closure if KEY was found.~%~@
:EXAMPLE~%
 \(let \(\(tt-ht \(make-hash-table :test 'equal\)\)\)
   \(setf \(gethash \"bubba\" tt-ht\) \"bubba\"\)
   \(values \(hash-found-p \"bubba\" tt-ht\)
           \(hash-found-p \"no-bubba\" tt-ht \"can't find bubba\"\)\)\)~%
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
