;;; :FILE mon-systems/seqs.lisp
;;; ==============================

;;; ==============================
;; :TODO :EMACS-COMPAT `add-to-ordered-list', `assoc-default'
;;
;; :NOTE sb-impl::LAST-CONS-OF
;; (sb-int::singleton-p lst))
;; (sb-int::posq item lst))
;; (sb-int::neq obj-x obj-y))
;; (sb-int::memq elt list))
;; (sb-int::nth-but-with-sane-arg-order lst idx))
;; (sb-int::delq elt list))
;; (sb-impl::last-cons-of in-list))
;;
;;
;; #:string-listify       ;; sb-impl::string-listify 
;; (sb-impl::string-listify (list #\s #\t #\r #\i #\n #\g))
;; #:symbol-listify       ;; sb-impl::symbol-listify
;;
;;; ==============================


(in-package #:mon)
;; *package*



;; file-local-variables-alist
;; slime-mode-hook
;; (cdr (assq 'show-trailing-whitespace file-local-variables-alist))
;;; ==============================
;;; :SEQ-PREDICATE-PREDICATE-LIKE
;;; ==============================

;;; :SOURCE D. Mcdermott ytools/base.lisp :SEE-ALSO Mark Kantrowitz' xref.lisp
(declaim (inline car-eq))
(defun car-eq (lst-a obj-b) 
  (and (consp lst-a)
       (eq (car lst-a) obj-b)))

#-sbcl
(defun position-eq (item list)
  (do ((i list (cdr i))
       (j 0 (1+ j)))
      ((null i))
    (when (eq (car i) item)
      (return j))))

#+sbcl 
(defun position-eq (item lst)
  (sb-int::posq item lst))

;;; ==============================
;; :PASTED-BY pkhuong :DATE 2011-04-13
;; :PASTE-URL (URL `http://paste.lisp.org/+2LRM/1')
(defun positions (item sequence)
  ;; (positions 4 '(1 2 3 4 4 4 9 2 4 4 1)) ;=> (3 4 5 8 9)
  (let ((index 0)
        (positions '()))
    (map nil (lambda (x)
               (when (eql x item)
                 (push index positions))
               (incf index))
         sequence)
    (nreverse positions)))

;;; ==============================

;;; :SOURCE Sam Steingold :HIS /clocc/src/cllib/elisp.lisp
#-sbcl (defun memq (elt list) 
	 (declare (list list))
	 (member elt list :test #'eq))

#-sbcl (declaim (inline not-eq))
#-sbcl (defun not-eq (obj-x obj-y) (not (eq obj-x opj-y)))

#+sbcl 
(defun not-eq (obj-x obj-y)
  (sb-int::neq obj-x obj-y))

(declaim (inline %ensure-both-cars))
(defun %ensure-both-cars (cons-a cons-b)
  (declare (optimize (speed 3)))
  (and
   (or (and (consp cons-a) (consp cons-b))
       (simple-error-mon  :w-sym '%ensure-both-cars
                          :w-type 'function
                          :w-spec (list "Arg cons-a or cons-b not `cl:consp'~%"
                                        "Arg cons-a~14Tgot: ~S~%arg cons-a type-of: ~S~%"
                                        "arg cons-b~14Tgot: ~S~%arg cons-b type-of: ~S~%")
                          :w-args `(,cons-a ,(type-of cons-a) ,cons-b ,(type-of cons-b))))
   (or (and 
        (realp (car (the cons cons-a)))
        (realp (car (the cons cons-b))))
       (simple-error-mon  :w-sym '%ensure-both-cars
                          :w-type 'function
                          :w-spec (list "Arg cons-a or cons-b not `cl:realp'~%"
                                        "arg cons-a~14Tgot: ~S~%arg cons-a type-of: ~S~%"
                                        "arg cons-b ~14Tgot: ~S~%arg cons-b type-of: ~S~%")
                          :w-args `(,cons-a ,(type-of cons-a) ,cons-b ,(type-of cons-b))))
   (the list (list (the cons cons-a) (the cons cons-b)))))

;; (caar (%ensure-both-cars '(8 . 9) '(11 12 13)))
;; (caadr (%ensure-both-cars '(8 . 9) '(11 . 12)))

(defun car-less-than-car (a b)
  (declare (inline %ensure-both-cars)
           (optimize (speed 3)))
  (let* ((chk-cars (%ensure-both-cars a b))
         (chk-a (the real (caar (the cons chk-cars))))
         (chk-b (the real (caadr (the cons chk-cars)))))
    (declare (type real chk-a chk-b))
    (< chk-a chk-b)))

(defun car-greater-than-car (a b)
  (declare (inline %ensure-both-cars)
           (optimize (speed 3)))
  (let* ((chk-cars (%ensure-both-cars a b))
         (chk-a (the real (caar (the cons chk-cars))))
         (chk-b (the real (caadr (the cons chk-cars)))))
    (declare (type real chk-a chk-b))
    (< chk-a chk-b)))

;;; ==============================
;;; :SOURCE sbcl/src/code/late-extensions.lisp :WAS `list-with-length-p'
;;; Is X a list for which LENGTH is meaningful, i.e. a list which is
;;; not improper and which is not circular?
;;; (defun list-length-n-p (x)
;;;   (values (ignore-errors (list-length x))))
;;; ==============================
;; :SOURCE asdf.lisp :WAS `length=n-p' 
;; :NOTE See `cl:list-length' and `cl:endp'
(defun list-length-n-p (cnt-list int) 
  ;; :WAS (check-type n (integer 0 *))
  (unless (list-proper-p cnt-list)
    (typecase cnt-list
      (circular-list (circular-list-error cnt-list
                                          :w-sym 'list-length-n-p 
                                          :w-type 'function
                                          :signal-or-only nil))
      (t (proper-list-error
          :w-sym 'list-length-n-p
          :w-type 'function
          :error-args `(cnt-list ,cnt-list)
          :signal-or-only nil))))
  (unless (typep int 'fixnum-0-or-over)
    (error (make-condition 'type-error 
                           :datum int
                           :expected-type 'fixnum-0-or-over)))
  (let nil
    (declare (type proper-list cnt-list)
             (type fixnum-0-or-over int))
    (loop
       :for l = (the proper-list cnt-list) :then (cdr l)
       :for i :downfrom (the fixnum-0-or-over int) 
       :do (cond
             ((zerop i) (return (null l)))
             ((not (consp l)) (return nil))))))


;;; ==============================
;;; :SEQ-SETS
;;; ==============================

;; #-sbcl
;; (defun memq (item list)
;;   (do ((i list (cdr i)))
;;       ((null i))
;;     (when (eq (car i) item)
;;       (return i))))
;;
;; #+sbcl
;; (defun memq (elt list) 
;;   (declare (type proper-list list))
;;   (sb-int::memq elt list))
;;
;; #+sbcl
;; (define-compiler-macro memq (elt list)
;;   `(member ,elt (the proper-list ,list) :test #'eq))
;;
;; (member 'a '(a b . b))
;; (memq 'a '(a b . b))
;; (listp '(a b . b))
(defun memq (elt list)
  (declare (type list list))
  (labels ((lcl-memq (itm lst)
             (do ((i lst (cdr i)))
                 ((null i))
               (when (eq (car i) itm)
                 (return i)))))
    (let* ((l-d-p-d (multiple-value-list (list-dotted-p-destructure list)))
           (chk-it  (cond ((car l-d-p-d)
                           (if (eq (car l-d-p-d) elt)
                               (return-from memq  (list (car l-d-p-d)))
                               (cadr l-d-p-d)))
                          ((null (car l-d-p-d))
                           (if (list-proper-p (cadr l-d-p-d))
                               (cadr l-d-p-d)
                               (proper-list-error :w-sym  'memq
                                                  :w-type 'function
                                                  :error-args `(list ,(cadr l-d-p-d))
                                                  :signal-or-only nil))))))
      (declare (type list chk-it))
      (lcl-memq elt chk-it))))

;;; :SOURCE freedius/lisp/system-tool/system-tool.lisp :WAS `union-eq-preserve-first'
(defun union-eq-keep-first (&rest lists)
  (let ((gthr '())
        (ht (make-hash-table)))
    (declare (type list gthr)
             (type hash-table ht))
    (loop 
       :for list :in lists
       :do (loop 
              :for inner :in list
              :unless (gethash inner ht)
              :do (setf (gethash inner ht) t)
              (push inner gthr)))
    (nreverse gthr)))


;;; ==============================
;;; :SEQ-ACCESSORS
;;; ==============================

;; :NOTE 
;; (nth 3 '(a b c)) => NIL 
;; (elt '(a b c) 3) => error 
;; (setf (nth 3 '(a b c)) 'q) => error

;;; :SOURCE D. Mcdermott ytools/nilscompat.lisp
(declaim (inline list-elt))
(defun list-elt (lst idx) 
  (declare (type list lst) (type index idx))
  ;; (elt lst index idx)
  (nth idx lst))
;;
(defsetf list-elt (lst idx)
    (x)
  `(setf (nth (the index ,idx) (the list ,lst)) ,x))

;;; :SOURCE D. Mcdermott ytools/base.lisp
(declaim (inline last-elt))
(defun last-elt (lst) 
  (car (last lst)))

(declaim (inline list-last))
(defalias 'list-last 'last-elt)

#+sbcl
(defun last-cons (in-list)
  (sb-impl::last-cons-of in-list))

#+sbcl
(defun nth-sane (lst idx)
  (sb-int::nth-but-with-sane-arg-order lst idx))

;; :SOURCE xit/cl-utilities/cl-utilities.lisp :WAS `single-to-list'
(declaim (inline list-from-singleton))
(defun list-from-singleton (arg)
  (if (listp arg)
      arg
    (list arg)))

;;; :SOURCE xit/cl-utilities/cl-utilities.lisp :WAS `single-from-list'
(declaim (inline list-get-singleton))
(defun list-get-singleton (arg)
  (if (and (consp arg) (not (cdr arg)))
      (car arg)
    arg))

(declaim (inline car-safe))
(defun car-safe (object)
  (and (consp object) (car object)))

(declaim (inline cdr-safe))
(defun cdr-safe (object)
  (and (consp object) (cdr object)))

;;; ==============================
;;; :COURTESY Kaz Kylheku comp.lang.lisp
;;; :DATE 2008-11-20 :SUBJECT Re: Detection of dotted list?
;;; :SOURCE (URL `http://groups.google.com/group/comp.lang.lisp/msg/0977b44e2331bb7e')
;;; :MODIFICATIONS
(defun list-dotted-p-destructure (object)  
  (multiple-value-bind (is-dot is-type) (list-dotted-p object)
    (case is-type
      ;; (circular-list (values is-dot is-type))
      (circular-list (circular-list-error object
                                          :w-sym  'list-dotted-p-destructure 
                                          :w-type 'function
                                          :signal-or-only nil))
      (null          (values is-dot is-dot))
      ((eql nil)     (values is-dot object))
      (proper-list   (values nil object))
      (dotted-list   (loop 
                        :with terminator
                        :for tail :on object
                        :collecting (car tail) :into new-list
                        :when (atom (cdr tail))
                         :do (setf terminator (cdr tail))
                        :finally (return (values terminator
                                                 (if terminator new-list object))))))))



;; :SOURCE PJB common-lisp/cesarum/list.lisp :WAS `dotted-list-length' :LICENSE GPL
(defun list-dotted-length (dotted-list)
  (declare (list dotted-list))
  (loop
     :for length :from 0
     :for current = dotted-list :then (cdr current)
     :until (atom current)
     :finally (return length)))

;; :SOURCE PJB common-lisp/cesarum/list.lisp :WAS `circular-list-lengths' :LICENSE GPL
(defun list-circular-lengths (circular-list)
  (declare (list circular-list))
  (let ((cells (make-hash-table)))
    (declare (hash-table cells))
    (loop
       :for index :from 0
       :for cell = circular-list :then (cdr cell)
       :for previous = (gethash cell cells)
       :do (if previous
               (return-from list-circular-lengths (values previous (- index previous)))
               (setf (gethash cell cells) index)))))

;; :SOURCE PJB common-lisp/cesarum/list.lisp :WAS `list-lengths' :LICENSE GPL
;; :NOTE Has test in mon-test/testing.lisp 
(defun list-lengths (list)
  (declare ((or atom list) list))
  (labels ((proper (current slow)
             ;; (print (list 'proper current slow))
             (cond ((null current)       (values (list-length        list) 0))
                   ((atom current)       (values (list-dotted-length list) nil))
                   ((null (cdr current)) (values (list-length        list) 0))
                   ((atom (cdr current)) (values (list-dotted-length list) nil))
                   ((eq current slow)    (list-circular-lengths list))
                   (t                    (proper (cddr current) (cdr slow))))))
    (typecase list
      (cons  (proper list (cons nil list)))
      (null  (values 0 0))
      ;; :WAS (t     (values 0 nil))
      (t     (values nil (type-of list))))))



;;; ==============================
;;; :SEQ-DESTURCTIVE
;;; ==============================

#-sbcl (defun delq (elt list) 
	 (declare (list list))
	 (delete elt list :test #'eq))

#+sbcl
(defun delq (elt list) 
  (declare (type list list))
  (sb-int::delq elt list))

#+sbcl
(define-compiler-macro delq (elt list)
  `(delete ,elt (the list ,list) :test #'eq))

;;; :SOURCE emacs/lisp/subr.el
(defun delete-dups (list)
  (declare (type list list))
  (delete-duplicates list :test #'equal))

;;; :SOURCE GBBopen/source/tools/tools.lisp
(defun delq-one (item list)
  (declare (type list list))
  ;; (with-full-optimization ()
  (cond
     ;; Deleting the first element:
     ((eq item (first list))
      (rest list))
     (t (let ((ptr list)
              next-ptr)
          (declare (list ptr next-ptr))
          (loop
            (unless (consp (setf next-ptr (cdr ptr)))
              (return list))
            (when (eq item (car next-ptr))
              (setf (cdr ptr) (cdr next-ptr))
              (return-from delq-one list))
            (setf ptr next-ptr))))))

(defun delete-all-elts-eq (in-list from-list)
  (declare (type list in-list from-list))
  (delete-if #'(lambda (element) 
		 (member element in-list :test #'eq))
	     from-list))

;;; :SOURCE GBBopen/source/tools/tools.lisp :WAS Counted-delete
(defun delete-w-count (item seq &rest args &key (test #'eql) 
		       (test-not nil test-not-supplied-p)
                       &allow-other-keys)
  (declare (dynamic-extent args))
  ;; no need to check for both test and test-not, delete should do it for us
  ;; (but doesn't in most implementations...):
  (let ((items-deleted 0)
        (test (if test-not 
                  (coerce test-not 'function) 
                  (coerce test 'function))))
    (declare (type function test))
    (flet ((new-test (a b)
             (when (funcall test a b)
               (incf (the fixnum items-deleted)))))
      (declare (dynamic-extent #'new-test))
      (values (apply #'delete item seq 
                     (if test-not-supplied-p ':test-not ':test)
                     #'new-test 
                     args)
              items-deleted))))

;;; :SOURCE emacs/lisp/subr.el
(declaim (inline remq))
(defun remq (elt list)
  (declare (type list list))
  (if (memq elt list) ;;(member elt list :test #'eq)
      ;; :WAS (delete elt (copy-seq list) :test #'eq) ;;(copy-sequence list)
      (delq elt (copy-seq list))
      list))

;;; :SOURCE ltk-0.91/ltk-mw.lisp :WAS `remove-nth'
(defun nth-remove (n list)
  ;; (declare ((integer 0 *) n))
  (declare (type index n)) ;; <- (1- array-dimension-limit)
  (concatenate 'list (subseq list 0 n) (subseq list (1+ n))))

;;; :SOURCE cllib/simple.lisp
(defun nsublist (lst &optional pos0 pos1)
  (declare (type list lst))
  (when pos1 
    (let ((cut (nthcdr pos1 lst)))
      (when cut (setf (cdr cut) nil))))
  (if pos0 (nthcdr pos0 lst) lst))

(declaim (inline setcar))
(defun setcar (cell newcar) 
  ;;(setf (car cell) newcar)
  (rplaca cell newcar))

(declaim (inline setcdr))
(defun setcdr (cell newcdr) 
  ;;(setf (cdr cell) newcdr))
  (rplacd cell newcdr))

(defun add-to-list (list elt)
  (unless (boundp list) (setf (symbol-value list) nil))
  (pushnew elt (symbol-value list) :test #'equal))

;;; :SOURCE sbcl/src/compiler/assem.lisp `add-to-nth-list'
;;; (slime-describe-symbol "error")
(defun add-to-nth-list (list thing n)
  (declare (type list list)
           (type index n)
           (optimize (speed 3)))
  (do ((cell (or list (setf list (list nil)))
             (or (cdr cell) 
                 ;; :WAS (setf (cdr cell) (list nil))
                 (rplacd cell (list nil)) ))
       ;; :WAS (i n (1- i)))
       (i n (1- (the index-or-minus-1 i))))
      ((zerop i)
       (push thing (car cell))
       list)))

;;; :SOURCE D. Mcdermott ytools/base.lisp :WAS `take'
(defun list-take (take-n from-lst)
  (declare 
   ;;(type fixnum take-n)
   (type list from-lst))
  (cond ((< take-n 0)
	 (let ((g (length from-lst)))
	   (subseq from-lst (+ g take-n) g)))
	(t (subseq from-lst 0 take-n))))

;;; :SOURCE D. Mcdermott ytools/base.lisp :WAS `drop'
(defun list-drop (drop-n from-lst)
  (declare (type index drop-n)
	   (type list from-lst))
  (cond ((< drop-n 0) 
	 (subseq from-lst 0 (+ (length from-lst) drop-n)))
	(t (subseq from-lst drop-n (length from-lst)))))

;;; ==============================
;; :PASTE-NUMBER  123401
;; :PASTE-BY      rswarbrick
;; :PASTE-URL     (URL `http://paste.lisp.org/+2N7T')
;; :PASTE-DATE    2011-07-21
;; :PASTE-CHANNEL #lisp
;; :WAS `each-n-tuple'
(defun list-n-tuples (w-fun n-tuples in-list)
  (declare
   (index-from-1 n-tuples)
   (list in-list)
   (optimize (speed 3)))
  (do ((rest in-list (nthcdr n-tuples rest)))
      ((null rest) (values))
    (funcall w-fun
             (subseq (the list rest) 0 
                     ;; (min n-tuples  (length (the list rest)))))))
                     ;; NOTE list-length is likely to return wacko if rest is ever circular.
                     (min n-tuples  (list-length rest))))))

(defun list-slice (n-tuples in-list)
  (declare 
   (index-from-1 n-tuples)
   (list in-list)
   (optimize (speed 3)))
  (let ((gthr '()))
    (flet ((mk-slice (sublist)
             (declare (list sublist gthr))
             (push sublist gthr)))
      (list-n-tuples #'mk-slice n-tuples in-list))
    (setf gthr (nreverse gthr))))


;;; ==============================
;;; :SEQ-COLLECT
;;; ==============================
(declaim (inline copy-seq))
(defun copy-sequence (seq)
  (declare (type sequence seq))
  (copy-seq seq))

;;; :SOURCE D. Mcdermott ytools/nilscompat.lisp
(declaim (inline adjoinq))
(defun adjoinq (item lst);; &key key) 
  (declare (type list lst))
  ;; (adjoin item lst :test #'eq :key key))
  (adjoin item lst :test #'eq))

;;; :SOURCE freedius/lisp/lisp/custom.lisp :WAS `quote-list-elements'
(defun list-quote-elts (lst)
  (declare (type list lst))
  ;; (loop for x in (the list lst) collect `',x))
  (loop for x in lst collect `',x))

;;; :SOURCE clocc/src/onlisp-util.lisp :WAS `shuffle'
(defun interleave (lst-a lst-b)
  (declare ((or cons list atom) lst-a lst-b))
  (cond ((null lst-a) lst-b)
        ((null lst-b) lst-a)
        (t (list* (car lst-a) (car lst-b)
                  (interleave (cdr lst-a) (cdr lst-b))))))

;;; ==============================
;;; :SOURCE clocc/src/simple.lisp
;; (defun flatten (lst-of-lsts)
;;  "atom -> (atom); (1 (2) (3 (4) (5 (6) 7) 8) 9) -> (1 2 3 4 5 6 7 8 9)"
;;   (labels ((fl (lst-of-lsts acc) 
;;              (cond ((null lst-of-lsts lst-of-lsts) acc)
;;                    ((atom lst-of-lsts) (cons lst-of-lsts acc))
;;                    (t (fl (car lst-of-lsts) (fl (cdr lst-of-lsts) acc))))))
;;     (fl lst-of-lsts nil)))
;;
;;; :SOURCE alexandria/lists.lisp
(defun flatten (tree)
  (let ((list '()))
    (labels ((traverse (subtree)
               (when subtree
                 (if (consp subtree)
                     (progn
                       (traverse (car subtree))
                       (traverse (cdr subtree)))
                     (push subtree list)))))
      (traverse tree))
    (nreverse list)))

;;; :WAS `transpose-lists'
(defun list-transpose (lists)
  (cond ((null lists) '())
        ((some #'null lists) '())
        (t (cons (mapcar #'car lists)
                 (list-transpose (mapcar #'cdr lists))))))

(defun list-subsets (set)
  (let ((first (first set)) (rest (rest set)))
    (if rest
        (let ((others (list-subsets rest)))
          (nconc others
                 (mapcar (lambda (subset)
                           (cons first subset))
                         others)))
        (list nil (list first)))))

;;; :SOURCE cllib/matrix.lisp
(defun list-to-array (list dims)
  (declare (type list list))
  (let* ((arr (make-array dims)) 
         (sz (array-total-size arr)))
    (unless (= (length list) sz)
      (simple-error-mon :w-sym 'list-to-array
                        :w-type 'function
                        :w-spec "list/dimension mismatch for list:~% ~S~%~
                                   list-length: ~:D~%~
                                   array-size: ~:D~%~
                                   got-dimension: ~S"
                        :w-args `(,list ,(length list) ,sz  ,dims)))
    (loop :for el :in list :for i :upfrom 0
       :do (setf (row-major-aref arr i) el))
    arr))

;; (defparameter *tt--array* (make-array '(2 3) :initial-contents '((a b c) (1 2 3))))
;; *tt--array* ;=> #2A((A B C) (1 2 3))
;;   
;; #2A((A B C)  ;; A is at rank 0 idx 0 => (aref *tt--array* 0 0)
;;     (1 2 3)) ;; 2 is at rank 1 idx 1 => (aref *tt--array* 1 2)

;; (row-major-aref *tt--array* 4) ;=> 2 
;; #2A((a    ;; 0 
;;      b    ;; 1
;;      c)   ;; 2
;;     (1    ;; 3
;;      2    ;; 4
;;      3))  ;; 5

;;; :SOURCE clocc/src/list.lisp
(defun freqs (seq &key (test #'eql) (key #'identity))
  (declare (sequence seq) 
	   (type (function (t t) t) test)
           (type (function (t) t) key)
           (optimize (speed 3)))
  #-sbcl (assert (sequencep seq))
  (unless (sequence-zerop seq)
    (sort
     (reduce (lambda (res el)
               (let ((fi (assoc el res :test test)))
                 (cond (fi (incf (cdr fi)) res) ((acons el 1 res)))))
             seq :key key :initial-value nil)
     #'> :key #'cdr)))

;;; ==============================
;;; :NEWSGROUP comp.lang.lisp
;;; :FROM                      Wade Humeniuk <whume...@telus.net.no.spam>
;;; :DATE                      Wed, 09 May 2007 13:27:40 GMT
;;; :SUBJECT                   Re: How do I make this utility more flexible without losing speed?
;;; (URL `http://blog.moertel.com/articles/2007/09/01/clusterby-a-handy-little-function-for-the-toolbox')
(defun group-by-w-hash (list test key)
  (declare (list list))
  (let ((hash-table (make-hash-table :test test)))
    (declare (hash-table hash-table))
    (dolist (el list)
      (push el (gethash (funcall key el) hash-table)))
    (loop
       for val being the hash-value in hash-table
       collect val into vals
       finally (return vals))))

(defun group-by-w-seq (list test key)
  ;; (declare (list list))
  (let ((groups '()))
    (dolist (elt list groups)
      (let ((pos (position (funcall key elt) groups :test
			   (lambda (e group)
			     (funcall test e
				      (funcall key (car group)))))))
        (if pos
	    (push elt (nth pos groups))
	    (push (list elt) groups))))))

;; :REQUIRES `standard-test-function-p' introspect.lisp
(defun list-group-by (list &key (test #'eql) (key #'identity))
  (if (standard-test-function-p test)
      (group-by-w-hash list test key)
      (group-by-w-seq  list test key)))


;; :SOURCE (URL `http://paste.lisp.org/+2K4L')  
;; :WAS `count-subsequence-occurance'
;; Stas Boukarev's initial version using reduce/search:
;; (defun subseq-count-2 (subsequence sequence)
;;   (cdr (reduce (lambda (x y &aux (search (search subsequence sequence :start2 (car x))))
;;                  (declare (ignore y))
;;                  (if search
;;                      (setf (car x) (1+ search)
;;                            (cdr x) (1+ (cdr x)))
;;                      (return-from subseq-count-2 (cdr x))))
;;                sequence
;;                :initial-value (cons 0 0))))
;;
;; :SOURCE (URL `http://paste.lisp.org/+2K4L/1')
;; Prxq's version using do/search
(defun subseq-count (subsequence sequence)
  (if (or (null subsequence)
          (zerop (length subsequence)))
      (simple-error-mon :w-sym "subseq-count"
                        :w-type 'function
                        :w-spec '("length of arg SUBSEQUENCE is zerop, "
                                  "arg SEQUENCE is an infinite set of 0 length SUBSEQUENCEs"))
      (do ((position (search subsequence sequence)
                     (search subsequence sequence :start2 (1+ position)))
           (count 1 (1+ count)))
          ((null position) (1- count)))))


;;; :SOURCE cllib/string.lisp
(defun split-seq (seq pred &key (start 0) end key strict)
  (declare (type sequence seq) 
	   (type (function (t t) t) pred) 
	   ;; (type fixnum start)
	   (type fixnum-exclusive start))
  (loop :for st0 = (if strict start
                       (position-if-not pred seq :start start
                                        :end end :key key))
        :then (if strict (if st1 (1+ st1))
                  (position-if-not pred seq :start (or st1 st0)
                                   :end end :key key))
        :with st1 = 0 :while (and st0 st1) :do
        (setq st1 (position-if pred seq :start st0 :end end :key key))
        :collect (subseq seq st0 (or st1 end))))

;;; :SOURCE clocc/src/screamer/iterate.lisp :WAS `split-list-odd-even'
(defun list-split-odd-even (lst &optional return-list)
  (do ((lis lst (cddr lis))
       (odds '())
       (evens '()))
      ((null lis) (if return-list 
                      (list (nreverse odds) (nreverse evens))
                      (values (nreverse odds) (nreverse evens))))
    (push (car lis) odds)
    (push (cadr lis) evens)))

;;;; :SOURCE cllib/data.lisp :WAS `list->intervals'
(defun list-to-intervals (list)
  (let ((beg (car list))
	(end (car list)) ret)
    (dolist (curr (cdr list) (nreverse (cons (cons beg end) ret)))
      (if (= curr (1+ end))
          (setq end curr)
          (setq ret (cons (cons beg end) ret) end curr beg curr)))))

;;; Some simple functions that help avoid consing when we're just
;;; recursively filtering things that usually don't change.
;;; :SOURCE sbcl/src/compiler/disassem.lisp :WAS `sharing-cons'
(defun %sharing-cons (old-cons car cdr)
  (if (and (eq car (car old-cons)) (eq cdr (cdr old-cons)))
      old-cons
      (cons car cdr)))
;;; :SOURCE sbcl/src/compiler/disassem.lisp :WAS `sharing-mapcar'
(defun mapcar-sharing (fun list)
 (declare (type function fun))
  (and list
       (%sharing-cons list
		      (funcall fun (car list))
		      (mapcar-sharing fun (cdr list)))))


;;; ==============================
;;; :SEQS-DOCUMENTATION
;;; ==============================

(fundoc 'list-elt 
  "Return element at IDX in LST.~%~@
setfable~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'interleave ;;; LMH
  "Interleave the two lists LST-A and LST-B.~%
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'list-take 
  "TAKE-N elements FROM-LST.~%~@
:EXAMPLE~%
 \(list=take 2 '\(a b c d e\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'last-cons 
      "Get the last cons IN-LIST.~%~@
:EXAMPLE~%
 \(last-cons '\(a b c \(d . \(a\)\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'list-dotted-p-destructure
 "Return tail of dotted-list for destructuring.~%~@
Return OJBECT as if by `cl:values'.~%~@
When OBJECT of type `mon:dotted-list' first value is the terminating atom of
OBJECT, second is a version of OBJECT with terminating atom replaced by NIL.~%~@
When OBJECT is of type `cl:null' first value s NIL, second value is NIL.~%~@
When OBJECT is any other type first value is NIL, second value is OBJECT.~%~@
When OBJECT is of type `mon:circular-list' signal a `mon:circular-list-error'.~%~@
:EXAMPLE~%
 \(list-dotted-p-destructure \(cons 'a  'c\)\)~%
 \(list-dotted-p-destructure '\(a . c\)\)~%
 \(list-dotted-p-destructure '\(a b . c\)\)~%
 \(list-dotted-p-destructure nil\)~%
 \(list-dotted-p-destructure '\(a b c\)\)~%
 \(list-dotted-p-destructure \"STRING\"\)~%
 \(let \(\(list \(list 1 2 3\)\)\)
   \(setf \(cdddr list\) list\)
   \(list-dotted-p-destructure list\)\)~%~@
:SEE-ALSO `mon:list-proper-p', `mon:list-dotted-p', `mon:list-circular-p',
`mon:last-cons', `mon:nth-sane', `mon:list-from-singleton',
`mon:list-get-singleton', `mon:car-safe', `mon:cdr-safe', `cl:last'.~%▶▶▶")

(fundoc 'list-drop 
  "~%DROP-N elements FROM-LST~%~@
:EXAMPLE~%
 \(drop 2 '\(a b c d e\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'list-slice
"Partition IN-LIST into N-TUPLES ~%~@
N-TUPLES is an integer  greater than 0. Its declared type is `mon:index-from-1'.~%~@
:EXAMPLE~%~@
 \(list-slice 3 '\(1 2 3 4 5 6 7\)\)~%~@
:SEE-ALSO `mon:list-n-tuples'.~%▶▶▶")

(fundoc 'list-n-tuples
        "Invoke function W-FUN for each set of N-TUPLES IN-LIST.~%~@
N-TUPLES must be an integer value 1 or greater.
:EXAMPLE~%
 \(list-n-tuples \(lambda \(x\) \(print x\)\) 3 '\(1 2 3 4 5 6 7\)\)~%~@
:SEE-ALSO `mon:list-slice'.~%▶▶▶")

(fundoc 'last-elt 
  "Return the car of the `last' elt in LST.~%~@
:EXAMPLE~%
 \(last-elt '(a b c d))~%
 \(last-elt '\(a b c . d\)\)~%
 \(last-elt '\(a b c . \(d \(a\)\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'freqs 
  "Return an alist of (num . freq) of elements of the SEQ.~%~@
The alist is sorted by decreasing frequencies. TEST defaults to `eql'.~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:SEE-ALSO `sequence-zero-len-p'.~%▶▶▶")

(fundoc 'list-group-by 
  "Group list items by TEST with KEY.~%~@
When TEST is an equality function satisfying the precicate `standard-test-function-p' 
dispatch return value is as if by `group-by-hash' and will most likely evaluate considerably faster
than calls which must instead dispatch on `group-by-list'.~%~@
:EXAMPLE~%
 \(list-group-by '\(\"the\" \"tan\" \"ant\" \"gets\" \"some\" \"fat\"\) :key #'length\)~%
 \(defparameter *nums* \(loop repeat 100000 collect \(random 100000\)\)\)~%
 \(time \(progn 
	 \(list-group-by *nums* :key #'\(lambda \(x\) \(mod x 100\)\)\) nil\)\)~%
 \(time \(progn 
	 \(list-group-by *nums* :test #'\(lambda \(x y\) \(eql x y\)\)
			:key #'\(lambda \(x\) \(mod x 100\)\)\) nil\)\)~%~@
:SEE-ALSO `group-by-w-hash', `group-by-w-seq'.~%▶▶▶")

(fundoc 'group-by-w-seq 
  "Group LIST items by TEST with KEY.~%~@
Helper function for list-group-by~%~@
:EXAMPLE~%
 \(group-by-w-seq '\(\"the\" \"tan\" \"ant\" \"gets\" \"some\" \"fat\"\) 
                 #'\(lambda \(x\) \(> length 2\)\)  #'length \)~%~@
:NOTE When TEST is an equality function satisfying the precicate
`standard-test-function-p' it is faster to evaluate LIST with `group-by-w-hash'.
:SEE-ALSO `group-by-w-hash'.~%▶▶▶")

(fundoc 'subseq-count
"Return the number of occurences of SUBSEQUENCE in SEQUENCE.~%~@
Signal an error if subsequence is null or has length satisfying `zerop'.~%
SUBSEQUENCE is a sequence of elements occuring sequentially in SEQUENCE.~%~@
:EXAMPLE~%
 \(subseq-count '\(a b\) '\(a b '\(a b\) '\(a b\) a b c d '\(a b\)\)\)
 \(subseq-count \"dog\" #\(a b a b \(a b\) #\(q z\) a b c d #\\d #\\o #\\g\)\)
 \(subseq-count \"dog\" '\(a b a b #\\d #\\o #\\g\)\)
 \(subseq-count \"dog\" \"dog cat dog cat dog\"\)
 \(subseq-count #\(#\\d #\\o #\\g\) \"dog cat dog cat dog\"\)
 \(subseq-count '\(#\\d #\\o #\\g\) \"dog cat dog cat dog\"\)
:NOTE Does not match conses
 \(subseq-count '\(\(a b\)\) '\(a b '\(a b\) '\(a b\) a b c d '\(a b\)\)\)
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'group-by-w-hash
  "Group LIST items by TEST with KEY.~%~@
Helper function for `list-group-by' called when TEST satisfies precicate `standard-test-function-p'.~%~@
:EXAMPLE~%
 \(group-by-w-hash '\(\"the\" \"tan\" \"ant\" \"gets\" \"some\" \"fat\"\) #'equal #'length \)~%~@
:SEE-ALSO `group-by-w-seq'.~%▶▶▶")

(fundoc  'add-to-list
  "Add element to the value of list-var if it isn't there yet.~%~@
The test for presence of element is as if by `cl:equal',
or with compare-fn if that's non-nil.~%~@
If element is added, it is added at the beginning of the list,
unless the optional argument append is non-nil, in which case
element is added at the end.~%~@
The return value is the new value of list-var.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:add-to-nth-list', `cl:pushnew'.~%▶▶▶")

(fundoc 'add-to-nth-list
"cdr down LIST N times, push THING into the car of cons cell at N. Return LIST.~%~@
If N exceeds the bounds of LIST's lenth List is extended if necessary.~%~@
:EXAMPLE~%
 \(add-to-nth-list nil \"bubba\" 0\)~%
 \(add-to-nth-list nil \"bubba\" 1\)~%
 \(add-to-nth-list '\(a b c\) \"bubba\" 0\)~%
 \(add-to-nth-list '\(nil b c\) \"bubba\" 0\)~%
 \(add-to-nth-list '\(nil . nil\) 'q 0\)
 \(add-to-nth-list '\(nil nil\) 'q 0\)
 \(add-to-nth-list '\(nil nil\) 'q 1\)
 \(add-to-nth-list \"\" 'q 1\)
 \(let \(\(lst '\(a b c d e f\)\)\)
   \(loop 
      for add upfrom 0 below 10
      for new = \(copy-seq lst\)
      collect \(list :at-nth add \(add-to-nth-list new \"bubba\" add\)\) into rtn
      finally \(return \(nconc `\(\(:original ,lst\)\) rtn\)\)\)\)~%~@
:SEE-ALSO `mon:add-to-list', `cl:pushnew'.~%▶▶▶")

(fundoc  'setcar  
  "Set the car of CELL to be NEWCAR.  Return NEWCAR.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc  'setcdr  
  "Set the cdr of CELL to NEWCDR, return NEWCDR.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc  'car-safe 
"Return the car of OBJECT if it is a cons cell, else NIL.~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'cdr-safe 
"Return the cdr of OBJECT if it is a cons cell, or else nil.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `car-safe'.~%▶▶▶")


;;; ==============================
;;; :EQ-FUNCTIONS
;;; ==============================

(fundoc 'not-eq 
"Return non-nil if OBJ-X is not `cl:eq' OBJ-Y~%~@
:EXAMPLE~%
 \(not-eq 'a 'a\)~%
 \(not-eq 'a 'b\)~%
 \(not-eq #1=\#\\\a \#1\#\)~%
 \(not-eq \"a\" \"a\"\)~%
 \(not-eq \"a\" \(string  #\\a\)\)~%~@
:NOTE Per ANSI spec (eq 1.0 1.0) might not necessarily return T.~%
 (eq 1.0 1.0)  ;; \(esp. at top level\)~%
 (eql 1.0 1.0)~%
 ,----
 | An implementation is permitted to make \"copies\" of characters and
 | numbers at any time.  The effect is that Common Lisp makes no guarantee
 | that ‘eq’ is true even when both its arguments are \"the same thing\" if
 | that thing is a character or number.
 `----~%~@
:NOTE The following operators are defined to use `cl:eq' rather than `cl:eql':
 `cl:catch', `cl:throw',
 `cl:get', `cl:get-properties', `cl:getf', 
 `cl:remf', `cl:remprop'~%
:SEE \(info \"\(ansicl\)eq\"\)~%
:SEE-ALSO `mon:car-eq', `mon:not-eq', `mon:memq', `mon:position-eq', `delq',
`mon:remq', `mon:adjoinq', `mon:union-eq-keep-first'.~%▶▶▶")

(fundoc 'car-eq 
 "Return non-nil when car of LST-X is `eq' OBJ-B.~%~@
:EXAMPLE~%
 (let ((mk-car 1.0))
   (list (car-eq (cons mk-car 2) mk-car)
         (car-eq (cons mk-car 2) 1.0)
         (eq 1.0 1.0)
         (eq mk-car 1.0)
         (eq mk-car (float 1))))
:NOTE It is likeley that called at top level `cl:eq' will not return t for 
 (eq 1.0 1.0)~%
 (eql 1.0 1.0)~%~%
:SEE-ALSO `mon:not-eq', `mon:memq', `mon:position-eq', `delq', `mon:remq',
`mon:adjoinq', `mon:union-eq-keep-first', `cl:catch', `cl:throw', `cl:get',
`cl:get-properties', `cl:getf', `cl:remf', `cl:remprop'.~%▶▶▶")

(fundoc 'adjoinq 
  "Add ITEM to LIST as if by `cl:adjoin' with :test arg `cl:eq'~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:SEE-ALSO `mon:car-eq', `mon:not-eq', `mon:memq', `mon:position-eq', `delq',
`mon:remq', `mon:union-eq-keep-first', `cl:catch', `cl:throw',
`cl:get', `cl:get-properties', `cl:getf', `cl:remf', `cl:remprop'.~%▶▶▶")

(fundoc 'memq 
 "Return non-nil if elt is an element of list.~%~@
Comparison is as if by `cl:eq'.~%~@
The value is actually the tail of list whose car is elt.~%~@
:EXAMPLE~%
 \(memq 'a  '\(a . c\)\)~%
 \(memq 'c  '\(a . c\)\)~%
 \(memq 'a  '\(c d . a\)\)~%
 \(memq 'c  '\(c d . a\)\)~%
 \(memq 'd  '\(c d . a\)\)~%
 \(memq 'a  '\(c d . a\)\)~%
 \(memq nil '\(c nil . a\)\)~%
 \(memq 'a  '\(c d a\)\)~%
 \(memq nil '\(nil d a\)\)~%
 \(memq nil '\(d \(\) a\)\)~%
:NOTE This is not an entirely faithful implementation of Emacs lisp's `memq':~%~@
elisp> \(memq   'a '\(a . b\)\)
       ;=> \(a . b\)
elisp> \(memq   'b '\(a . b\)\)
       ;=> wrong-type-argument
elisp> \(member 'a '\(a . b\)\) ;=> \(a . b\)
elisp> \(member 'b '\(a . b\)\) ;=> nil
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:not-eq', `mon:memq', `mon:position-eq', `delq', `mon:remq',
`mon:union-eq-keep-first', `mon:car-eq', `mon:adjoinq', `cl:catch', `cl:throw',
`cl:get', `cl:get-properties', `cl:getf', `cl:remf', `cl:remprop'.~%▶▶▶")

(fundoc 'position-eq
        "Return the position of the first element in LST `eq' to ITEM.~%~@
Like \(position {...} :test #'eq\)~%~@
:EXAMPLE~%
 \(position-eq 1   '\(a b q 1 4\)\)~%
 \(position-eq 1   '\(a b q 1.0 4\)\)~%
 \(position-eq 1.0 '\(a b q 1 4\)\)~%
;; :NOTE Tests for `cl:eq' do not find floats \(among other things\).~%~@
 \(position 1   '\(a b q 1 4\)   :test #'eq\)~%
 \(position 1   '\(a b q 1.0 4\) :test #'eq\)~%
 \(position 1.0 '\(a b q 1.0 4\) :test #'eq\)~%
 \(position 1   '\(a b q 1.0 4\) :test #'eql\)~%
 \(position 1.0 '\(a b q 1.0 4\) :test #'eql\)~%
:SEE-ALSO `cl:position', `mon:not-eq', `mon:memq', `mon:position-eq', `mon:delq',
`mon:remq', `mon:union-eq-keep-first', `mon:car-eq', `mon:adjoinq', `cl:catch',
`cl:throw', `cl:get', `cl:get-properties', `cl:getf', `cl:remf',
`cl:remprop'.~%▶▶▶")

(fundoc 'union-eq-keep-first
"An alternative set union operation.~%~@
Common Lisp's `cl:union' doesn't guarantee the order when there are duplicates.~%~@
This version of union guarantees that the position of the FIRST occurance
of a duplicate is preserved.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:not-eq', `mon:memq', `mon:position-eq', `mon:delq', `mon:remq',
`mon:union-eq-keep-first', `mon:car-eq', `mon:adjoinq', `cl:catch', `cl:throw',
`cl:get', `cl:get-properties', `cl:getf', `cl:remf', `cl:remprop'.~%▶▶▶")

(fundoc 'delq
  "Delete by side effect any occurrences of elt as a member of list.~%~@
The modified list is returned.  Comparison is as if by `cl:eq'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `cl:delete', `cl:remove', `mon:delete-dups', `mon:delete-w-count',
`mon:not-eq', `mon:memq', `mon:position-eq', `mon:remq',
`mon:union-eq-keep-first', `mon:car-eq', `mon:adjoinq', `cl:catch', `cl:throw',
`cl:get', `cl:get-properties', `cl:getf', `cl:remf', `cl:remprop'.~%▶▶▶")

(fundoc 'remq  
  "Return LIST with all occurrences of ELT removed.~%~@
The comparison is done with `cl:eq'.~%~@
Contrary to `cl:delq', this does not use side-effects, and the argument LIST is
notmodified.~%~@
:EXAMPLE~%~@
  { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `cl:delete', `cl:remove', `mon:delete-dups', `mon:delete-w-count',
`mon:not-eq', `mon:memq', `mon:position-eq', `delq', `mon:remq',
`mon:union-eq-keep-first', `mon:car-eq', `mon:adjoinq', `cl:catch', `cl:throw',
`cl:get', `cl:get-properties', `cl:getf', `cl:remf', `cl:remprop'.~%▶▶▶")

(fundoc 'delete-all-elts-eq
"Delete all `cl:eq' elts IN-SUBLIST FROM-LIST~%~@
:EXAMPLE~%
 \(delete-all-elts-eq '\(e d a q\) '\(a q \(a b c\) e d \(f g h\) \(a b c\) \(q e d\)\)\)~%~@
:SEE-ALSO `cl:delete', `cl:remove', `mon:delete-w-count', `mon:delete-dups',
`mon:delete-all-elts-eq', `mon:car-eq', `mon:adjoinq'`cl:catch', `cl:throw',
`cl:get', `cl:get-properties', `cl:getf', `cl:remf', `cl:remprop'.~%▶▶▶")

;;; ==============================
(fundoc 'delete-dups
  "Destructively remove `cl:equal' duplicates from LIST.~%~@
Store the result in LIST and return it.~%~@
LIST must be a proper list.~%~@
Of several `cl:equal' occurrences of an element in LIST, the first one is kept.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:delete-w-count', `mon:delete-dups', `delete-all-elts-eq',
`cl:delete', `cl:remove'.~%▶▶▶")

(fundoc 'delete-w-count
"Like `cl:delete' but also return as if by `values' the number of items deleted.~%~@
:EXAMPLE~%
 \(delete-w-count 'a '\(a b c a b q\)\)~%
  ;=>\(B C B Q\)
  ;  2~%~@
:NOTE This is what `cl:delete' should have been \(and was on the LispMs\).~%~@
:SEE-ALSO `mon:delete-dups', `delete-all-elts-eq', `cl:delete',
`cl:remove'.~%▶▶▶")

(fundoc 'copy-sequence  
    "Return a copy of a list, vector, string or char-table.~%~@
 The elements of a list or vector are not copied; they are shared with the original.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `cl:copy-tree', `cl:copy-seq', `cl:copy-alist', `cl:copy-list',
`cl:copy-structure'.~%▶▶▶")

(fundoc 'nsublist 
 "Return the part of list LST between POS0 and POS1, *destructively*.~%~@
0 Indexed.~%~@
:EXAMPLE~%
 \(nsublist '(1 2 3 4 5) nil 2\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'flatten 
"Traverses the tree in order, collecting non-null leaves into a list.~%~@
:EXAMPLE~%
 \(flatten \(1 \(2\) \(3 \(4\) \(5 \(6\) 7\) 8\) 9\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'list-subsets 
"Return a list of all subsets of the given set \(represented as a list\).~%~@
:EXAMPLE~%
 \(list-subsets '\(a b a b c d\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'list-to-array 
"Convert the list to an array of given dimensions DIM, assuming row-major order.~%~@
:EXAMPLE~%
 \(list-to-array '\(0 1 3 5 7\) 5\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'list-to-intervals 
"Return the intervals for conses in LIST.~%~@
:EXAMPLE~%
 \(list-to-intervals '\(10 9 8 5 4 3 1\)\)~% 
 ;=> \(\(10 . 10\) \(9 . 9\) \(8 . 8\) \(5 . 5\) \(4 . 4\) \(3 . 3\) \(1 . 1\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'list-split-odd-even 
        "Split LIST into odd and even numbered elements.~%~@
Return odd and even postioned elements as if by `cl:values':~%
 - nth-value 0 is a list contained of the evens
 - nth-value 1 is a list contained of the odds~%~@
When optional arg RETURN-LIST is non-nil return valuis is a two element list:
 - car is a list contained of the evens
 - cadr is a list contained of the odds~%~@
:EXAMPLE~%
 \(list-split-odd-even \(number-sequence 0 10\)\)~%
 \(list-split-odd-even \(number-sequence 1 10\)\)~%
 \(list-split-odd-even \(number-sequence 1 10\) t\)~%
 \(let \(\(mkpairs \(list-split-odd-even \(number-sequence 1 10\) t\)\)\)
   \(reverse \(pairlis \(car mkpairs\) \(cadr mkpairs\)\)\)\)~%
 \(multiple-value-bind \(even odd\) \(list-split-odd-even \(number-sequence 1 10\)\)
   \(reverse \(pairlis even odd\)\)\)~%~@
:SEE-ALSO `cl:pairlist', `cl:acons'.~%▶▶▶")

(fundoc 'split-seq 
"Return a list of subseq's of SEQ, split on predicate PRED.~%~@
Start from START, end with END.~%~@
If STRICT is non-nil, collect zero-length sub-sequences too.~%~@
 \(split-seq SEQ PRED &key \(start 0\) end key strict\)~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'list-from-singleton 
"When ARG is an atom a list containing the atom is returned, else return ARG.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `singleton-p', `list-get-singleton'.~%▶▶▶")

(fundoc 'list-get-singleton 
"When ARG is a list with a single element return ARG as singleton.~%~@
When ARG is not single element list return ARG.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `singleton-p', `list-from-singleton'.~%▶▶▶")

(fundoc 'list-quote-elts 
"Quote the elements in list lst.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'car-less-than-car 
"Return T if \(car A\) is numerically less than \(car B\).~%~@
Arg A and B should satisfy `cl:consp'.~%~@
Signal an error if car of either A or B is not `cl:realp'
:EXAMPLE~%
 \(car-less-than-car '\(2 . a\) '\(3 . b\)\)~%
 \(car-less-than-car '\(2  a\)  '\(3  b\)\)~%~@
;; Following errors successfully:~% 
 \(car-less-than-car '\(2  a\)  '\(b  3\)\)~%
 \(car-less-than-car '\(2  a\)  nil\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:car-greater-than-car'.~%▶▶▶")

(fundoc 'car-greater-than-car
"Return T if \(car A\) is numerically greater than \(car B\).~%~@
Arg A and B should satisfy `cl:consp'.~%~@
Signal an error if car of either A or B is not `cl:realp'
:EXAMPLE~%~@
 \(car-less-than-car '\(3 . a\) '\(2 . b\)\)~%
 \(car-less-than-car '\(3  a\)  '\(2  b\)\)~%~@
;; Following errors successfully:~% 
 \(car-less-than-car '\(3  a\)  '\(b  2\)\)~%
 \(car-less-than-car nil  '\(b  2\)\)~%~@
:SEE-ALSO `mon:car-less-than-car'.~%▶▶▶")


;;; ==============================
;;; :LIST-LENGTHS
;;; ==============================

(fundoc 'list-dotted-length
"Return the number of cons cells in DOTTED-LIST.~%~@
DOTTED-LIST must be a dotted list or a proper list.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:list-lengths', `mon:list-dotted-length', `mon:list-circular-lengths',
`mon:list-proper-p', `mon:proper-list', `mon:list-proper-not-null-p',
`mon:proper-list-not-null', `mon:list-dotted-p', `mon:dotted-list',
`mon:circular-list', `mon:list-circular-p', `mon:each-a-sequence-proper',
`mon:sequencep', `mon:sequence-zerop', `mon:sequence-type'.~%▶▶▶")

(fundoc 'list-circular-lengths
"Return the length of a circular-list.~%~@
CIRCULAR-LIST must be a circular list.~%~@
Return value is the length of CIRCULAR-LIST's stem and the length of
the cyclic circlular portion.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:list-lengths', `mon:list-dotted-length', `mon:list-circular-lengths',
`mon:list-proper-p', `mon:proper-list', `mon:list-proper-not-null-p',
`mon:proper-list-not-null', `mon:list-dotted-p', `mon:dotted-list',
`mon:circular-list', `mon:list-circular-p', `mon:each-a-sequence-proper',
`mon:sequencep', `mon:sequence-zerop', `mon:sequence-type'.~%▶▶▶")

;; (values nil nil)
;; (export (find-symbol "LIST-LENGTHS"))
(fundoc 'list-lengths
"Return the length of a LIST.~%~@
LIST is any kind of list: proper-list, circular-list or dotted-list.~%~@
Return as if by `cl:values' as follows:~%
 - for a proper list, the length of the list and 0;~%
 - for a circular list, the length of the stem, and the length of the circle;~%
 - for a dotted list, the number of cons cells, and nil;~%
 -  for an atom, nil, and (type-of <ATOM>).~%
:EXAMPLE~%
 ;; :ATOM
 \(equal \(multiple-value-list \(list-lengths 'a\)\)               '\(nil symbol\)\)~%
 ;; :PROPER-LISTS
 \(equal \(multiple-value-list \(list-lengths \(\)\)\)           '\(0 0\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a\)\)\)         '\(1 0\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b\)\)\)       '\(2 0\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b c\)\)\)     '\(3 0\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b c d\)\)\)   '\(4 0\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b c d e\)\)\) '\(5 0\)\)~%
 ;; :DOTTED-LISTS
 \(equal \(multiple-value-list \(list-lengths '\(a . b\)\)\)         '\(1 nil\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b . c\)\)\)       '\(2 nil\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b c . d\)\)\)     '\(3 nil\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b c d . e\)\)\)   '\(4 nil\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b c d e . f\)\)\) '\(5 nil\)\)~%
 ;; :CIRCULAR-LISTS
 \(equal \(multiple-value-list \(list-lengths '#1=\(a . #1#\)\)\)               '\(0 1\)\)~%
 \(equal \(multiple-value-list \(list-lengths '#2=\(a b . #2#\)\)\)             '\(0 2\)\)~%
 \(equal \(multiple-value-list \(list-lengths '#3=\(a b c . #3#\)\)\)           '\(0 3\)\)~%
 \(equal \(multiple-value-list \(list-lengths '#4=\(a b c d . #4#\)\)\)         '\(0 4\)\)~%
 \(equal \(multiple-value-list \(list-lengths '#5=\(a b c d e . #5#\)\)\)       '\(0 5\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a . #6=\(b . #6#\)\)\)\)         '\(1 1\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a . #7=\(b c . #7#\)\)\)\)       '\(1 2\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a . #8=\(b c d . #8#\)\)\)\)     '\(1 3\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a . #9=\(b c d e . #9#\)\)\)\)   '\(1 4\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b . #10=\(c . #10#\)\)\)\)     '\(2 1\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b . #11=\(c d . #11#\)\)\)\)   '\(2 2\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b . #12=\(c d e . #12#\)\)\)\) '\(2 3\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b c . #13=\(d . #13#\)\)\)\)   '\(3 1\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b c . #14=\(d e . #14#\)\)\)\) '\(3 2\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b c d . #15=\(e . #15#\)\)\)\) '\(4 1\)\)~%
 \(equal \(multiple-value-list \(list-lengths '\(a b c d e . #16=\(#16#\)\)\)\)   '\(6 0\)\)~%~@
:SEE-ALSO `mon:list-lengths', `mon:list-dotted-length', `mon:list-circular-lengths',
`mon:list-proper-p', `mon:proper-list', `mon:list-proper-not-null-p',
`mon:proper-list-not-null', `mon:list-dotted-p', `mon:dotted-list',
`mon:circular-list', `mon:list-circular-p', `mon:each-a-sequence-proper',
`mon:sequencep', `mon:sequence-zerop', `mon:sequence-type'.~%▶▶▶")



;;; ==============================
;;; Predicate and predicate like
;;; ==============================

(fundoc 'list-length-n-p
        "Wheter length of PROPER-LIST is = INT.~%~@
e.g. is it that:~%
 \(= \(length seq\) int\)~%
Signal a `mon:proper-list-error' if PROPER-LIST is not of type `mon:proper-list'.~%~@
Signal a `cl:type-error' if INT is not of type `mon:fixnum-0-or-over'.~%~@
:EXAMPLE~%
 \(list-length-n-p '\(a b\) 2\)~%
 \(list-length-n-p '\(a b\) 3\)~%
 \(list-length-n-p nil 0\)~%
 \(list-length-n-p \(list\) 0\)~%
 \(list-length-n-p \(cons nil nil\) 0\)~%~@
;; Following successfully signal an error:~%
 \(list-length-n-p '\(nil . t\) 0\)~%
 \(list-length-n-p '\(\) -1\) ~%~@
:SEE-ALSO `mon:sequence-zerop', `cl:list-length', `cl:length', `cl:endp'.~%▶▶▶")

(fundoc 'list-transpose 
"Turn a list-of-lists on its side.~%~@
If the rows are of unequal length, truncate uniformly to the shortest.~%~@
:EXAMPLE~%
 \(transpose-lists '\(\(ONE TWO THREE\) \(1 2\)\)\)~%
 ;=> \(\(ONE 1\) \(TWO 2\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'mapcar-sharing
  "A simple \(one list arg\) version of `cl:mapcar'~%~@
Avoids consing up a new list as long as the results of calling FUN on the
elements of LIST are eq to the original. An Emacs lisp style mapcar.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `%sharing-cons'.~%▶▶▶")

(fundoc '%sharing-cons
  "If CAR is eq to the car of OLD-CONS and CDR is eq to the CDR, return
OLD-CONS, otherwise return \(cons CAR CDR\).~%~@
Helper function for `mon:mapcar-sharing'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")


;;; ==============================
;;; Provide better documentation of functions for the split-sequence package
;;; ==============================

#+split-sequence 
(fundoc 'split-sequence:split-sequence 
  "Split SEQ delimited by DELIMITER.~%~@
Return value is as if by `cl:values':~%
 - nth-value 0 is a list of subsequences~%
 - nth-value 1 is an index into SEQ indicating where processing stopped
   It is suitable for use as an argument to `cl:subseq'.~%~@
Arg DELIMITER is an object upon which to split SEQ by.~%~@
Arg SEQ is a proper seqeuence.~%~@
The keyword arguments work analogously to those of `cl:substitute' with the
proviso that the behaviour of keyword :from-end is possibly different from
`cl:substitute' such that :from-end values of NIL and T are equivalent except
where :count is supplied.~%~@
Keyword COUNT is an integer or nil, the default is nil.~%~@
Keyword REMOVE-EMPTY-SUBSEQS when non-nil indicates that empty subsequences are
discarded from return value if :remove-empty-subseqs is nil \(the default\),
empty subsequences are included with return value.~%~@
Keyword START and END are bounding index designator of SEQ. The defaults for
START and END are 0 and nil, respectively.~%~@
Keyword FROM-END is a generalized boolean. The default is nil.~%~@
Keyword TEST is a designator for a function of two arguments that returns a
generalized boolean. Default is nil.~%~@
Keyword TEST-NOT is a designator for a function of two arguments that returns a
generalized boolean. Default is nil.~%~@
Keyword KEY isa a designator for a function of one argument. Default is nil.~%~@
:EXAMPLE~%~@
 \(split-sequence #\\; \"a;;b;c\"\)
 => \(\"a\" \"\" \"b\" \"c\"\), 6~%
 \(split-sequence #\\; \"a;;b;c\" :from-end t\)
 => \(\"a\" \"\" \"b\" \"c\"\), 0~%
 \(split-sequence #\\; \"a;;b;c\" :from-end t :count 1\)
 => \(\"c\"\), 4~%
 \(split-sequence #\\; \"a;;b;c\" :remove-empty-subseqs t\)
 => \(\"a\" \"b\" \"c\"\), 6~%
 \(split-sequence #\\; \";oo;bar;ba;\" :start 1 :end 9\)
 => \(\"oo\" \"bar\" \"b\"\), 9~%~@
:SEE info node \(info \"\(ansicl\)substitute\"\)
:SEE info node \(info \"\(ansicl\)Rules about Test Functions\"\)
:SEE-ALSO `split-sequence:split-sequence-if',
`split-sequence:split-sequence-if-not', `cl:substitute-if',
`cl:substitute-if-not', `cl:nsubstitute', `cl:nsubstitute-if',
`cl:nsubstitute-if-not', `cl:position', `cl:position-if',
`cl:position-if-not'.~%▶▶▶")

#+split-sequence
(fundoc 'split-sequence:split-sequence-if
  "Split SEQ into subsequences delimited by items satisfying PREDICATE.~%~@
Return value is as if by `cl:values':~%
 - nth-value 0 is a list of subsequences~%
 - nth-value 1 is an index into SEQ indicating where processing stopped
   It is suitable for use as an argument to `cl:subseq'.~%~@
Arg PREDICATE is a designator for a function of one argument that returns a
generalized boolean.~%~@
Arg SEQ is a proper seqeuence.~%~@
The keyword arguments work analogously to those of `cl:substitute-if' with the
proviso that the behaviour of keyword :from-end is possibly different from
`cl:substitute-if' such that :from-end values of NIL and T are equivalent except
where :count is supplied.~%~@
Keyword COUNT is an integer or nil, the default is nil.~%~@
Keyword REMOVE-EMPTY-SUBSEQS when non-nil indicates that empty subsequences are
discarded from return value if :remove-empty-subseqs is nil \(the default\),
empty subsequences are included with return value.~%~@
Keyword START and END are bounding index designator of SEQ. The defaults for
START and END are 0 and nil, respectively.~%~@
Keyword FROM-END is a generalized boolean. The default is nil.~%~@
Keyword KEY isa a designator for a function of one argument. Default is nil.~%~@
:EXAMPLE~%
 \(split-sequence-if \(lambda \(x\) \(member x '\(#\\a #\\b\)\)\) \"abracadabra\"\)
 => \(\"\" \"\" \"r\" \"c\" \"d\" \"\" \"r\" \"\"\), 11~%~@
:SEE info node \(info \"\(ansicl\)substitute\"\)
:SEE info node \(info \"\(ansicl\)Satisfying a One-Argument Test\"\)
:SEE-ALSO `split-sequence:split-sequence',
`split-sequence:split-sequence-if-not', `cl:substitute', `cl:substitute-if-not',
`cl:nsubstitute', `cl:nsubstitute-if', `cl:nsubstitute-if-not', `cl:position',
`cl:position-if', `cl:position-if-not'.~%▶▶▶")

#+split-sequence
(fundoc 'split-sequence:split-sequence-if
  "Split SEQ into subsequences delimited by items satisfying PREDICATEs complement.~%~@
Return value is as if by `cl:values':~%
 - nth-value 0 is a list of subsequences~%
 - nth-value 1 is an index into SEQ indicating where processing stopped
   It is suitable for use as an argument to `cl:subseq'.~%~@
Arg PREDICATE is a designator for a function of one argument that returns a
generalized boolean.~%~@
Arg SEQ is a proper seqeuence.~%~@
The keyword arguments work analogously to those of `cl:substitute-if-not' with
the proviso that the behaviour of keyword :from-end is possibly different from
`cl:substitute-if-not' such that :from-end values of NIL and T are equivalent
except where :count is supplied.~%~@
Keyword COUNT is an integer or nil, the default is nil.~%~@
Keyword REMOVE-EMPTY-SUBSEQS when non-nil indicates that empty subsequences are
discarded from return value if :remove-empty-subseqs is nil \(the default\),
empty subsequences are included with return value.~%~@
Keyword START and END are bounding index designator of SEQ. The defaults for
START and END are 0 and nil, respectively.~%~@
Keyword FROM-END is a generalized boolean. The default is nil.~%~@
Keyword KEY isa a designator for a function of one argument. Default is nil.~%~@
:EXAMPLE~%
 \(split-sequence-if-not \(lambda \(x\) \(member x '\(#\\a #\\b\)\)\) \"abracadabra\"\)
 => \(\"ab\" \"a\" \"a\" \"ab\" \"a\"\), 11
:SEE info node \(info \"\(ansicl\)substitute\"\)
:SEE info node \(info \"\(ansicl\)Satisfying a One-Argument Test\"\)
:SEE-ALSO `split-sequence:split-sequence', `split-sequence:split-sequence-if',
`cl:substitute', `cl:substitute-if', `cl:nsubstitute', `cl:nsubstitute-if',
`cl:nsubstitute-if-not', `cl:position', `cl:position-if',
`cl:position-if-not'.~%▶▶▶")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; paragraph-ignore-fill-prefix: nil
;; package: mon
;; End:

;;; ==============================
;;; EOF
