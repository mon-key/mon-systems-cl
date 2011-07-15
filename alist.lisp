;;; :FILE mon-systems/alist.lisp
;;; ==============================


(in-package #:mon)


;;; ==============================
;;; :SOURCE garnet-20030525/kr/kr-macros.lisp :WAS `assocq'
;;; Assoc, but with a test of "eq"
;; (defmacro assq (item alist)
;;   (if (keywordp item)
;;   `(assoc ,item ,alist)
;;   `(assoc ,item ,alist :test #'eq)))
;;; ==============================
#-sbcl (defun assq (key a-list) 
	 (declare (list list))
	 (assoc key a-list :test #'eq))
;; :NOTE SBCL's assq is build from whole cloth w/ `do' macro.
#+sbcl 
(defun assq (key a-list)
  (declare (type list a-list))
  (sb-int::assq key a-list))

(define-compiler-macro assq (key a-list)
  `(assoc ,key (the list ,a-list) :test #'eq))

;;; :SOURCE cllib/miscprint.lisp :WAS `alist='
(defun alist-eql (a1 a2 &key (test #'eql))
  (macrolet ((a= (a b)
               `(dolist (pair ,a t)
                  (let ((other (assoc (car pair) ,b :test test)))
                    (unless (and other (funcall test (cdr pair) (cdr other)))
                      (return nil))))))
    (and (a= a1 a2) (a= a2 a1))))

;;; :SOURCE fare-utils/base/lists.lisp :`WAS `sort-keys'
(defun alist-sort-keys (alist &optional (sort-pred #'string<))
  (alist-to-plist (sort (plist-to-alist alist) sort-pred :key #'car)))

;;; The consing version of `nalist-to-plist'
;;; :SOURCE fare-utils/base/lists.lisp :`WAS `alist->plist'
(defun alist-to-plist (alist)
  (loop :for (key . val) :in alist :nconc (list key val)))

;;; :SOURCE cllib/miscprint.lisp :WAS `alist->plist'
(defun nalist-to-plist (alist)
  (do ((ll alist (cddr ll))) ((null ll) alist)
    (let ((co (car ll)))
      (setf (car ll) (car co)
	    (car co) (cdr co)
	    (cdr co) (cdr ll)
	    (cdr ll) co))))


;;; ==============================
;;; :ALIST-DOCUMENTATIION
;;; ==============================

(fundoc 'assq
  "Return non-nil if KEY is `eq' to the car of an element of A-LIST.~%~@
The value is actually the first element of A-LIST whose car is KEY.~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'alist-eql
"Check that the two association lists have the same values.~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:SEE-ALSO `plist-eql'.~%▶▶▶")

(fundoc 'alist-sort-keys
  "Sort keys of ALIST.~%~@
Optional arg SORT-PRED is a predicate to sort keys by, default is `cl:string<'.~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:SEE-ALSO `plist-to-alist'.~%▶▶▶")

(fundoc   'alist-to-plist

"Transform an ALIST to a PLIST as if by `cl:nconc', consing.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... }~%~@
:SEE-ALSO `nalist-to-plist', `nplist-to-alist', `plist-to-alist'.~%▶▶▶")

(fundoc   'nalist-to-plist
"Destructively transform ALIST to a PLIST as if by `do'/`setf', non-consing.~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:SEE-ALSO `nalist-to-plist', `nplist-to-alist', `plist-to-alist', `alist-to-plist'.~%▶▶▶")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF
