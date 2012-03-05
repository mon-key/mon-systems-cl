;;; :FILE mon-systems/plist.lisp
;;; ==============================



(in-package #:mon)
;; *package*

;; :SOURCE cllib/miscprint.lisp :WAS `alist->plist'
(defun nplist-to-alist (plist)
  (declare (proper-plist plist))
  #-sbcl
  (assert (plist-proper-p plist)
          ()
          ":FUNCTION `nplist-to-alist' -- expected an object of type `mon:proper-plist'~%~
           got: ~S~%" plist)
  (do ((ll plist (cdr ll))) ((null (cdr ll)) plist)
    (let ((co (cdr ll)))
      (setf (cdr ll) (cdr co)
            (cdr co) (car co)
            (car co) (car ll)
            (car ll) co))))

;;; consing
;;; :SOURCE fare-utils/base/lists.lisp :`WAS `plist->alist'
(defun plist-to-alist (plist)
  (declare (proper-plist plist))
  #-sbcl
  (assert (plist-proper-p plist)
          ()
          ":FUNCTION `plist-to-alist' -- expected an object of type `mon:proper-plist'~%~
           got: ~S~%" plist)
  (loop :for (key val) :on plist by #'cddr :collect (cons key val)))
#|

 (if (null plist)
     nil
     (acons (car plist) (cadr plist) (plist->alist (cddr plist))))
|#

;; :SOURCE cllib/miscprint.lisp :WAS `plist='
(defun plist-eql (p1 p2 &key (test #'eql))
  (declare (proper-plist p1 p2))
  #-sbcl (and (not (assert (plist-proper-p p1)
                           ()
                           ":FUNCTION `plist-eql' -- expected an object of type `mon:proper-plist'~%~
                            got: ~S~%" p1))
              (not (assert (plist-proper-p p1)
                           ()
                           ":FUNCTION `plist-eql' -- expected an object of type `mon:proper-plist'~%~
                            got: ~S~%" p1)))
  (macrolet ((p= (a b)
               `(do ((tail ,a (cddr tail)))
                    ((endp tail) t)
                  (unless (funcall test (second tail)
                                   (getf ,b (first tail) ,b))
                    (return nil)))))
    (and (p= p1 p2) (p= p2 p1))))

(defun put (symbol propname value)
  (setf (get symbol propname) value))

(defun putf (sym propname value)
  (setf (getf (symbol-plist sym) propname) value))

;;; ==============================
;; :SOURCE pjb/common-lisp/list.lisp
(defun plist-get (plist prop)
  (getf plist prop))

;;; ==============================
;; :SOURCE pjb/common-lisp/list.lisp
;; (defun plist-put (plist prop value)
;;   "Change value in PLIST of PROP to VALUE.~%
;; PLIST is a property list, which is a list of the form
;;  (PROP1 VALUE1 PROP2 VALUE2 {...})~%
;; PROP is a symbol and VALUE is any object.~%
;; If PROP is already a property on the list, its value is set to VALUE,
;; otherwise the new PROP VALUE pair is added.~%
;; The new plist is returned; use:
;;  \(setq x \(plist-put x prop val\)\)~%
;; to be sure to use the new value.~%
;; The PLIST is modified by side effects.~%"
;;   (setf (getf plist prop) value)
;;   plist)
;;; ==============================
;;
(defun plist-put (plist prop val)
  (let ((pl plist))
    (setf (getf pl prop) val)
    pl))

;;; ==============================
;; :SOURCE pjb/common-lisp/list.lisp
;; (defun plist-remove (plist prop)
;;   "Remove PROP from PLIST as if by `remf' return the modified plist.~%"
;;   (remf plist prop)
;;   plist)
;;; ==============================

;;; ==============================
;;; :SOURCE rucksack/cache.lisp
;; (defun sans (plist &rest keys)
;;   "Returns PLIST with keyword arguments from KEYS removed."
;;   ;; From Usenet posting <3247672165664225@naggum.no> by Erik Naggum.
;;   (let ((sans ()))
;;     (loop
;;       (let ((tail (nth-value 2 (get-properties plist keys))))
;;         ;; this is how it ends
;;         (unless tail
;;           (return (nreconc sans plist)))
;;         ;; copy all the unmatched keys
;;         (loop until (eq plist tail) do
;;               (push (pop plist) sans)
;;               (push (pop plist) sans))
;;         ;; skip the matched key
;;         (setq plist (cddr plist))))))
;;; ==============================

;; :SOURCE alexandria/lists.lisp :WAS `remove-from-plist'
(defun plist-remove (plist &rest keys)
  (declare (proper-plist plist)
           (optimize (speed 3)))
  #-sbcl
  (assert (plist-proper-p plist)
          ()
          ":FUNCTION `plist-remove' -- expected an object of type `mon:proper-plist'~%~
           got: ~S~%" plist)
  ;; :FIXME possible optimization: (plist-remove '(:x 0 :a 1 :b 2) :a)
  ;; could return the tail without consing up a new list.
  (loop for (key . rest) on plist by #'cddr
     ;; :WAS do (assert rest () "Expected a proper plist, got ~S" plist)
     unless (memq key keys)
     collect key and collect (first rest)))
;;
;; :SOURCE alexandria/lists.lisp :WAS `delete-from-plist'
(defun plist-delete (plist &rest keys)
  (declare (proper-plist plist))
  #-sbcl
  (assert (plist-proper-p plist)
          ()
          ":FUNCTION `plist-delete' -- expected an object of type `mon:proper-plist'~%~
           got: ~S~%" plist)
  ;; :FIXME should not cons
  (apply 'plist-remove plist keys))
;;
(define-modify-macro plist-removef (&rest keys)
  plist-remove)

(define-modify-macro plist-deletef (&rest keys)
  plist-delete)

;; :EXAMPLE
;; (progn
;;   (defparameter *item-template* '())
;;   (setf (getf (symbol-plist '*item-template*) 'bubba) "value")
;;   (setf (getf (symbol-plist '*item-template*) :bubba) "valueb")
;;   (prog1 
;;       (equal (mon::%plist-keys-fast (symbol-plist '*item-template*))
;;              '(:BUBBA BUBBA))
;;     (unintern '*item-template*)))
;;
;; (ignore-errors 
;;   (mon::%plist-keys-fast (cons nil nil)))
(defun %plist-keys-fast (plist)
  (declare ((or null proper-plist) plist))
  (if (null plist)
      plist
      (loop
         for key in plist by #'cddr collect key)))

;;; ==============================
;; :NOTE called by `dbc::%print-sax-parsed-slots-calculate-padding-for-format-control'
;; :SOURCE sbcl/src/pcl/ctor.lisp
(defun plist-keys (plist &key test)
  (declare (proper-plist plist))
  #-sbcl
  (assert (plist-proper-p plist)
          ()
          ":FUNCTION `plist-keys' -- expected an object of type `mon:proper-plist'~%~
           got: ~S~%" plist)
  (if test 
      (loop
         for (key . more) on plist by #'cddr
         if (funcall test key)
          collect key)
      (%plist-keys-fast plist)))

;; :SOURCE sbcl/src/pcl/ctor.lisp
(defun plist-values (plist &key test)
  (declare (proper-plist plist))
  #-sbcl
  (assert (plist-proper-p plist)
          ()
          ":FUNCTION `plist-values' -- expected an object of type `mon:proper-plist'~%~
           got: ~S~%" plist)
  (loop
     for (key . more) on plist by #'cddr
     if (or (null test)
            (funcall test (car more)))
      collect (car more)))

;; :PASTE-NUMBER 128074
;; :PASTE-TITLE Hide in property list
;; :PASTE-BY chr, stassats
;; :PASTE-DATE 2012-03-01
;; :PASTE-URL (URL `http://paste.lisp.org/+2QTM')
;; :WAS `hide-in-plist'
;;
(defun plist-hide (plist secret-indicators obfuscate-with)
  (loop 
     with secrets = (alexandria:ensure-list secret-indicators)
     for (key value) on plist by #'cddr collect key
     if (member key secrets)
     collect obfuscate-with
     else 
     collect value))


;;; ==============================
;;; :PLIST-DOCUMENTATIION
;;; ==============================

(fundoc 'plist-delete
  "Like `plist-remove', but may destructively modify PLIST.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:plist-keys', `mon:plist-values' `mon:plist-eql' `mon:nplist-to-alist',
`mon:plist-to-alist', `mon:plist-delete', `mon:plist-remove', `mon:plist-get',
`mon:plist-put', `mon:putf', `mon:put', `mon:plist-proper-p', `mon:proper-plist'
`cl:get-properties', `cl:get', `cl:getf', `cl:remf', `cl:remprop',
`cl:symbol-plist'.~%▶▶▶")

(fundoc 'plist-remove
 "Return propery-list PLIST with same KEYS and values, except that keys
in the list designated by KEYS and values corresponding to them are removed.~%~@
The returned property-list may share structure with the PLIST, but PLIST is
not destructively modified.~%~@
Keys are compared using EQ.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:plist-keys', `mon:plist-values' `mon:plist-eql' `mon:nplist-to-alist',
`mon:plist-to-alist', `mon:plist-delete', `mon:plist-remove', `mon:plist-get',
`mon:plist-put', `mon:putf', `mon:put', `mon:plist-proper-p', `mon:proper-plist'
`cl:get-properties', `cl:get', `cl:getf', `cl:remf', `cl:remprop',
`cl:symbol-plist'.~%▶▶▶")

;; plist-eq
(fundoc 'plist-eql
"Check that the two property lists have the same properties.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:alist-eql', `mon:plist-keys', `mon:plist-values' `mon:plist-eql'
`mon:nplist-to-alist', `mon:plist-to-alist', `mon:plist-delete',
`mon:plist-remove', `mon:plist-get', `mon:plist-put', `mon:putf', `mon:put',
`mon:plist-proper-p', `mon:proper-plist' `cl:get-properties', `cl:get',
`cl:getf', `cl:remf', `cl:remprop', `cl:symbol-plist'.~%▶▶▶")

(fundoc 'plist-to-alist
 "Transform PLIST of consecutive pairs into an alist as if by `cl:cons', consings.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:alist-to-plist', `mon:plist-keys', `mon:plist-values'
`mon:plist-eql' `mon:nplist-to-alist', `mon:plist-to-alist', `mon:plist-delete',
`mon:plist-remove', `mon:plist-get', `mon:plist-put', `mon:putf', `mon:put',
`mon:plist-proper-p', `mon:proper-plist' `cl:get-properties', `cl:get',
`cl:getf', `cl:remf', `cl:remprop', `cl:symbol-plist' .~%▶▶▶")

(fundoc 'nplist-to-alist
"Destructively transform a PLIST to an alist as if by `do'/`setf',  non-consing.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:nalist-to-plist', `mon:alist-to-plist', `mon:plist-keys',
`mon:plist-values' `mon:plist-eql' `mon:nplist-to-alist', `mon:plist-to-alist',
`mon:plist-delete', `mon:plist-remove', `mon:plist-get', `mon:plist-put',
`mon:putf', `mon:put', `mon:plist-proper-p', `mon:proper-plist'
`cl:get-properties', `cl:get', `cl:getf', `cl:remf', `cl:remprop',
`cl:symbol-plist' .~%▶▶▶")

(fundoc 'put
  "Store symbol's propname property with value value.~%~@
:EXAMPLE~%~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:plist-keys', `mon:plist-values' `mon:plist-eql'
`mon:nplist-to-alist', `mon:plist-to-alist', `mon:plist-delete',
`mon:plist-remove', `mon:plist-get', `mon:plist-put', `mon:putf', `mon:put',
`mon:plist-proper-p', `mon:proper-plist' `cl:get-properties', `cl:get',
`cl:getf', `cl:remf', `cl:remprop', `cl:symbol-plist'.~%▶▶▶")

(fundoc 'putf
  "Like `cl:put' but uses the generalized accessor `cl:getf' instead of `cl:get'.~%~@
Search the property list stored in SYM (a place) for a property EQ to PROPNAME.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:put', `mon:plist-keys', `mon:plist-values' `mon:plist-eql'
`mon:nplist-to-alist', `mon:plist-to-alist', `mon:plist-delete',
`mon:plist-remove', `mon:plist-get', `mon:plist-put', `mon:putf',
`mon:plist-proper-p', `mon:proper-plist' `cl:get-properties', `cl:get',
`cl:getf', `cl:remf', `cl:remprop', `cl:symbol-plist'.~%▶▶▶")

(fundoc 'plist-put
  "Change value in SYM's symbol-plist of PROP to VAL.~%~@
PLIST is a property list, which is a list of the form~%~@
 \(PROP1 VALUE1 PROP2 VALUE2 ...\).
PROP is a symbol and val is any object.~%~@
If PROP is already a property on the symbol-plist, its value is set to VAL,
otherwise the new PROP VAL pair is added.~%~@
The new plist is returned.~%~@
The plist is modified by side effects as if by `cl:setf'/`cl:getf'.~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:plist-keys', `mon:plist-values' `mon:plist-eql' `mon:nplist-to-alist',
`mon:plist-to-alist', `mon:plist-delete', `mon:plist-remove', `mon:plist-get',
`mon:plist-put', `mon:putf', `mon:put', `mon:plist-proper-p', `mon:proper-plist'
`cl:get-properties', `cl:get', `cl:getf', `cl:remf', `cl:remprop',
`cl:symbol-plist'.~%▶▶▶")

(fundoc 'plist-get
  "Extract a value from a property list.~%~@
PLIST is a property list, which is a list of the form
 \(PROP1 VALUE1 PROP2 VALUE2 {...} \)~%~@
This function returns the value corresponding to the given PROP, or nil if PROP
is not one of the properties on the list.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:plist-keys', `mon:plist-values' `mon:plist-eql'
`mon:nplist-to-alist', `mon:plist-to-alist', `mon:plist-delete',
`mon:plist-remove', `mon:plist-get', `mon:plist-put', `mon:putf', `mon:put',
`mon:plist-proper-p', `mon:proper-plist' `cl:get-properties', `cl:get',
`cl:getf', `cl:remf', `cl:remprop', `cl:symbol-plist'.~%▶▶▶")

;;
(fundoc 'plist-values
"Collect values in PLIST.~%~@
Keyword TEST is a function predicate applied to each value of PLIST.~%~@
:EXAMPLE~%
 \(plist-values '\(1 a 2 b 3 c\)\)~%
 \(plist-values '\(1 a 2 \"b\" 3 c\) :test #'stringp\)~%~@
:SEE-ALSO `mon:plist-keys', `mon:plist-values' `mon:plist-eql'
`mon:nplist-to-alist', `mon:plist-to-alist', `mon:plist-delete',
`mon:plist-remove', `mon:plist-get', `mon:plist-put', `mon:putf', `mon:put',
`mon:plist-proper-p', `mon:proper-plist' `cl:get-properties', `cl:get',
`cl:getf', `cl:remf', `cl:remprop', `cl:symbol-plist'.~%▶▶▶")

(fundoc 'plist-keys
"Collect keys in PLIST.~%~@
Keyword TEST is a function predicate applied to each key of PLIST.
:EXAMPLE~%
 \(plist-keys '\(1 a 2 b 3 c\)\)~%
 \(plist-keys '\(\"one\" a 2 b \"three\" c\) :test #'integerp\)~%~@
:SEE-ALSO `mon:plist-keys', `mon:plist-values' `mon:plist-eql'
`mon:nplist-to-alist', `mon:plist-to-alist', `mon:plist-delete',
`mon:plist-remove', `mon:plist-get', `mon:plist-put', `mon:putf', `mon:put',
`mon:plist-proper-p', `mon:proper-plist' `cl:get-properties', `cl:get',
`cl:getf', `cl:remf', `cl:remprop', `cl:symbol-plist'.~%▶▶▶")

(fundoc 'plist-removef
        "Modify macro for `mon:plist-remove'.~%~@
:SEE-ALSO `mon:plist-keys', `mon:plist-values' `mon:plist-eql'
`mon:nplist-to-alist', `mon:plist-to-alist', `mon:plist-delete',
`mon:plist-remove', `mon:plist-get', `mon:plist-put', `mon:putf', `mon:put',
`mon:plist-proper-p', `mon:proper-plist' `cl:get-properties', `cl:get',
`cl:getf', `cl:remf', `cl:remprop', `cl:symbol-plist'.~%▶▶▶")

(fundoc 'plist-deletef
"Modify macro for `mon:plist-delete'.~%~@
:SEE-ALSO `mon:plist-keys', `mon:plist-values' `mon:plist-eql' `mon:nplist-to-alist',
`mon:plist-to-alist', `mon:plist-delete', `mon:plist-remove', `mon:plist-get',
`mon:plist-put', `mon:putf', `mon:put', `mon:plist-proper-p', `mon:proper-plist',
`cl:get-properties', `cl:get', `cl:getf', `cl:remf', `cl:remprop',
`cl:symbol-plist'.~%▶▶▶")

(fundoc 'plist-hide
"Hide values of PLIST keys indicated by SECRET-INDICATORS using OBFUSCATE-WITH.~%~@
Arg PLIST is a property list.~%~@
Arg SECRET-INDICATORS is an atom or list of keys assosciated with a value in PLIST.~%~@
Arg OBFUSCATE-WITH is a value to swap for existing plist values keyed by SECRET-INDICATORS.~%~@
:EXAMPLE~%
 \(plist-hide \(list :pass \"secret\" :user \"myid\" :ssid \"1234\" :height \"18\"\) \(list :pass :ssid\) '<hidden>\)
  => \(:PASS <HIDDEN> :USER \"myid\" :SSID <HIDDEN> :HEIGHT \"18\"\)~%
 \(plist-hide \(list :pass \"secret\" :user \"myid\" :ssid \"1234\" :height \"18\"\) :pass :hidden\)
  => \(:PASS :HIDDEN :USER \"myid\" :SSID \"1234\" :HEIGHT \"18\"\)~%~@
:SEE-ALSO `mon:plist-keys', `mon:plist-values' `mon:plist-eql' `mon:nplist-to-alist',
`mon:plist-to-alist', `mon:plist-delete', `mon:plist-remove', `mon:plist-get',
`mon:plist-put', `mon:putf', `mon:put', `mon:plist-proper-p', `mon:proper-plist',
`cl:get-properties', `cl:get', `cl:getf', `cl:remf', `cl:remprop',
`cl:symbol-plist'.~%▶▶▶")


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
