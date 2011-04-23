;;; :FILE mon-systems/compose.lisp
;;; ==============================


(in-package #:mon)
;; *package*


;;; ==============================
;;; :COMPOSE-MACROS
;;; ==============================

;; :SOURCE CLOCC/cllib/port/ext.lisp
(defun compose-fun (&rest functions)
  (reduce #'(lambda (f0 f1)
	      (declare (function f0 f1))
	      (lambda (&rest args) (funcall f0 (apply f1 args))))
          functions :initial-value #'identity))

;; :SOURCE CLOCC/cllib/port/ext.lisp
(defun compose-all (&rest functions)
  (reduce #'(lambda (f0 f1)
	      (declare (function f0 f1))
	      (lambda (&rest args)
		(multiple-value-call f0 (apply f1 args))))
          functions :initial-value #'identity))

;;; ==============================
;; :SOURCE CLOCC/cllib/port/ext.lisp
;; (defmacro compose-safe (&rest functions)
;;   (labels ((rec (xx yy)
;;              (let* ((first (first xx)) (rest (rest xx))
;;                     (var (gensym (format nil "~S ~S " 'compose-safe- first))))
;;                (if rest
;;                    `(let ((,var ,(rec rest yy))) (and ,var (,first ,var)))
;;                    `(and ,yy (,first ,yy))))))
;;     ;; #-sbcl ((with-gensyms "-COMPOSE-SAFE-" arg)
;;     (with-gensyms  (compose-safe-) ;;arg)
;;       `(lambda (,arg) ,(rec functions arg)))))
;;
;; (setf (documentation 'compose-safe 'function)
;;       #.(format nil
;;   "Like `compose' but return nil when an intermediate value is nil.~%~@
;; :EXAMPLE~%~%~@
;;  { ... <EXAMPLE> ... } ~%~@
;; :SEE-ALSO `compose-fun', `compose-all'.~%►►►"))
;;
;;; ==============================


;;; ==============================
;;; :COMPOSE-DOCUMENTATION
;;; ==============================

(fundoc 'compose-fun
  "Return the composition of all the arguments.~%~@
All FUNCTIONS should take one argument, except for the last one, which can take
several.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `compose-fun', `compose-all', `%compose'.~%►►►")

(fundoc 'compose-all
 "Return the composition of all the arguments.~%~@
All the values from nth function are fed to the n-1th of FUNCTIONS.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `compose-fun', `compose-all', `%compose'.~%►►►")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
