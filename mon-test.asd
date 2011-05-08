;;; :FILE mon-systems/mon-test.asd
;;; ==============================

;;; ==============================
;;
;; (translate-logical-pathname "MON:MON-SYSTEMS;tests")
;;
;;; ==============================

(defpackage #:mon-test-system (:use :common-lisp :asdf))

(in-package #:mon-test-system)

(defsystem :mon-test
  ;; :name ""
  :author  "MON KEY"
  :maintainer "MON KEY"
  :license  "BSD"
  :description "MON tests."
  :version "1.0.0"
  :pathname  "tests/"
  :serial t
  :depends-on (:mon :rt)
  :components 
  ((:file "package")
   (:file "test")
   (:file "timing")
   (:file "testing")))

(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (asdf:find-system :mon-test))))
  (pushnew :mon-test cl:*features*))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF

