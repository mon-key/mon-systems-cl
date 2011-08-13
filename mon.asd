;;; -*- mode: lisp -*-
;;; :FILE mon-systems/mon.asd
;;; ==============================

;; ,----
;; | "I am sick to death of knee-jerk anti-LOOPism and I am beginning to
;; |  irrationally regard it as a plot to disable me as a programmer by
;; |  excommunicating my useful tools."
;; |
;; |     :SOURCE "Knee-jerk Anti-LOOPism and other E-mail Phenomena" p 17 
;; `---- :SEE http://ccs.mit.edu/papers/CCSWP150.html

;;; ==============================
;;; :LOGICAL-PATHNAMES
;;; ==============================
;;;
;;; (translate-logical-pathname "MON:MON-SYSTEMS;")
;;;
;;; (push (translate-logical-pathname  "MON:MON-SYSTEMS;") asdf:*central-registry*)
;;;
;;; Finding current fasls:
;;; (logical-pathname-translations "FASL")
;;;
;;; (translate-logical-pathname "FASL:MON-SYSTEMS;")
;;;
;;; ==============================
;;;
;;; :NOTE While debugging per file:
;;;
;;; (declaim (optimize (debug 3)))
;;;
;;; (setf sb-debug:*show-entry-point-details* t)
;;;
;;; (setf *break-on-signals* t)
;;; 
;;; Or, (sb-ext:restrict-compiler-policy 'debug 3)
;;;
;;; ==============================
;;;
;;; To remove the compile fasls do:
;;;
;;;  (asdf:clear-system :mon)
;;;
;;; ==============================
;;;
;;; (ql:quickload :mon :verbose t :explain t)
;;;
;;; ==============================
;;;
;;; (let (des) (do-external-symbols (i :mon des) (push i des)))
;;;
;;; ==============================


(defpackage #:mon-build-system (:use :common-lisp :asdf))

(in-package #:mon-build-system)

(defsystem :mon
  ;; :name ""
  :author  "MON KEY"
  :maintainer "MON KEY"
  :license "MIT" 
  :description "MON agglomerated"
  :version "1.0.0"
  :depends-on (:split-sequence
               :string-case
	       :alexandria
	       :cl-ppcre
	       :flexi-streams
	       :ironclad
               :salza2
               :chipz
               :closer-mop
               :cffi
               :osicat
               :cl-fad        ;; :NOTE This is likely to be deprecated in favor of osicat which provides most of same.
               ;; :local-time
	       )
  :serial t    
  :components
  ((:file "package"      ) 
   (:file "specials"     ) 
   (:file "types"        ) 
   (:file "macros"       ) 
   (:file "file-io"      ) 
   (:file "environ"      ) 
   (:file "char-numeric" ) ; :BEFORE chars.lisp!
   (:file "chars"        ) 
   (:file "seqs"         ) 
   (:file "class-utils"  ) 
   (:file "numbers"      ) 
   (:file "plist"        )  
   (:file "alist"        )  
   (:file "hash"         )  
   (:file "strings"      ) 
   (:file "introspect"   ) ; :AFTER strings.lisp seqs.lisp
   (:file "bit-twiddle"  ) 
   (:file "arrays"       ) 
   (:file "file-dir"     ) 
   (:file "io"           ) 
   (:file "chronos"      ) 
   (:file "regexp"       ) 
   (:file "format"       ) 
   (:FILE "image-rotate" )
   (:file "compose"      ) 
   (:file "conditions"   ) 
   (:file "class-doc"    )
   ))

;; This used to work... is something funny with latest SBCL 1.0.47.1?
;; (asdf:perform (asdf:
(defmethod asdf:perform :after ((op asdf:load-op) (system (eql (asdf:find-system :mon))))
  (pushnew :mon cl:*features*)
  (let* ((chk-cons-file 
          #-IS-MON #.((probe-file (merge-pathnames (make-pathname :name "loadtime-bind") 
                                                   (load-time-value *default-pathname-defaults*))))
          #+IS-MON #.(probe-file (translate-logical-pathname "MON:MON-SYSTEMS;loadtime-bind")))
         (chk-lb-file 
          (and chk-cons-file
               (probe-file (merge-pathnames chk-cons-file (make-pathname :type "lisp"))))))
    (and chk-lb-file 
         ;; *load-print* *load-verbose* 
         (load chk-lb-file :verbose t :print t)))
  (asdf:operate 'asdf:load-op 'mon-test))


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; End:

;;; ==============================
;;; EOF

