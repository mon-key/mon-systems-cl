;;; :FILE-CREATED <Timestamp: #{2011-04-04T19:30:54-04:00Z}#{11141} - by MON>
;;; :FILE mon-systems/char-numeric.lisp
;;; ==============================

;;; ==============================
;; :NOTE This file exists for the sole purpose of segregating `char-numeric='
;; from the rest of the system. We do this b/c on SBCL `%char-numeric=' is
;; defined with significant optimizations and if any portion of this file is
;; changed we will get a restart at compile-time. IOW unless specifically
;; editing `%char-numeric=' do your edits elswhere!
;;; ==============================


(in-package #:mon)
;; *package*


;;; ==============================
;; SOURCE Paul Khuong's string-case/string-case.lisp :WAS `numeric-char='
#-(and sbcl (or x86 x86-64))
(declaim (inline %char-numeric=)
         (ftype (function (character character)
                          (values (and unsigned-byte fixnum)))
                %char-numeric=))
(defun %char-numeric= (x y)
  (declare (type character x y))
  (logxor (char-code x)
          (char-code y)))

#+(and sbcl (or x86 x86-64))
(progn
  (SB-C:DEFKNOWN %char-numeric= (character character)
      (unsigned-byte #. (1- SB-VM:N-MACHINE-WORD-BITS))
      (SB-C:MOVABLE SB-C:FOLDABLE SB-ASSEM:FLUSHABLE SB-C::EXPLICIT-CHECK))

  (SB-C:DEFINE-VOP (%char-numeric=)
    (:args (x :scs (SB-VM::CHARACTER-REG SB-VM::CHARACTER-STACK)
              :target r
              :load-if (not (location= x r))))
    (:info y)
    (:arg-types (:constant character) character)
    (:results (r :scs (SB-VM::UNSIGNED-REG)
                 :load-if (not (SB-C:LOCATION= x r))))
    (:result-types SB-VM::UNSIGNED-NUM)
    (:translate %char-numeric=)
    (:policy :fast-safe)
    (:note "inline constant %char-numeric=")
    (:generator 1
       (SB-C:MOVE r x)
       (SB-VM::INST SB-VM::XOR r (char-code y)))))

;; (defmacro tt--char-numeric= (char-x char-y)
;;   ;; (tt--char-numeric= 9658 9658)
;;   ;; (tt--char-numeric= 9658 #\►)
;;   ;; (tt--char-numeric= #\◄ #\►)
;;   ;; (tt--char-numeric= 9657 #\►)
;;   ;; (tt--char-numeric= 9657 9658)
;;   ;; (tt--char-numeric= 9657 9658)
;;   `(values-list 
;;     ,`(etypecase ,char-x 
;;         (character 
;;          (etypecase ,char-y
;;            (character (or (and (zerop (%char-numeric= ,char-x  ,char-y)) 
;;                                (list t ,char-x))
;;                           (list nil ,char-x ,char-y)))
;;            (char-code-integer (or (and (zerop (%char-numeric= ,char-x (code-char ,char-y)))
;;                                        (list t ,char-x))
;;                                   (list nil ,char-x (code-char ,char-y))))))
;;         (char-code-integer
;;          (etypecase ,char-y
;;            (char-code-integer (or (and (zerop (logxor ,char-x ,char-y))
;;                                        (list t (code-char ,char-x)))
;;                                   (list nil (code-char ,char-x) (code-char ,char-y))))
;;            (character (or (and (zerop (logxor ,char-x (char-code ,char-y)))
;;                                (list t ,char-y))
;;                           (list nil (code-char ,char-x) ,char-y))))))))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
