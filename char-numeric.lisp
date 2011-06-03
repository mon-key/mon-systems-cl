;;; :FILE-CREATED <Timestamp: #{2011-04-04T19:30:54-04:00Z}#{11141} - by MON>
;;; :FILE mon-systems/char-numeric.lisp
;;; ==============================

;;; ==============================
;; :NOTE This file exists for the sole purpose of segregating `char-numeric='
;; from the rest of the system. We do this b/c on SBCL `%char-numeric=' is
;; defined with significant optimizations and if any portion of this file is
;; changed we will get a restart at compile-time. IOW unless specifically
;; editing `%char-numeric=' do your edits elswhere! 
;; See notes below.
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
              :load-if (not (SB-C:LOCATION= x r))))
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


#|
:SEE (URL `http://paste.lisp.org/display/122345')

I ripped numeric= out of string-case.lisp along with its defknown and
define-vop. I'm sure i'm _really_ missing the point... but if i then inline
my (renamed) %numeric= in a separate file I get compiler warnings about
overwriting old fun info. Is this to be expected?

With File A char-numeric.lisp appearing before File B char.lisp in my mon.asd
I ql:quickload the system:

> (ql:quickload 'mon)

No warnings (at least related to %NUMERIC=)

I muck around...

Later I either do another ql:quickload of mon and or ql:quickload a
system which depends on mon

> (ql:quickload 'mon) |  (ql:quickload 'some-other-sys-with-mon-depends)

overwriting old FUN-INFO
  #<SB-C::FUN-INFO
    :ATTRIBUTES (SB-C:FOLDABLE SB-ASSEM:FLUSHABLE
                 SB-C:UNSAFELY-FLUSHABLE SB-C:MOVABLE
                 SB-C::EXPLICIT-CHECK)
    :TEMPLATES (#)>
for MON::%CHAR-NUMERIC=
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [CONTINUE] Go ahead, overwrite it.
 1: [TRY-RECOMPILING] Recompile char-numeric and try loading it again
 2: [RETRY] Retry loading component ("mon" "char-numeric").
 3: [ACCEPT] Continue, treating loading component ("mon" "char-numeric") as having been successful.
 4: [ABORT] Give up on "mon"
 5: [*ABORT] Return to SLIME's top level.
 --more--

<pkhuong> mon_key: reloading the file containing the defknown will
	  warn. You can ignore it.

<mon_key> pkhuong: I don't think its the inline (or at least not just). I get
	  the overwritiong old FUN-INFO restart if I delete old fasls from
	  ~/.cache/big-long-path-to-fasl/char-numeric.fasl before the next
	  ql:quickload  [19:29]
<pkhuong> that would do it.  [19:30]
<pkhuong> but you don't want to inline.
<mon_key> Ok. the inline is gone. Out of curiousity why does zapping the fasl
	  trigger the restart?  [19:31]
<pkhuong> it recompiles the file.  [19:32]
<mon_key> wich then causes the compiler to overwrite something in core?
								        [19:33]
<pkhuong> just reloading the fasl will re-execute that defknown  [19:35]
<pkhuong> and that overwrites the old one.
<mon_key> I think what i'm asking is what is it (if anything) about the goo in
	  sb-c:fun-info that is different from other goo?  [19:37]
<pkhuong> I don't understand the question.  [19:38]
<mon_key> Sorry. None of the other fasls i zap in ~/.cache/.../*.fasl trigger
	  a similar restart. So I assume that the one i am seeing w/r/t
	  numeric= has to do with the sb-c:fun-info structure. I'm curious
	  about its interaction with numeric=.  [19:41]
<pkhuong> no, it's just that it expands into a function call that errors when
	  it's called multiple times with the same name.  [19:42]
<mon_key> pkhuong: Ok. I was missing the expansion around %defknown. Thanks
	  for helping me to better understand what I was seeing.

IOW SB-C:DEFKNOWN is a wrapper around SB-C::%DEFKNOWN
It is the expansion of the SB-C::%DEFKNOWN that triggers the restart.

|#

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
