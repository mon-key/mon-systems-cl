;;; :FILE-CREATED <Timestamp: #{2011-03-12T18:11:04-05:00Z}#{11106} - by MON>
;;; :FILE tests/package.lisp
;;; ==============================

;; (in-package #:mon-test) ;; for Slime

(defpackage #:mon-test 
  (:use #:common-lisp #:mon)
  (:import-from #:sb-ext #:call-with-timing)
  (:export
   ;;
 ;; tests/test.lisp
   ;;
   #:*random-chars*
   #:make-random-char-array
   #:make-random-string
   #:make-random-inverted-number-array
   ;;
 ;; tests/timing.lisp
   ;;
   #:with-timing-collected
   #:timing-data-show   
   #:timing-data-clear 
   #:timing-data-compile
   ;;
 ;; tests/testing.lisp
   ;;
   #:pathname-not-wild-empty-or-dotted-p-TEST
   #:mapconcat-TEST))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon-test
;; End:


;;; ==============================
;;; EOF
