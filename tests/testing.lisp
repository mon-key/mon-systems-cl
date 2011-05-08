;;; :FILE-CREATED <Timestamp: #{2011-03-12T14:41:07-05:00Z}#{11106} - by MON>
;;; :FILE tests/testing.lisp
;;; ==============================


(in-package #:mon-test)
;; *package*

;;; ==============================
;;
;; (values (mapconcat (lambda (x) (and x (string-downcase x))) 
;;                        '("one" two three nil "five") "-")
;;         (mapconcat (lambda (x) (and x (string-downcase x))) 
;;                                '("one" two three nil "five") "-"))

(defun mapconcat-TEST ()
  (loop :for (expression expected)
     :in '(((mapconcat (lambda (x) (and x (string-downcase x))) '("one" two three nil "five") "-")
            "one-two-three--five")
           ((mapconcat (lambda (x) (and x (string-downcase x))) '("one") "-")
            "one")
           ((mapconcat (lambda (x) (and x (string-downcase x))) '(nil) "-")
            "")
           ((mapconcat (lambda (x) (and x (string-downcase x))) '() "-")
            "")
           ((mapconcat (lambda (x) (and x (string-downcase x))) #("one" two three nil "five") "-")
            "one-two-three--five")
           ((mapconcat (lambda (x) (and x (string-downcase x))) #("one") "-")
            "one")
           ((mapconcat (lambda (x) (and x (string-downcase x))) #(nil) "-")
            "")
           ((mapconcat (lambda (x) (and x (string-downcase x))) #() "-")
            ""))
     :do (assert (equal (eval expression) expected)
                 ()
                 "~%Expression: ~S~%Expected: ~S~%Got: ~S~%"
                 expression expected (eval expression)))
  :success)
;;
;; (mapconcat-TEST)

(sb-rt:deftest string-split-on-chars-TEST
    (values 
     (mon:string-split-on-chars "bub ba	bubba")
     (mon:string-split-on-chars "bubba	bubba" "b")
     (mon:string-split-on-chars "bubba" #\b)
     (mon:string-split-on-chars " b u bba " 32)
     (equal (mon:string-split-on-chars (format nil "~{~C~}" mon:*whitespace-chars*))
            (format nil "~{~C~}" mon:*whitespace-chars*))
     (mon:string-split-on-chars (format nil "~{~C~}" mon:*whitespace-chars*) nil t))
  ("bub" "ba" "bubba")
  ("" "u" "" "a	" "u" "" "a")
  ("" "u" "" "a")
  ("" "b" "u" "bba")
  t
  ("" "" "" "" "" "" ""))
;; sb-rt:assert-
;; (rt:rem-all-tests)
;; sb-rt:*test*
;; (sb-rt:do-test)
;; (sb-rt:do-tests) 

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon-test
;; End:

;;; ==============================
;;; EOF
