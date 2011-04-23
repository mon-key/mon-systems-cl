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

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon-test
;; End:

;;; ==============================
;;; EOF
