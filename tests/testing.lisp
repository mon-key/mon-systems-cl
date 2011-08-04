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


(defun pathname-not-wild-empty-or-dotted-p-TEST (paths-and-results)
  (flet ((get-test-result (path-and-result)
           (destructuring-bind (pth result) path-and-result
             (let ((chk-rslt (equal (multiple-value-list (pathname-not-wild-empty-or-dotted-p pth)) result)))
               (values  chk-rslt pth result)))))
    (loop 
       for p-and-r in paths-and-results
       collect (multiple-value-bind (pass-or-fail w-path expecting)  (get-test-result p-and-r)
                 (list :PASSED pass-or-fail :WITH-PATH w-path :RESULT-EXPECT expecting)))))
#+nil
(pathname-not-wild-empty-or-dotted-p-TEST
 `(("/some/valid-path/designator"   (T (:STRING "/some/valid-path/designator")))
   (#P"/some/valid-path/designator" (T (:PATHNAME #P"/some/valid-path/designator")))
   (""     (NIL (:PATHNAME-EMPTY #P"")))
   (#P""   (NIL (:PATHNAME-EMPTY #P"")))
   (#P"*"  (NIL (:WILD #P"*")))
   ("*.*"  (NIL (:WILD #P"*.*")))
   ("."    (NIL (:STRING-DOTTED ".")))
   (".."   (NIL (:STRING-DOTTED "..")))
   (#P"."  (NIL (:PATHNAME-DOTTED #P".")))
   (#P".." (NIL (:PATHNAME-DOTTED #P"..")))
   (,(pathname (concatenate 'string *whitespace-chars*))
     (NIL (:PATHNAME-WHITESPACE ,(pathname (concatenate 'string *whitespace-chars*)))))
   (,(concatenate 'string *whitespace-chars*)    
     (NIL (:STRING-WHITESPACE ,(concatenate 'string *whitespace-chars*))))))

#+nil
(equal (map 'list  #'cadr 
            (pathname-not-wild-empty-or-dotted-p-TEST
             `(("/some/valid-path/designator"   (T (:STRING "/some/valid-path/designator")))
               (#P"/some/valid-path/designator" (T (:PATHNAME #P"/some/valid-path/designator")))
               (""     (NIL (:PATHNAME-EMPTY #P"")))
               (#P""   (NIL (:PATHNAME-EMPTY #P"")))
               (#P"*"  (NIL (:WILD #P"*")))
               ("*.*"  (NIL (:WILD #P"*.*")))
               ("."    (NIL (:STRING-DOTTED ".")))
               (".."   (NIL (:STRING-DOTTED "..")))
               (#P"."  (NIL (:PATHNAME-DOTTED #P".")))
               (#P".." (NIL (:PATHNAME-DOTTED #P"..")))
               (,(pathname (concatenate 'string *whitespace-chars*))
                 (NIL (:PATHNAME-WHITESPACE ,(pathname (concatenate 'string *whitespace-chars*)))))
               (,(concatenate 'string *whitespace-chars*)    
                 (NIL (:STRING-WHITESPACE ,(concatenate 'string *whitespace-chars*)))))))
       '(T T T T T T T T T T T T))

(sb-rt:deftest each-a-string-or-null-p-TEST
 (values 
  (mon:each-a-string-or-null-p '("a" "b" "c"))
  (mon:each-a-string-or-null-p '(nil nil nil))
  (mon:each-a-string-or-null-p '("a" nil "b" nil))
  (mon:each-a-string-or-null-p nil)
  (mon:each-a-string-or-null-p 8)
  (mon:each-a-string-or-null-p '("a" . "b"))
  (mon:each-a-string-or-null-p '("a" "b" . "c")))
 T T T NIL NIL NIL NIL)
;;
;; (sb-rt:do-test  'each-a-string-or-null-p-TEST)

;; testing `mon:pathname-or-namestring-not-empty-dotted-or-wild-p'
(sb-rt:deftest pathname-or-namestring-not-empty-dotted-or-wild-p-TEST
    (values 
     (let ((possibilities '("" "." ".." " "  #P"" #P"." #P".." #P" " 
                            "*" "*." "*.*" #P"*" #P"*." #P"*.*"
                            "./" "../" #P"./" #P"../")))
       (list 
        (map 'list #'mon::pathname-or-namestring-not-empty-dotted-or-wild-p possibilities)
        (map 'list #'(lambda (x) 
                       (mon::pathname-or-namestring-not-empty-dotted-or-wild-p x :no-relatives t))
             possibilities)))
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p "")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p ".")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p "..")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p " ")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p #P"")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p #P".")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p #P"..")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p #P" ")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p "*")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p "*.")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p "*.*")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p #P"*")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p #P"*.")
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p #P"*.*")
     ;;
     (not (mon::pathname-or-namestring-not-empty-dotted-or-wild-p "./"))
     (not (mon::pathname-or-namestring-not-empty-dotted-or-wild-p "../"))
     (not (mon::pathname-or-namestring-not-empty-dotted-or-wild-p #P"./"))
     (not (mon::pathname-or-namestring-not-empty-dotted-or-wild-p #P"../"))
     ;;
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p "./"    :no-relatives t)
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p "../"   :no-relatives t)
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p #P"./"  :no-relatives t)
     (mon::pathname-or-namestring-not-empty-dotted-or-wild-p #P"../" :no-relatives t))
  ;;
   ((NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL T T T T)
    (NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL))
   NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
;;
;; (sb-rt:do-test 'pathname-or-namestring-not-empty-dotted-or-wild-p-TEST)

;; testing `mon:concat'
(sb-rt:deftest concat-TEST
    (values 
     (mon:concat "a" "b" "c")
     (mon:concat #(#\a #\b #\c) "a")
     (mon:concat "a" #(#\a) "b")
     (mon:concat "a" '(#\a) "b")
     (mon:concat "a" #\a "b" #\b)
     (mon:concat  nil)
     (mon:concat "a" nil "b" nil)
     (mon:concat "a" nil #(#\b) nil)
     (mon:concat "a" nil #() nil)
     (mon:concat nil #() ()))
  "abc" "abca" "aab" "aab" "aabb" "" "ab" "ab" "a" "")
;;
;; (sb-rt:do-test 'concat-TEST)

;;; ==============================


;; :TEST `mon:string-split-on-chars'
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
;;
;; (sb-rt:do-test 'string-split-on-chars-TEST)
;; (sb-rt:do-tests)

;;; ==============================
;; :SOURCE PJB common-lisp/cesarum/list.lisp :WAS `test/list-lengths' :LICENSE GPL
;; (defun list-lengths-TEST ()
;;   (dolist (test
;;             '(;; atom
;;               (a nil symbol)
;;               ;; proper lists
;;               (()  0 0)
;;               ((a)  1 0)
;;               ((a b)  2 0)
;;               ((a b c)  3 0)
;;               ((a b c d)  4 0)
;;               ((a b c d e)  5 0)
;;               ;; dotted lists
;;               ((a . b)  1 nil)
;;               ((a b . c) 2 nil)
;;               ((a b c . d) 3 nil)
;;               ((a b c d . e) 4 nil)
;;               ((a b c d e . f) 5 nil)
;;               ;; circular lists
;;               (#1=(a . #1#) 0 1)
;;               (#2=(a b . #2#) 0 2)
;;               (#3=(a b c . #3#) 0 3)
;;               (#4=(a b c d . #4#) 0 4)
;;               (#5=(a b c d e . #5#) 0 5)
;;               ((a . #6=(b . #6#)) 1 1)
;;               ((a . #7=(b c . #7#)) 1 2)
;;               ((a . #8=(b c d . #8#)) 1 3)
;;               ((a . #9=(b c d e . #9#)) 1 4)
;;               ((a b . #10=(c . #10#)) 2 1)
;;               ((a b . #11=(c d . #11#)) 2 2)
;;               ((a b . #12=(c d e . #12#)) 2 3)
;;               ((a b c . #13=(d . #13#)) 3 1)
;;               ((a b c . #14=(d e . #14#)) 3 2)
;;               ((a b c d . #15=(e . #15#)) 4 1)
;;               ((a b c d e . #16=(#16#)) 6 6) ; a proper list! :-)
;;               )
;;            :success)
;;     (destructuring-bind (list . expected) test
;;       (let ((result  (multiple-value-list (list-lengths list)))
;;             (*print-circle* t))
;;         (assert (equal expected result)
;;                 (result)
;;                 "(list-lengths '~S)~%  returned ~S~%  expected ~S~%"
;;                 list result expected)))))

;;; ==============================
;; :NOTE When evaluating (esp. in Slime which frobs value of *standard-output*
;; and can blow the stack when printing circular objects) following should be
;; evaluated with `sb-rt::*print-circle-on-failure*' dynamically bound t e.g.:
;;
;;  (let ((sb-rt::*print-circle-on-failure* t))
;;   (sb-rt:do-test  'list-lengths-TEST))
;;
(sb-rt:deftest list-lengths-TEST
    (values
     ;; :ATOM
     (multiple-value-list (mon:list-lengths 'a))
     ;; :PROPER-LISTS
     (multiple-value-list (mon:list-lengths ()))
     (multiple-value-list (mon:list-lengths '(a)))
     (multiple-value-list (mon:list-lengths '(a b)))
     (multiple-value-list (mon:list-lengths '(a b c)))
     (multiple-value-list (mon:list-lengths '(a b c d)))
     (multiple-value-list (mon:list-lengths '(a b c d e)))
     ;; :DOTTED-LISTS
     (multiple-value-list (mon:list-lengths '(a . b)))
     (multiple-value-list (mon:list-lengths '(a b . c)))
     (multiple-value-list (mon:list-lengths '(a b c . d)))
     (multiple-value-list (mon:list-lengths '(a b c d . e)))
     (multiple-value-list (mon:list-lengths '(a b c d e . f)))
     ;; :CIRCULAR-LISTS
     (multiple-value-list (mon:list-lengths '#1=(a . #1#)))
     (multiple-value-list (mon:list-lengths '#2=(a b . #2#)))
     (multiple-value-list (mon:list-lengths '#3=(a b c . #3#)))
     (multiple-value-list (mon:list-lengths '#4=(a b c d . #4#)))
     (multiple-value-list (mon:list-lengths '#5=(a b c d e . #5#)))
     (multiple-value-list (mon:list-lengths '(a . #6=(b . #6#))))
     (multiple-value-list (mon:list-lengths '(a . #7=(b c . #7#))))
     (multiple-value-list (mon:list-lengths '(a . #8=(b c d . #8#))))
     (multiple-value-list (mon:list-lengths '(a . #9=(b c d e . #9#))))
     (multiple-value-list (mon:list-lengths '(a b . #10=(c . #10#))))
     (multiple-value-list (mon:list-lengths '(a b . #11=(c d . #11#))))
     (multiple-value-list (mon:list-lengths '(a b . #12=(c d e . #12#))))
     (multiple-value-list (mon:list-lengths '(a b c . #13=(d . #13#))))
     (multiple-value-list (mon:list-lengths '(a b c . #14=(d e . #14#))))
     (multiple-value-list (mon:list-lengths '(a b c d . #15=(e . #15#))))
     (multiple-value-list (mon:list-lengths '(a b c d e . #16=(#16#)))))
  (nil symbol) (0 0) (1 0) (2 0) (3 0) (4 0) (5 0) (1 nil) (2 nil) (3 nil) 
  (4 nil) (5 nil) (0 1) (0 2) (0 3) (0 4) (0 5) (1 1) (1 2) (1 3) (1 4) (2 1) (2 2)
  (2 3) (3 1) (3 2) (4 1) (6 0))


    
;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon-test
;; End:

;;; ==============================
;;; EOF
