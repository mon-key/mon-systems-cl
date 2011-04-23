;;; :FILE-CREATED <Timestamp: #{2011-02-12T12:54:10-05:00Z}#{11066} - by MON>
;;; :FILE mon-systems/format.lisp
;;; ==============================


;;; ==============================
;;
;; :NOTE Don't forget that the DESTINATION argument to `cl:format's may be a
;; string so long as that string has a fill-pointer, e.g.:
;;
;;  (let ((dest-string (make-array 16 :element-type 'character :fill-pointer 0))) 
;;    (format dest-string "~a" "my example- to string") dest-string)
;;
;; ,----
;; | `cl:format' sends the output to DESTINATION.
;; | 
;; |  - If DESTINATION is NIL, `cl:format' creates and returns a string
;; |    containing the output from CONTROL-STRING.
;; | 
;; |  - If DESTINATION is non-nil, it must be a string with a fill
;; |    pointer, a stream, or the symbol T.
;; | 
;; |  - If DESTINATION is a string with a fill pointer, the output is
;; |    added to the end of the string.
;; | 
;; |  - If DESTINATION is a stream, the output is sent to that stream.
;; | 
;; | - If DESTINATION is T, the output is sent to standard output.
;; `---- 
;; :SEE (info "(ansicl)Formatted Output")
;;
;; However, there is the proviso in "exceptional situations" of the spec:
;;
;; ,----
;; | If DESTINATION is a string with a fill pointer, the consequences are
;; | undefined if destructive modifications are performed directly on the
;; | string during the dynamic extent of the call.
;; `---- 
;; :SEE (info "(ansicl)format")
;;
;; Also, note that:
;;
;; (let ((dest-string (make-array 16 :element-type 'character :fill-pointer 0)))
;;   (list (typep dest-string 'string-stream) (typep dest-string 'stream)) 
;;  => = (nil nil)
;;
;; And that:
;; (read string-with-fill-pointer) 
;; => error
;; 
;; And that: 
;; (write 'x string-with-fill-pointer)
;; => error
;; 
;; (let ((fstr (make-array '(0) :element-type 'character :fill-pointer 0 :adjustable t)))
;;  (with-output-to-string (s fstr)
;;    (format s "Is S string-streamp: ~S~%" (typep s 'string-stream))
;;    (format s "Is FSTR string-streamp: ~S" (typep fstr 'string-stream))
;;    fstr))
;;
;;
;; And then there's this:
;; ,----
;; | Fill pointer STRING-OUTPUT-STREAMs are not explicitly mentioned in the CLM,
;; | but they are required for the implementation of WITH-OUTPUT-TO-STRING.
;; |
;; | :SOURCE SBCL's "fill-pointer streams" section of 
;; | :FILE sbcl/src/code/stream.lisp
;; `----
;;
;;; ==============================


(in-package #:mon)
;; *package*


;; :TODO Add support for prepending "Correlative conjunctions":
;; :COURTESY Stas Boukarev #lisp 2011-02-12T14:49 :WAS `format-englishy-list'
(defun format-delimited-english-list (directive conjunction &key (delimiter ",") no-list)
  (let ((delim (etypecase delimiter
                 (boolean ",")
                 (character (or (and (char= delimiter #\~) "~~")
                                (string delimiter)))
                 (string
                  (case (length delimiter)
                    (0 ",")
                    ((1 2)  (or 
                             (and (= (length delimiter) 1)
                                  (and (char= (char delimiter 0) #\~)
                                       (concatenate 'string "~" delimiter)))
                             ;; Punting, sans a regexp or equiv. state machine its
                             ;; hard to account for " ~" "~ " while ignoring "~~".
                             (and (every #'(lambda (dlm) (char= dlm #\SPACE)) delimiter) " ")
                             delimiter))
                    (t  (string-right-trim '(#\SPACE) delimiter))))))
        (conj (case (length conjunction)
                (0 " ")
                (1 (or 
                    (and (char= (char conjunction 0) #\~) "~~")
                    conjunction))
                (t (or 
                    (and (every #'(lambda (dlm) (char= dlm #\SPACE)) conjunction) conjunction)
                    (string-right-trim '(#\SPACE) conjunction))))))
    (concatenate 'string 
                 "~" (if no-list "@" "") 
                 "{~#[~;" directive "~;" directive " " conj " " directive
                 "~:;~@{" directive "~#[~;" delim " " conj " ~:;" delim " ~]~}~]~}")))

;; <stassats>  and me being a fan of format, i'd do it:
;; (format nil "~~~2@*~@[@~]{~~#[~~;~@*~a~~;~@*~a ~1@*~a~@*~a~~:;~~@{~@*~a~~#[~~;, ~1@*~a ~~:;, ~~]~~}~~]~~}"
;;         directive conjunction list)

;; Stolen/adapted from PCL chap 18 pg 229 :WAS `*english-list*'
;; (URL `http://www.gigamonkeys.com/book/a-few-format-recipes.html')
;; "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}"
;;   ^      ^   ^ ^^^  ^       ^     ^ ^^^    ^
(defvar *format-delimited-english-list-templates*
  (mapcar #'(lambda (list)
              (destructuring-bind (nm fmt cnj dlm lst)
                  list
                (cons nm (format-delimited-english-list fmt cnj :delimiter dlm :no-list lst))))
          `((~A-and-w-list    "~A"  "and" nil nil)
            (~A-or-w-list     "~A"  "or"  nil nil)
            (~A-and-no-list   "~A"  "and" nil t)
            (~A-or-no-list    "~A"  "or"  nil t)
            (~S-and-w-list    "~S"  "and" nil nil) 
            (~S-or-w-list     "~S"  "or"  nil nil)
            (~S-and-no-list   "~S"  "and" nil t)
            (~S-or-no-list    "~S"  "or"  nil t)
            (~W-and-w-list    "~W"  "and" nil nil)
            (~W-or-w-list     "~W"  "or"  nil nil)
            (~W-and-no-list   "~W"  "and" nil t)
            (~W-or-no-list    "~W"  "or"  nil t)  
            (~@W-and-w-list   "~@W" "and" nil nil)
            (~@W-or-w-list    "~@W" "or"  nil nil)
            (~@W-and-no-list  "~@W" "and" nil t)  
            (~@W-or-no-list   "~@W" "or"  nil t)
            (~\:W-and-w-list  "~:W" "and" nil nil)
            (~\:W-or-w-list   "~:W" "or"  nil nil)
            (~\:W-and-no-list "~:W" "and" nil t)  
            (~\:W-or-no-list  "~:W" "or"  nil t)
            (~2f-and-w-list   "~,2f" "and" nil nil)
            (~2f-or-w-list    "~,2f" "or"  nil nil)
            (~2f-and-no-list  "~,2f" "and" nil t)
            (~2f-or-no-list   "~,2f" "or"  nil t))))

;; (format nil (cdr (assoc '~2f-or-no-list *format-delimited-english-list-templates*)) 3 .5 3.3333)
;; (format nil (cdr (assoc '~S-AND-NO-LIST *format-delimited-english-list-templates*)) "A" "B" "Q")

(defun format-emit-tab (column &optional (stream nil))
  (declare ((integer 0 80) column))
  (funcall (formatter "~V,0t") stream column)
  (values))

;;; ==============================
;; :SOURCE (URL `http://groups.google.com/group/comp.lang.lisp/msg/f519ba59df92c958')
;; :COURTESY Thomas A. Russ comp.lang.lisp
;; :DATE 2011-03-24T12:39:43
;; :SUBJECT Re: FORMAT question
;; :WAS `format-n-items'
(defun %format-list-items (n directive)
  (with-output-to-string (s)
    (loop 
       repeat n
       as separator = "" then "~^, "
       do (write-string separator s)
       (write-string directive s))
    (write-string "~%" s)))
;;
;; :WAS `format-list-by-n'
(defun format-list-items-by-n (stream prefix-str by-n format-directive list)
  (declare (type string prefix-str)
           (type list list)
           ((integer 0 *) by-n))
  (format stream  "~A ~D:~%~{~:}" prefix-str by-n 
          (%format-list-items by-n format-directive) list))

;;; ==============================
;;; :FORMAT-DOCUMENTATION
;;; ==============================

(vardoc '*format-delimited-english-list-templates*
"Alist of control-strings to generate gramatically delimited lists.~%~@
:EXAMPLE~%
 \(format nil \(cdr \(assoc '~~S-or-no-list *format-delimited-english-list-templates*\)\)
         1 2 3 4\)~%~@
:NOTE Stolen/adapted from PCL chap 18 pg 229.~%
:SEE \(URL `http://www.gigamonkeys.com/book/a-few-format-recipes.html'\)
:SEE-ALSO `mon:format-delimited-english-list'.~%►►►")

(fundoc 'format-list-items-by-n
"Format elts of LIST with FORMAT-DIRECTIVE grouping output BY-N.~%~@
STREAM is as an output stream per `cl:format'.~%~@
PREFIX-STR is a string to prepend to output of preceding the formatted LIST.~%~@
BY-N is an integer value to group elts of LIST by.~%~@
FORMAT-DIRECTIVE is a format directive to format elts of LIST with.~%~@
:EXAMPLE~%~@
 \(format-list-items-by-n t \"list grouped by\" 8 \"~~S\"
                         \(loop for i from 0 to 10 collect \(expt 1.065 i\)\)\)
 \(format-list-items-by-n t 8 \"~~,2F\"
                         \(loop for i from 0 to 25 collect \(expt 1.065 i\)\)\)
:SEE-ALSO `mon::%format-list-items'.~%►►►")

(fundoc '%format-list-items
        "Helper function for `mon:format-list-items-by-n'.~%~@
Return directive as a format-directive repeated N times.~%~@
:EXAMPLE~%
 \(%format-list-items 8 \"~~S\"\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'format-delimited-english-list
"Return a format control string for presenting enumerated lists grammatically.~%~@
DIRECTIVE is a format directive to be inserted at appropriate places in returned
format control-string.~%~@
CONJUNCTION is a coordinating conjunctive, a string of one or more words, e.g.:~%
  \"and\" \"or\" \"nor\" \"as well as\" \"and also\"~%~@
If CONJUNCTION is suffixed by trailing whitespace it is removed as if by
`cl:string-right-trim'.~%~@
Keyword arg :DELIMITER is a character or string to delimit by. 
If it is a string suffixed by trailing whitespace it is removed as if by
`cl:string-right-trim'~%~@
Keyword arg :NO-LIST when non-nil returned control-string is prefixed by \"~~@{\"
such that ARGS to `cl:format' may be a consumed as multiple arguments.
Default is \"~~{\".~%~@
:EXAMPLE~%
 \(format-delimited-english-list \"~~,2f\" \"and\"\)
 ;=> \"~~{~~#[~~;~~,2f~~;~~,2f and ~~,2f~~:;~~@{~~,2f~~#[~~;,and ~~:;, ~~]~~}~~]~~}\"~%
 \(format-delimited-english-list \"~~A\" \"~~%or \"\)
 ;=> \"~~{~~#[~~;~~A~~;~~A ~~%or ~~A~~:;~~@{~~A~~#[~~;,~~%or ~~:;, ~~]~~}~~]~~}\"~%
 \(format t \(format-delimited-english-list \"~~,2f\" \"and\"\) '\(1.5 2.5 3.5\)\)
 ;=> 1.50, 2.50, and 3.50~%
 \(format nil \(format-delimited-english-list \"~~A\" \"and also\"\) '\(1 2\)\)~%
 \(format nil \(format-delimited-english-list \"~~A\" \"and\" :no-list t\) 1 2\)~%
 \(format nil \(format-delimited-english-list \"~~A\" \"and\" :delimiter \"~~%\" :no-list t\) 1 2\)~%
 \(format nil \(format-delimited-english-list \"~~A\" \"and\" :delimiter \"~~%\" :no-list t\) 1 2 3\)~%
 \(format nil \(format-delimited-english-list \"~~A\" \"~~%or \"\) '\(1 2\)\)~%
 \(format nil \(format-delimited-english-list \"~~A\" \"  \"\) '\(1 2\)\)~%
 \(format nil \(format-delimited-english-list \"~~A\" \"  \"\) '\(1 2 3\)\)~%
 \(format nil \(format-delimited-english-list \"~~A\" \"or\" :delimiter #\\~~\) '\(1 2 3\)\)~%
 \(format nil \(format-delimited-english-list \"~~A\" \"and\" :delimiter \"~~%\"\) '\(1 2 3\)\)~%
 \(format nil \(format-delimited-english-list \"~~S\" \"or\" :delimiter #\\; :no-list t\)
         \"bubba\" \"Bubba\" \"buBBa\"\)~%
 \(format nil \(format-delimited-english-list \"~~S\" \"or\" :delimiter #\\;\)
         '\(\"bubba\" \"Bubba\" \"buBBa\"\)\)~%~@
:SEE (URL `http://www.gigamonkeys.com/book/a-few-format-recipes.html')~%
:SEE-ALSO `mon:*format-delimited-english-list-templates*'.~%►►►")

(fundoc 'format-emit-tab 
"Emit a tab to COLUMN as if by \(formatter \"~~V,0t\"\).~%~@
Column is a positivie integer value 0,80.~%~@
:EXAMPLE~%
 \(progn \(format-emit-tab 18 t\) \(format t \"str\"\)\)~%~@
:SEE-ALSO `cl:formatter'.~%►►►")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
