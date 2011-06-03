;;; :FILE mon-systems/regexp.lisp
;;; ==============================


(in-package #:mon)
;; *package*

;; :COURTESY cl-etsy/base.lisp :WAS `underscore-to-dash'
(defun string-underscore-to-dash (string)
  ;; :WAS (intern (nstring-upcase (cl-ppcre:regex-replace-all "_" string "-"))))
  (declare (type string string))
  (or (and (string-empty-p string)
           (values string nil))
      (cl-ppcre:regex-replace-all "_" string "-")))

;; (typep #\Tab '(not (member *whitespace-chars*)))

(defun string-whitespace-to-char (target-string replacement-char &key (convert-all-whitespace nil))
  (declare (string target-string)
           (boolean convert-all-whitespace)
           (char-or-char-code-integer-or-string-1 replacement-char)
           (optimize (speed 3)))
  (let* ((char-nrmlz (char-or-char-code-integer-or-string-1-ensure-char replacement-char))
         (char-wspc-bail-if
          (if (char-not-whitespace-char-p char-nrmlz)
              (the character char-nrmlz)
              (simple-error-mon  :w-sym "string-whitespace-to-char"
                                 :w-type 'function
                                 :w-spec "arg REPLACEMENT-CHAR did not satsify `mon:char-not-whitespace-char-p'"
                                 :w-got replacement-char
                                 :w-type-of t
                                 :signal-or-only nil)))
         (char-nmrl-str (string char-wspc-bail-if))
         (wspc-scnr
          (cl-ppcre:create-scanner
           #.(concatenate 'string "[" (coerce '(#\  #\Newline #\Tab #\Return #\NO-BREAK_SPACE #\Page #\Vt) 'string)  "]+?"))))
    (when (string-empty-p target-string)
      (return-from string-whitespace-to-char (values target-string nil :string-empty)))
    (locally 
        (declare (string-not-empty target-string))
      (if (string-all-whitespace-p target-string)
          (if convert-all-whitespace
              (multiple-value-bind (repl bool) (cl-ppcre:regex-replace-all wspc-scnr target-string char-nmrl-str)
                (values repl bool :string-all-whitespace))
              (values target-string nil :string-all-whitespace))
          (multiple-value-bind (repl bool) (cl-ppcre:regex-replace-all wspc-scnr target-string char-nmrl-str)
            (values repl bool target-string))))))

(defun string-whitespace-to-dash (target-string &key (pre-trim t) (convert-all-whitespace nil))
  (declare (string target-string)
           (boolean pre-trim convert-all-whitespace)
           (optimize (speed 3)))
  (if pre-trim
      (flet ((pretrim-for-string-whitespace-to-foo (pre-trim-target-string)
               (declare (string pre-trim-target-string))
               (when (string-empty-p pre-trim-target-string)
                 (return-from string-whitespace-to-dash (values pre-trim-target-string nil :string-empty)))
               (let ((pre (string-trim-whitespace pre-trim-target-string)))
                 (if (string-empty-p pre)
                     (if convert-all-whitespace
                         pre-trim-target-string
                         (if pre-trim ;; 
                             (return-from string-whitespace-to-dash (values (make-string 0 :initial-element #\nul) nil 
                                                                            :string-empty-or-all-whitespace))
                             ;; can't actually get here.
                             (return-from string-whitespace-to-dash (values pre-trim-target-string nil :string-all-whitespace))))
                     pre))))
        (let ((pre-post (pretrim-for-string-whitespace-to-foo target-string)))
          (declare (string-not-empty pre-post))
          (string-whitespace-to-char pre-post #\- :convert-all-whitespace convert-all-whitespace)))
      (string-whitespace-to-char target-string #\- :convert-all-whitespace convert-all-whitespace)))


(defun string-whitespace-to-underscore (target-string &key (pre-trim t) (convert-all-whitespace nil))
  (declare (string target-string)
           (boolean pre-trim convert-all-whitespace)
           (optimize (speed 3)))
  (if pre-trim
      (flet ((pretrim-for-string-whitespace-to-foo (pre-trim-target-string)
               (declare (string pre-trim-target-string))
               (when (string-empty-p pre-trim-target-string)
                 (return-from string-whitespace-to-underscore 
                   (values pre-trim-target-string nil :string-empty)))
               (let ((pre (string-trim-whitespace pre-trim-target-string)))
                 (if (string-empty-p pre)
                     (if convert-all-whitespace
                         pre-trim-target-string
                         (if pre-trim
                             (return-from string-whitespace-to-underscore 
                               (values (make-string 0 :initial-element #\nul) nil :string-empty-or-all-whitespace))
                             ;; can't actually get here.
                             (return-from string-whitespace-to-underscore 
                               (values pre-trim-target-string nil :string-all-whitespace))))
                     pre))))
        (let ((pre-post (pretrim-for-string-whitespace-to-foo target-string)))
          (declare (string-not-empty pre-post))
          (string-whitespace-to-char pre-post #\_ :convert-all-whitespace convert-all-whitespace)))
      (string-whitespace-to-char target-string #\_ :convert-all-whitespace convert-all-whitespace)))


;;; :COURTESY bknr-datastore-20100901-git/src/utils/utils.lisp `find-matching-strings'
(defun string-find-matching (regexp strings &key case-sensitive)
  (let ((scanner (cl-ppcre:create-scanner regexp :case-insensitive-mode (not case-sensitive))))
    (remove-if-not #'(lambda (str)
                       (cl-ppcre:scan scanner str)) strings)))

;; ;;(file-namestring (pathname-directory (probe-file "/mnt/NEF-DRV-A/EBAY/BMP-Scans/e1052/"))
;; ;;  (directory-namestring (pathname-directory "/mnt/NEF-DRV-A/EBAY/BMP-Scans/e1052/"))
;; ;; (probe-file-maybe "/mnt/NEF-DRV-A/EBAY/BMP-Scans/e1052/e1056-0 (1).bmp")

;; ;;   (probe-file "/mnt/NEF-DRV-A/EBAY/BMP-Scans/e1052/e1056-0 (1).bmp")
;; ;;            (typecase 
;; ;; :SOURCE (URL `http://a-nickels-worth.blogspot.com/2008/02/scripting-in-cl.html')
;; (defun do-all-lines (fun-for-line &rest filenames)
;;   (let ((chk-filenames (pathname-file-list-if fun-for-line)))
;;     (when (null chk-filenames) 
;;       (return-from do-all-lines))
;;     (dolist (cur-pth chk-filenames)
;;       (declare (pathname cur-pth))
;;       ;; (dolist (filename (typecase filename-or-list
;;       ;;                     (cons filename-or-list)
;;       ;;                     (t (list filename-or-list))))
;;       (with-open-file (stream cur-pth) ;; :external-format :element-type :default
;;         (if (eql (stream-element-type stream) 'character)
;;             (loop
;;                for line = (read-line stream nil nil)
;;                while line
;;                do (funcall fun-for-line line))
;;             ;; do (apply fun-for-line line))                   
;;             )))))

;; (fundoc 'do-all-lines
;;         "Evaluate FUN-FOR-LINE for each line of file in FILENAMES.~%~@
;; :EXAMPLE~%~@
;;  { ... <EXAMPLE> ... } ~%~@
;; :SEE-ALSO `<XREF>'.~%►►►")

;; ;; :COURESY Jacob Gabrielson 
;; ;; :SOURCE (URL `http://a-nickels-worth.blogspot.com/2008/02/scripting-in-cl.html')
;; (defmacro with-lines-from-files ((var &rest filenames) &body body)
;;   `(do-all-lines (lambda (,var) ,@body) ,@filenames))

;; ;; :COURESY Jacob Gabrielson 
;; ;; :SOURCE (URL `http://a-nickels-worth.blogspot.com/2008/02/scripting-in-cl.html')
;; (defun file-grep (regexp &rest filenames)
;;   (with-lines-from-files (line filenames)
;;     (when (scan regexp line)
;;       (format t "~A~%" line))))

;; (defun file-grep-one (regexp filename)
;;   (file-grep regexp filename))


;;; ==============================
;;; :REGEXP-DOCUMENTATION
;;; ==============================

(fundoc 'string-underscore-to-dash
"Replace all occurences of #\\_ with #\\- in STRING.~%~@
Return value is as if by `cl:values':~%
 - First value is STRING with any modifications.
 - Second value is a boolean, T when modifications to STRING were made else NIL.~%~@
Replacement as if by `cl-ppcre:regex-replace-all'.~%~@
:EXAMPLE~%
 \(string-underscore-to-dash \"i_am_an_ugly_string\"\)~%
 \(string-underscore-to-dash \(string-underscore-to-dash \"i_am_an_ugly_string\"\)\)~%
 \(string-underscore-to-dash \"\"\)~%~@
:NOTE Included mostly to remind us to use the :PACKAGE cl-ppcre.~%~@
:SEE-ALSO `dbc:field-name-underscore-to-dash'.~%►►►")

(fundoc 'string-find-matching
"Find matches for REGEXP in STRINGS.~%~@
When keyword CASE-SENSITIVE is non-nil do so as if the keyword arg
CASE-INSENSITIVE-MODE to `cl-ppcre:create-scanner' is non-nil.~%~@
:EXAMPLE~%
 \(string-find-matching \"a.[ab]\" '\(\"a\" \"a a\" \"a b\" \"b a\" \"c d a\"\)\)~%
 \(string-find-matching \"a.[ab]\\\(q.*\\\)\" '\(\"a bqbib\"\)\)~%~@
:SEE-ALSO `cl-ppcre:scan', `cl:find', `cl:find-if', `cl:position',
`cl:search'.~%►►►")

(fundoc 'string-whitespace-to-char
"Replace each whitespace character in TARGET-STRING with REPLACEMENT-CHAR.
Each occurence of an elt in `mon:*whitespace-chars*' is replaced.~%~@
REPLACEMENT-CHAR should be of type `mon:char-or-char-code-integer-or-string-1'.
If REPLACEMENT-CHAR is a one character string or of type `char-code-integer' it
is converted to a character and must satisfy the predicate
`mon:char-not-whitespace-char-p'. An error is signaled if not.~%~@
Keyword CONVERT-ALL-WHITESPACE is a boolean. When non-nil if TARGET-STRING is
`mon:string-all-whitespace-p' each whitespace character will be replaced. 
For large empty strings this may not be desirable, as such the default is NIL.~%~@
Return value is as if by `cl:values'.
When TARGET-STRING is not `mon:string-empty' nor `mon:string-all-whitespace-p'
nth-value 0 and 1 are as if by `cl-ppcre:regex-replace-all'. In addition
TARGET-STRING is returned as nth-value 2:~%
 REPLACEMENT, <BOOLEAN>, <TARGET-STRING>~%~@
When TARGET-STRING is `mon:string-empty-p' return:~%
 TARGET-STRING, nil, :string-empty~%~@
When TARGET-STRING is `mon:string-all-whitespace-p' and keyword
CONVERT-ALL-WHITESPACE is NIL return:~%
 REPLACEMENT, <BOOLEAN>, :string-all-whitespace
When CONVERT-ALL-WHITESPACE is T return:~%
 TARGET-STRING, nil, :string-all-whitespace
:EXAMPLE~%
 \(string-whitespace-to-char \"Q E D\" #\\_\)~%
 \(string-whitespace-to-char \"Q E D\" \"_\"\)~%
 \(string-whitespace-to-char \"Q E D\" \(char-code #\\_\)~%
 \(string-whitespace-to-char \"Q E D\" 95\)~%
 \(string-whitespace-to-char \(format nil \"~{~C~}\" *whitespace-chars*\) #\\_\)~%
 \(string-whitespace-to-char \(format nil \"~{~C~}\" *whitespace-chars*\) 95  :convert-all-whitespace t\)~%
 \(string-whitespace-to-char \"\" #\\_\)~%
;; Following fail successfully:~%
 \(string-whitespace-to-char \"a b c\" #\\newline\)~%
 \(string-whitespace-to-char \"a b c\" 'bubba\)~%~@
:SEE-ALSO `mon:string-whitespace-to-dash' `mon:string-whitespace-to-underscore'.~%►►►")

(fundoc 'string-whitespace-to-underscore
"Like `string-whitespace-to-char' but with REPLACEMENT-CHAR hardwired to character #\\_.~%~@
Keyword PRE-TRIM and CONVERT-ALL-WHITESPACE are booleans.
When keyword PRE-TRIM is non-nil string is first processed with
`mon:string-trim-whitespace'. Default is T.~%~@
Keyword CONVERT-ALL-WHITESPACE is as `string-whitespace-to-char' but is affected
by value of PRE-TRIM. Default is NIL. See examples below for usage.~%~@
:EXAMPLE~%~@
 \(string-whitespace-to-underscore \"Q E D\")~%
 \(string-whitespace-to-underscore \"Q E D\"\)~%
 \(string-whitespace-to-underscore \"Q E D\")~%
 \(string-whitespace-to-underscore \"Q E D\"\)~%
 \(string-whitespace-to-underscore \(format nil \"~~{~~C~~}\" *whitespace-chars*\))~%
 \(string-whitespace-to-underscore \(format nil \"~~{~~C~~}\" *whitespace-chars*\) :convert-all-whitespace t\)~%
 \(string-whitespace-to-underscore \"\")~%
 \(string-whitespace-to-underscore \"    \"\)
 \(string-whitespace-to-underscore \"    \" :pre-trim t :convert-all-whitespace nil\)~%
 \(string-whitespace-to-underscore \"    \" :pre-trim nil :convert-all-whitespace t\)~%
 \(string-whitespace-to-underscore \"    \" :pre-trim t :convert-all-whitespace t\)~%
 \(string-whitespace-to-underscore \"    \" :pre-trim nil :convert-all-whitespace nil\)~%~@
:SEE-ALSO `mon:string-whitespace-to-dash' `mon:string-whitespace-to-underscore'.~%►►►")

(fundoc 'string-whitespace-to-dash
"Like `string-whitespace-to-char' but with REPLACEMENT-CHAR hardwired to character #\\-.~%
Keyword PRE-TRIM and CONVERT-ALL-WHITESPACE are booleans.
When keyword PRE-TRIM is non-nil string is first processed with
`mon:string-trim-whitespace'. Default is T.~%~@
Keyword CONVERT-ALL-WHITESPACE is as `string-whitespace-to-char' but is affected
by value of PRE-TRIM. Default is NIL. See examples below for usage.~%~@
:EXAMPLE~%
 \(string-whitespace-to-dash \"Q E D\")~%
 \(string-whitespace-to-dash \"Q E D\"\)~%
 \(string-whitespace-to-dash \"Q E D\"\)~%
 \(string-whitespace-to-dash \"Q E D\"\)~%
 \(string-whitespace-to-dash \(format nil \"~~{~~C~~}\" *whitespace-chars*\))~%
 \(string-whitespace-to-dash \(format nil \"~~{~~C~~}\" *whitespace-chars*\) :convert-all-whitespace t\)~%
 \(string-whitespace-to-dash \"\")~% 
 \(string-whitespace-to-dash \"    \" :pre-trim t :convert-all-whitespace nil\)~%
 \(string-whitespace-to-dash \"    \" :pre-trim nil :convert-all-whitespace t\)~%
 \(string-whitespace-to-dash \"    \" :pre-trim t :convert-all-whitespace t\)~%
 \(string-whitespace-to-dash \"    \" :pre-trim nil :convert-all-whitespace nil\)~%~@
:SEE-ALSO `mon:string-whitespace-to-dash' `mon:string-whitespace-to-underscore'.~%►►►")q

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
