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

(declaim (inline %string-find-matching-verify-char-seq))
(defun %string-find-matching-verify-char-seq (char-seq)
  ;; its a vector string or list. 
  ;; (%string-find-matching-verify-char-seq "abc")
  ;; (%string-find-matching-verify-char-seq '(#\a #\b #\c))
  ;; (%string-find-matching-verify-char-seq #(#\a #\b #\c))
  ;; (%string-find-matching-verify-char-seq 
  ;;  (make-array 3 :element-type 'character :adjustable t :initial-contents #(#\a #\b #\c)))
  (declare ((and not-null (or list simple-string vector)) char-seq)
           (optimize (speed 3)))
  (etypecase char-seq
    (list (the boolean (loop 
                          for chars in (the list char-seq)
                          always (characterp chars))))
    (string (the boolean t))
    (simple-vector (the boolean (loop 
                                   for chars across (the simple-vector char-seq)
                                   always (characterp chars))))
    (vector (the boolean (loop 
                            for chars across (the (vector character *) char-seq)
                            always (characterp chars))))))

(declaim (inline %string-find-matching-verify-list-for-strings-only-t))
(defun %string-find-matching-verify-list-for-strings-only-t (strings-list)
  (declare ((or string (and not-null list)) strings-list)
           (inline list-proper-p
                   each-a-string-p)
           (optimize (speed 3)))
  (when (stringp strings-list)
    (return-from %string-find-matching-verify-list-for-strings-only-t (the list (list strings-list))))
  (unless (list-proper-p strings-list)
    (proper-list-error :w-sym 'string-find-matching 
                       :w-type 'function 
                       :error-args (list (quote strings) strings-list)))
  (if (each-a-string-p strings-list)
      strings-list
      (simple-error-mon  :w-sym  "string-find-matching"
                         :w-type 'function
                         :w-spec "elt in list not `cl:stringp'~% ~
                                 found element of type: ~S~% ~"
                         :w-args (type-of (find-if-not #'stringp strings-list))
                         :w-got strings-list
                         :w-type-of nil
                         :signal-or-only nil)))

(declaim (inline %string-find-matching-verify-list-for-strings-only-null))
(defun %string-find-matching-verify-list-for-strings-only-null (list)
  ;; (%string-find-matching-verify-list-for-strings-only-null  '(\#a \#b))
  ;; (%string-find-matching-verify-list-for-strings-only-null  '("a" "b"))
  ;; (%string-find-matching-verify-list-for-strings-only-null  '("a" #(#\b)))
  (declare ((and not-null list) list)
           (inline %string-find-matching-verify-char-seq)
           (optimize (speed 3)))
  (or (and (%string-find-matching-verify-char-seq (the list list))
           (the list (list list)))
      (and (each-a-string-or-vector-in-list (the list list))
           (the list list))
      (simple-error-mon :w-sym  "string-find-matching"
                        :w-type 'function
                        :w-spec "With keyword STRINGS-ONLY null, arg STRINGS invalid~% ~
                                 found element of type: ~S~% ~"
                        :w-args (type-of (find-if-not #'stringp list))
                        :w-got list
                        :w-type-of nil
                        :signal-or-only nil)))

(declaim (inline %string-find-matching-verify-vector-for-strings-only-null))
(defun %string-find-matching-verify-vector-for-strings-only-null (vector strings-only)
  ;; (%string-find-matching-verify-vector-for-strings-only #("abc" "abc") nil )
  ;; => #("abc" "abc")
  ;; (%string-find-matching-verify-vector-for-strings-only-null #(#(#\a #\b #\c #\d) #(#\a #\b #\c #\d)) nil)
  ;; => #(#(#\a #\b #\c #\d) #(#\a #\b #\c #\d)) nil)
  ;; (%string-find-matching-verify-vector-for-strings-only-null #(#(#\a #\b #\c #\d) "abc") nil)
  ;; => #(#(#\a #\b #\c #\d) "abc")
  ;; (%string-find-matching-verify-vector-for-strings-only-null #(#\a #\b #\c #\d) nil)
  ;; => (#(#\a #\b #\c #\d))
  ;; (%string-find-matching-verify-vector-for-strings-only-null "abc" nil)
  ;; => ("abc")
  ;; (%string-find-matching-verify-vector-for-strings-only-null "abc" t)
  (declare ((and vector (not bit-vector)) vector)
           (boolean strings-only)
           (inline %string-find-matching-verify-char-seq)
           (optimize (speed 3)))
  (unless (null strings-only)
    (simple-error-mon  :w-sym "string-find-matching"
                       :w-type 'function
                       :w-spec "With keyword STRINGS-ONLY T found arg STRINGS was `cl:vectorp'"
                       :w-got vector
                       :w-type t
                       :signal-or-only nil))
  (if (etypecase vector
        (simple-string (%string-find-matching-verify-char-seq (the simple-string vector)))
        (string (%string-find-matching-verify-char-seq        (the string vector)))
        (simple-vector (%string-find-matching-verify-char-seq (the simple-vector vector)))
        (vector (%string-find-matching-verify-char-seq        (the vector vector))))
      (the list (list vector))
      (if (each-a-string-or-vector-in-vector vector)
          (the vector vector)
          (simple-error-mon  :w-sym  "string-find-matching"
                             :w-type 'function
                             :w-spec "With keyword STRINGS-ONLY non-nil, elt in list neither `cl:stringp' nor `cl:vectorp'~% ~
                                    found element of type: ~S~% ~"
                             :w-args (type-of (find-if-not #'vectorp vector))
                             :w-got vector
                             :w-type-of nil
                             :signal-or-only nil))))

;;; ==============================
;; :NOTE cl-ppcre:scan is perfectly capable of accepting a list or vector
;; contained of any of the following:
;;   "abc" #(#\a #\b #\c) '(#\a #\b #\c)
;; and will match on them without balking. IOW, each of the following is a valid form:
;;
;; (remove-if-not #'(lambda (seq) (cl-ppcre:scan #\a seq)) #("bc" (#\a #\b #\c) #(#\a #\b #\c)))
;;  => #((#\a #\b #\c) #(#\a #\b #\c))
;;
;; (remove-if-not #'(lambda (seq) (cl-ppcre:scan "a" seq)) '("bc" (#\a #\b #\c) #(#\a #\b #\c)))
;;  => ((#\a #\b #\c) #(#\a #\b #\c))
;;
;; We currently do not allow lists of characters regardless of whether STRINGS-ONLY is null or not.
;; It is a relatively straightforward fix to allow this but its not clear if we even _should_.
(defun string-find-matching (regexp strings &key case-sensitive (strings-only t))

  (declare ((and not-null sequence) strings) ;; sequence-zerop
           ((or string character) regexp)    ;; function) 
           (boolean case-sensitive)
           (inline %string-find-matching-verify-char-seq
                   %string-find-matching-verify-list-for-strings-only-t
                   %string-find-matching-verify-list-for-strings-only-null                   
                   %string-find-matching-verify-vector-for-strings-only-null)
           (optimize (speed 3)))
  (let ((scanner (cl-ppcre:create-scanner regexp :case-insensitive-mode (not case-sensitive)))
        (chk-strings
         (if (null strings-only)
             (etypecase strings
               (list
                (%string-find-matching-verify-list-for-strings-only-null (the list strings)))
               (vector
                (%string-find-matching-verify-vector-for-strings-only-null (the vector strings) strings-only)))
             (%string-find-matching-verify-list-for-strings-only-t strings))))
    (declare ((or list vector) chk-strings)
             (function scanner))
    (flet ((filter-matches (str)
             (cl-ppcre:scan scanner str)))
      (remove-if-not #'filter-matches chk-strings))))

;; :SOURCE mccme-helpers/packages:file mccme-helpers/parsetype 
;; :WAS `escape-for-regex'
(defun string-escape-for-regex (string-for-regex)
  (let ((anchor-brace (cl-ppcre:create-scanner "\\[\\^]"))
        (slashes-4 (cl-ppcre:create-scanner (make-string 2 :initial-element #\\))) ;; "\\\\"
        (slashes-8 (make-string 4 :initial-element #\\))) ;;  "\\\\\\\\"
    (cl-ppcre:regex-replace-all anchor-brace 
                                (cl-ppcre:regex-replace-all slashes-4
                                                            (cl-ppcre:regex-replace-all "(.)" string-for-regex "[\\1]")
                                                            slashes-8)
                                "\\^")))

 
;;; ==============================
;;; :NOTE `regex-when' `regex-case' macros are probably a bad idea b/c they can fail in
;;; bad ways when the index into the match array doesn't match the bindings.
;;; ==============================
;; :COURTESY Julian Stecklina's gdb-remote
;; :SOURCE (URL `https://github.com/blitz/gdb-remote/blob/master/utility-macros.lisp')
;; (defmacro regex-when ((regex string &rest submatches) &body body)
;;   (alexandria:with-unique-names (match? submatch-vector)
;;     `(multiple-value-bind (,match? ,submatch-vector)
;;          (cl-ppcre:scan-to-strings ,regex ,string)
;;        ;; (declare (ignorable ,submatch-vector))
;;        ;; Following is from Stecklina' orignal definition which failed when the match
;;        ;; was for simple regexes like "FOO". e.g.:
;;        ;;  (cl-ppcre:scan-to-strings "FOO" "string-FOO") => "FOO", #()
;;        ;; returns a vector we can't meaningfully index into b/c its length is zerop!
;;        ;; 
;;        ;; :WAS 
;;        ;; (when ,match?
;;        ;;  (let ,(loop
;;        ;;         for index upfrom 0
;;        ;;         for var-name in submatches
;;        ;;         collect `(,var-name (aref ,submatch-vector ,index)))
;;        ;;   ,@body)))))
;;        ;;
;;        (when ,match?
;;          (if (zerop (length ,submatch-vector))
;;              (let ((,(or (car submatches) (gensym))  ,match?))
;;                ,@body)
;;              ;; :NOTE this fails when the length of submatches is greater than that of submatch-vector
;;              (let ,(loop
;;                      for index upfrom 0
;;                      for var-name in submatches
;;                      collect `(,var-name (aref ,submatch-vector ,index)))
;;                ,@body))))))
;;             
;; (regex-when ("FOO" "string-FOO" foo-part more-part again) again)
;; (regex-when ("(.*)(-FOO)" "string-FOO" string-part foo-part) (downcase foo-part))
;; (regex-when ("((.*)(-FOO))" "string-FOO" string-part foo-part more-part) (list string-part foo-part more-part))
;; (regex-when ("(.*)(-FOO)" "string-FOO" string-part foo-part error-part) error-part) => error
;;
;; :COURTESY Julian Stecklina's gdb-remote
;; :SOURCE (URL `https://github.com/blitz/gdb-remote/blob/master/utility-macros.lisp')
;; (defmacro regex-case (string &body clauses)
;;   (alexandria:with-unique-names (the-string block-name)
;;     `(let ((,the-string ,string))
;;        (block ,block-name
;;          ,@(loop
;;               for (condition . body) in clauses
;;               collect (if (eq condition 't)
;;                           `(return-from ,block-name
;;                              (progn ,@body))
;;                           (destructuring-bind (regex &rest vars)
;;                               condition
;;                             `(regex-when (,regex ,the-string ,@vars)
;;                                (return-from ,block-name
;;                                  (progn ,@body))))))))))
;;
;; (fundoc 'regex-case
;; "Match STRING with a regular expression in CLAUSES.~%~@
;; CLAUSES is a list each element of which is a list with one of the following formats:~%
;;  \(\(<REGEX>\) <FORMS>\)~%
;;  \(\(<REGEX> <MATCH-VARS>\) <FORMS>\)~%~@
;; <REGEX> is a regular expression suitable for use with `cl-ppcre:scan-to-strings'.~%~@
;; <MATCH-VARS> are any one or more symbols bound to a corresponding index of the
;; vector at nth-value 1 returned by `cl-ppcre:scan-to-strings'. It is an error if
;; the number of MATCH-VARS exceeds the indexable range of the returned match vector.~%~@
;; <FORMS> is zero or more forms to be executed. When <FORMS> are present these are
;; executed as an implicit progn with <MATCH-VARS> bindings active in the dynamic
;; scope of the progn.~%~@
;; :EXAMPLE~% 
;;  \(regex-case \"string-FOO\"
;;    \(\(\"^-FOO\"\) \"you'll never-see-this\"\)
;;    \(\(\"\(.*\)\(-FOO\)\" string-part foo-part\)
;;     \(list
;;      \(string-capitalize string-part\)
;;      \(string-downcase foo-part\)\)\)\)~%~@
;; :NOTE The following signals an out-of-bounds error b/c the local wont-match var is
;; outside the bounds of the vector returned by `cl-ppcre:scan-to-strings'
;; nth-value 1:~%
;;  \(regex-case \"string-FOO\"
;;    \(\(\"^-FOO\"\) \"you'll never-see-this\"\)
;;    \(\(\"\(.*\)\(-FOO\)\" string-part foo-part wont-match\)
;;     wont-match\)\)~%~@
;; :SEE-ALSO `regex-when', `cl-ppcre:register-groups-bind', `string-case:string-case'.~%▶▶▶")
;;; ==============================


;;; ==============================
;; :NOTE Following functions: `do-all-lines', `with-lines-from-files', `file-grep'
;; :SOURCE (URL `http://a-nickels-worth.blogspot.com/2008/02/scripting-in-cl.html')
;; :COURESY Jacob Gabrielson 
;;; ==============================
;; 
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
;;
;; (fundoc 'do-all-lines
;;         "Evaluate FUN-FOR-LINE for each line of file in FILENAMES.~%~@
;; :EXAMPLE~%~@
;;  { ... <EXAMPLE> ... } ~%~@
;; :SEE-ALSO `<XREF>'.~%▶▶▶")
;;
;; (defmacro with-lines-from-files ((var &rest filenames) &body body)
;;   `(do-all-lines (lambda (,var) ,@body) ,@filenames))
;;
;; (defun file-grep (regexp &rest filenames)
;;   (with-lines-from-files (line filenames)
;;     (when (scan regexp line)
;;       (format t "~A~%" line))))
;;
;; (defun file-grep-one (regexp filename)
;;   (file-grep regexp filename))
;;
;;; ==============================


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
:SEE-ALSO `dbc:field-name-underscore-to-dash'.~%▶▶▶")

(fundoc 'string-find-matching
        "Find matches for REGEXP in STRINGS.~%~@
STRINGS is a list of strings or string.~%~@
When keyword CASE-SENSITIVE is non-nil it is as with `cl-ppcre:create-scanner'.~%~@
When keyword STRINGS-ONLY is null allow STRINGS to be lists containing vectors
of characters and allow STRINGS to be a vector containings strings or vectors of
characters.~%~@
:EXAMPLE~%
 \(string-find-matching \"a.[ab]\" '\(\"a\" \"a a\" \"a b\" \"b a\" \"c d a\"\)\)~%
 \(string-find-matching \"a.[ab]\" '\(\"a\" \"a a\" \"a b\" \"b a\" \"c d a\"\)\)~%
 \(string-find-matching \"a.[ab]\\\(q.*\\\)\" '\(\"a bqbib\"\)\)~%
 \(string-find-matching \"a.[ab]\" '\(\"a\" #\(#\\a #\\a #\\a\) #\(#\\a #\\c #\\b\) \"acb\" #\(#\\b #\\a\) \"cda\"\) :strings-only nil\)~%
 \(string-find-matching \"a.[ab]\" #\(\"a\" \"a a\" \"a b\" \"b a\" \"c d a\"\) :strings-only nil\)~%
 \(string-find-matching \"a.[ab]\" #\(\"a\" #\(#\\a #\\a #\\a\) \"acb\" #\(#\\a #\\c #\\b\) #\(#\\b #\\a\) \"c d a\"\) :strings-only nil\)~%
 \(string-find-matching \"a\" #\(#\\a  #\\b #\\c\) :strings-only nil\)~%
 \(string-find-matching  #\\a '\(#\(#\\a  #\\b #\\c\)\)\)~%
 \(string-find-matching  #\\a '\(#\(#\\a  #\\b #\\c\)\) :strings-only nil\)~%
 \(string-find-matching  #\\a #\(#\\a  #\\b #\\c\) :strings-only nil\)~%
 \(string-find-matching #\\a '\(\" b c\" \"c b\" \"a\" \"a c\" \"a b\"\)\)~%~@
:NOTE the main driver of this function is `cl-ppcre:scan' which is perfectly
capable of accepting a list or vector contained of any of the following:~%
  \"abc\" #\(#\\a #\\b #\\c\) '\(#\\a #\\b #\\c\)~%~@
It will match on them without balking. IOW, each of the following is a valid form:~%
 \(remove-if-not #'\(lambda \(seq\) \(cl-ppcre:scan #\\a seq\)\)
                #\(\"bc\" \(#\\a #\\b #\\c\) #\(#\\a #\\b #\\c\)\)\)
 => #\(\(#\\a #\\b #\\c\) #\(#\\a #\\b #\\c\)\)~%
\(remove-if-not #'\(lambda \(seq\) \(cl-ppcre:scan \"a\" seq\)\)
               '\(\"bc\" \(#\\a #\\b #\\c\) #\(#\\a #\\b #\\c\)\)\)
 => \(\(#\\a #\\b #\\c\) #\(#\\a #\\b #\\c\)\)~%~@
We currently do not allow STRINGS to be containlists of characters regardless of
whether STRINGS-ONLY is null or not. Indeed, wihile it is a relatively
straightforward fix to allow this, it is not clear if we even _should_.~%~@
:SEE-ALSO `cl-ppcre:scan', `cl:find', `cl:find-if', `cl:position',
`cl:search'.~%▶▶▶")

(fundoc 'string-whitespace-to-char
"Replace each whitespace character in TARGET-STRING with REPLACEMENT-CHAR.
Each occurence of an elt in `mon:*whitespace-chars*' is replaced.~%~@
REPLACEMENT-CHAR should be of type `mon:char-or-char-code-integer-or-string-1'.
If REPLACEMENT-CHAR is a one character string or of type `char-code-integer' it
is converted to a character and must satisfy predicate
`mon:char-not-whitespace-char-p'. An error is signaled if not.~%~@
Keyword CONVERT-ALL-WHITESPACE is a boolean. When non-nil if TARGET-STRING is
`mon:string-all-whitespace-p' each whitespace character will be replaced. 
For large empty strings this may not be desirable, as such the default is NIL.~%~@
Return value is as if by `cl:values'.
When TARGET-STRING is not `mon:string-empty' nor `mon:string-all-whitespace-p'
`cl:nth-value' 0 and 1 are as if by `cl-ppcre:regex-replace-all'.
In addition, TARGET-STRING is returned as `cl:nth-value' 2:~%
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
 \(string-whitespace-to-char \(format nil \"~~{~~C~~}\" *whitespace-chars*\) #\\_\)~%
 \(string-whitespace-to-char \(format nil \"~~{~~C~~}\" *whitespace-chars*\) 95  :convert-all-whitespace t\)~%
 \(string-whitespace-to-char \"\" #\\_\)~%
;; Following fail successfully:~%
 \(string-whitespace-to-char \"a b c\" #\\newline\)~%
 \(string-whitespace-to-char \"a b c\" 'bubba\)~%~@
:SEE-ALSO `mon:string-whitespace-to-dash' `mon:string-whitespace-to-underscore'.~%▶▶▶")

(fundoc 'string-whitespace-to-underscore
"Like `string-whitespace-to-char' but with REPLACEMENT-CHAR hardwired to character #\\_.~%~@
Keyword PRE-TRIM and CONVERT-ALL-WHITESPACE are booleans.
When keyword PRE-TRIM is non-nil string is first processed with
`mon:string-trim-whitespace'. Default is T.~%~@
Keyword CONVERT-ALL-WHITESPACE is as `string-whitespace-to-char' but is affected
by value of PRE-TRIM. Default is NIL. See examples below for usage.~%~@
:EXAMPLE~%
 \(string-whitespace-to-underscore \"Q E D\")~%
 \(string-whitespace-to-underscore \"Q E D\"\)~%
 \(string-whitespace-to-underscore \"Q E D\")~%
 \(string-whitespace-to-underscore \"Q E D\"\)~%
 \(string-whitespace-to-underscore \(format nil \"~~{~~C~~}\" *whitespace-chars*\))~%
 \(string-whitespace-to-underscore \(format nil \"~~{~~C~~}\" *whitespace-chars*\) :convert-all-whitespace t\)~%
 \(string-whitespace-to-underscore \"\")~%
 \(string-whitespace-to-underscore \"    \"\)~%
 \(string-whitespace-to-underscore \"    \" :pre-trim t :convert-all-whitespace nil\)~%
 \(string-whitespace-to-underscore \"    \" :pre-trim nil :convert-all-whitespace t\)~%
 \(string-whitespace-to-underscore \"    \" :pre-trim t :convert-all-whitespace t\)~%
 \(string-whitespace-to-underscore \"    \" :pre-trim nil :convert-all-whitespace nil\)~%~@
:SEE-ALSO `mon:string-whitespace-to-dash' `mon:string-whitespace-to-underscore'.~%▶▶▶")

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
:SEE-ALSO `mon:string-whitespace-to-dash' `mon:string-whitespace-to-underscore'.~%▶▶▶")

(fundoc 'string-escape-for-regex
        "Create a cl-ppcre:scanner closure from STRING-FOR-REGEX that \"won't miss\" when string contains regexp meta-chars.~%~@
:EXAMPLE
 \(let* \(\(date-str    \"2011-07-24\"\)
        \(date-str-re \(cl-ppcre:create-scanner 
                      \(string-escape-for-regex date-str\)\)\)
        \(repl-str    \"bubba\"\)\)
   \(cl-ppcre:regex-replace-all date-str-re date-str repl-str\)\)
 => \"bubba\", T~%
 \(cl-ppcre:regex-replace-all \(tt--string-escape-for-regex \"bub?a\"\) \"bubba\" \"howdy\"\)
 => \"bubba\", NIL~%
 \(string-escape-for-regex \"^/b*ub\\\\?a./$\"\)
 => \"\\\\^[/][b][*][u][b][\\\\\\\\][?][a][.][/][$]\", T~%
 \(cl-ppcre:regex-replace \(string-escape-for-regex \"^/b*ub\\\\?a./$\"\) \"^/b*ub\\\\?a./$\" \"bubba\"\)
 => \"bubba\", T~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
