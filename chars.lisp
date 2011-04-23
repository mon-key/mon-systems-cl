;;; :FILE mon-systems/chars.lisp
;;; ==============================

;;; ==============================
;;; :NOTE Following are teh SBCL internals notes re the implementation of :SB-UNICODE
;;; :SOURCE (URL `http://www.sbcl.org/sbcl-internals/Character-and-String-Types.html#Character-and-String-Types')
;;
;; The :SB-UNICODE feature implies support for all 1114112 potential characters in
;; the character space defined by the Unicode consortium, with the identity mapping
;; between lisp char-code and Unicode code point. SBCL releases before version
;; 0.8.17, and those without the :SB-UNICODE feature, support only 256 characters,
;; with the identity mapping between char-code and Latin1 (or, equivalently, the
;; first 256 Unicode) code point.
;;
;; In the absence of the :SB-UNICODE feature, the types base-char and character are
;; identical, and encompass the set of all 256 characters supported by the
;; implementation. With the :SB-UNICODE on *features* (the default), however,
;; base-char and character are distinct: character encompasses the set of all
;; 1114112 characters, while base-char represents the set of the first 128
;; characters.

;; The effect of this on string types is that an sbcl configured with :SB-UNICODE
;; has three disjoint string types: (vector nil), base-string and (vector
;; character). In a build without :SB-UNICODE, there are two such disjoint types:
;; (vector nil) and (vector character); base-string is identially equal to (vector
;; character).
;;
;; The SB-KERNEL:CHARACTER-SET-TYPE represents possibly noncontiguous sets of
;; characters as lists of range pairs: for example, the type standard-char is
;; represented as the type (sb-kernel:character-set '((10 . 10) (32 . 126)))
;;; ==============================
;;
;;; :NOTE :FILE sbcl/src/code/octets.lisp defines the character conversion routines
;;; :SEE comments beginning around: `define-unibyte-mapper'
;; ,----
;; | to-octets conversions
;; | 
;; | to latin (including ascii)
;; | 
;; | Converting bytes to character codes is easy: just use a 256-element
;; | lookup table that maps each possible byte to its corresponding
;; | character code.
;; | 
;; | Converting character codes to bytes is a little harder, since the
;; | codes may be spare (e.g. we use codes 0-127, 3490, and 4598).  
;; |  {...}
;; `----
;; :INTERNAL
;; `define-unibyte-mapper', `define-latin->string*', `string->latin%',
;; `get-latin-bytes', `define-latin->string*', `define-latin->string',
;; `sb-impl::*default-external-format*', `default-external-format'
;;
;; :PUBLIC
;; `octets-to-string', `string-to-octets', `+unicode-replacement-character+',
;; `use-unicode-replacement-char', `with-standard-replacement-character',
;; `with-default-decoding-replacement',
;;
;; +unicode-replacement-character+     ;; sb-impl::+unicode-replacement-character+
;; with-default-decoding-replacement   ;; sb-impl::with-default-decoding-replacement
;; with-standard-replacement-character ;; sb-impl::with-standard-replacement-character
;;
;; (string-to-octets "bubba")
;; ;=> #(98 117 98 98 117)
;; (octets-to-string (make-array 3 :element-type '(unsigned-byte 8)
;;				    :initial-contents '(32 59 102)))
;; => " ;f"

;; stringstring-coerce-from

;;; ==============================
;;
;; ,----
;; | 
;; | 10.2 Reader and Printer
;; | 
;; | The `"' reader macro always constructs an object of type `(simple-array
;; | character)', even if all of the characters within the quotation marks
;; | are of type `base-char'.  This implies that only strings of type
;; | `(vector character)' will be able to be printed when `*print-readably*'
;; | is true: attempting to print strings of other types will cause an error
;; | of type `print-not-readable'.
;; |
;; `---- :SEE info node `(sbcl-internals)Reader and Printer'
;;
;;; ==============================



(in-package #:mon)
;; *package*


(defun whitespace-char-p (maybe-wsp-char)
  ;; :WAS 
  ;; (or (member maybe-wsp-char *whitespace-chars* :test #'char=)
  ;; :NOTE #\space is considered to be `graphic-char-p' and is in fact its lower bounds.
  ;; (not (graphic-char-p maybe-wsp-char))))
  (typep maybe-wsp-char 'whitespace-char))

(defun hexadecimal-char-p (maybe-hex-char)
  (typep maybe-hex-char 'hexadecimal-char))

(defun char-code-integer-to-string (char-code-int &optional stream)
  (declare (type char-code-integer char-code-int)
           (stream-or-boolean stream))
  ;; :NOTE Using `mon:open-stream-output-stream-p' instead:  
  (and stream (open-stream-output-stream-p stream :allow-booleans t :w-error t))
  (etypecase char-code-int 
    (char-code-integer (or 
			(and stream (write-string (string (code-char char-code-int)) stream))
			(string (code-char char-code-int))))))

(defun char-to-string (char &optional stream)
  (declare (type (or char-code-integer character) char)
           (stream-or-boolean stream))
  ;; :WAS (and stream (or (and (streamp stream) (open-stream-p stream)) (error 'stream-error)))
  ;; :NOTE Use `mon:open-stream-output-stream-p' instead:
  (and stream (open-stream-output-stream-p stream :allow-booleans t :w-error t))
  (etypecase char
    (char-code-integer (char-code-integer-to-string char stream))
    (character         (or (and stream (write-string (string char) stream))
			   (string char)))))

(defun char-code-integer-to-char  (char-or-char-code-integer &key w-no-error)
  ;; (declare (type (or character char-code-integer) char-or-char-code-integer))
  (typecase char-or-char-code-integer
    (character char-or-char-code-integer)
    (char-code-integer (code-char char-or-char-code-integer))
    (t (if (not w-no-error)
           (simple-error-mon 
            :w-sym  'char-code-integer-to-char
            :w-type 'function 
            :w-spec "arg CHAR-OR-CHAR-CODE-INTEGER neither `cl:characterp' ~
                  nor `mon:char-code-integer-p'"
            :w-got   char-or-char-code-integer
            :w-type-of t
            :signal-or-only nil)
           char-or-char-code-integer))))

(defun char-numeric= (char-x char-y)
 (declare ((or character char-code-integer) char-x char-y)
          (inline %char-numeric=)
          (optimize (speed 3) (safety 1)))
  (values-list 
   (etypecase char-x 
     (character 
      (etypecase char-y
        (character (or (and (zerop (%char-numeric= char-x 
                                                   char-y)) 
                            (list t char-x))
                       (list nil char-x char-y)))
        (char-code-integer (or (and (zerop (%char-numeric= char-x (code-char char-y)))
                                    (list t char-x))
                               (list nil char-x (code-char char-y))))))
    (char-code-integer
     (etypecase char-y
       (char-code-integer (or (and (zerop (logxor char-x char-y))
                                   (list t (code-char char-x)))
                              (list nil (code-char char-x) (code-char char-y))))
       (character (or (and (zerop (logxor char-x (char-code char-y)))
                           (list t char-y))
                      (list nil (code-char char-x) char-y))))))))

(defun char-list-to-string (char-list &optional stream)
  (declare (type list char-list)
           (stream-or-boolean stream))
  ;; :WAS (and stream (or (and (streamp stream) (open-stream-p stream)) (error 'stream-error)))
  ;; :NOTE Using `mon:open-stream-output-stream-p' instead:  
  (and stream (open-stream-output-stream-p stream :allow-booleans t :w-error t))
  (when
      (etypecase char-list
	(each-a-character-or-char-code-integer t)
	(each-a-character                      t)
	(each-a-char-code-integer              t))	
    (with-open-stream (msos (make-string-output-stream))
	  (loop 
             :for chars :in char-list 
             :do (char-to-string chars msos)
             :finally (return (if stream
                                  (values (write-string (get-output-stream-string msos) stream)
                                          (length char-list))
                                  (get-output-stream-string msos)
                                  ))))))

(defun char-position (w-char in-string &optional (start-idx 0) (max-idx (length in-string)))
  (declare (type string in-string)
           (type character w-char))
  (do* ((i start-idx (1+ i)))
       ((= i max-idx) nil)
    (when (char= w-char (char in-string i)) (return i))))

(defun chars-not-in-string-p (char-bag string &key (test #'char=))
  (declare (type string string)
           (type proper-list-not-null char-bag))
  (loop 
     :named bail-if
     :for fnd-char :across string
     :for pos :below (length string)
     :do (loop 
           :for chk-chars :in char-bag
           :when (funcall test chk-chars fnd-char)
           :do (return-from bail-if (values nil pos)))
     :finally (return-from bail-if t)))

;; 
(defun max-char ()
  char-code-limit)

(defun ascii-string-p (string)
  ;; Allow STRING arg to be anything then filter out everything w/ string-not-null-p
  ;; (every #'ascii-char-p ())
  ;; (every #'ascii-char-p "í")
  (and (string-not-null-p string)
       (every #'ascii-char-p (the string string))))

(defun ascii-simple-string-p (string)
  (typep string 'simple-ascii-string))

(defun ascii-char-p (c) 
  (and (characterp c)
       (<= (char-code (the character c)) 127)))

(defun ascii-downcase (char-w-code)
  (let ((cwc
	 (etypecase char-w-code
	   ((unsigned-byte 7) char-w-code)
	   (standard-char     (char-code (the standard-char char-w-code)))
	   (char-code-integer (return-from ascii-downcase 
				(values (the char-code-integer char-w-code) nil)))
	   (character 
	    (return-from ascii-downcase (values (char-code char-w-code) nil))))))
    (values-list
     (or (and (<= 65 (the char-code-integer cwc) 90)
	      (list (+ (the (unsigned-byte 7) cwc) 32) t))
	 (list cwc nil)))))

(defun ascii-equal (char-a char-b)
  (and (ascii-char-p char-a)
       (ascii-char-p char-b)
       (eql (ascii-downcase char-a)
	    (ascii-downcase char-b))))

;;; :SOURCE chunga-1.1.1/read.lisp :WAS `controlp'
;;; What about an alias for `control-char-p'?
(defun ascii-control-p (char) 
  (and (characterp char)
       (or (<= 0 (char-code (the character char)) 31)
	   (= (char-code (the character char)) 127))))

(defun latin-1-char-p (char) 
  (and (characterp char)
       (<= (char-code (the character char)) 255)))


;; (and (notany #'characterp "") (every  #'characterp "")) ;=> t
;;
;; ascii-string-p
;; (type-of (coerce "0123456789abcdef" 'simple-base-string)) => (SIMPLE-BASE-STRING 16)
;; 
;; (aref (the (SIMPLE-BASE-STRING 16) (coerce "0123456789abcdef" 'simple-base-string)) 3)
;;
;;; :NOTE :SEE :FILE mon-systems/arrays.lisp  `string-ascii-to-byte-array'
;;;       :SEE :FILE mon-systems/types.lisp `simple-iso-latin-1-string'
(defun latin-1-string-p (string)
  (and (stringp string)
       (or (string-empty-p (the string string))
	   (every #'latin-1-char-p (the string string)))))

(defun latin-1-simple-string-p (string)
  (and (latin-1-string-p string) 
       (simple-string-p (the string string))))

;; :SOURCE slime-20101207-cvs/swank.lisp :WAS `casify-char'
(defun char-invert-case-maybe (char-to-invert &key (case :preserve))
  (declare (type character char-to-invert))
  (unless (memq case '(:preserve :upcase :downcase :invert))
    (setf case :preserve))
  ;; :WAS (ecase (readtable-case *readtable*)
  (case case
    (:preserve char-to-invert)
    (:upcase   (char-upcase char-to-invert))
    (:downcase (char-downcase char-to-invert))
    (:invert (if (upper-case-p char-to-invert)
                 (char-downcase char-to-invert)
                 (char-upcase char-to-invert)))))

(defun char-for-readtable-case (case-frob-char &optional readtable)
  (declare (type character case-frob-char)
           ((or null readtable) readtable))
  (string-invert-case case-frob-char 
                      :case (or 
                             (and readtable 
                                  (or 
                                   (and (readtablep readtable)
                                        (readtable-case readtable))
                                   (simple-error-mon :w-sym  'match-readtable-case
                                                     :w-type 'function
                                                     :w-spec "arg READTABLE not `readtablep'"
                                                     :w-got  readtable
                                                     :w-type-of readtable
                                                     :signal-or-only nil)))
                             (readtable-case (or readtable *readtable*)))))

#+sbcl 
(defun char-length (mb-char)
  (declare ((or character code-point) mb-char))
  (sb-impl::char-len-as-utf8
   (typecase mb-char 
     (code-point mb-char)
     (character (char-code mb-char)))))

;; (sb-impl::string->utf8 "►" 0 1 0)
;; => #(226 150 186)
;; (char-length 
;;
;; (sb-impl::string->utf8 "abcÿ►◄λ" 0 3 0)
;; (ironclad:ascii-string-to-byte-array "abcÿ")
;; => #(97 98 99 255)
;; (length "abcÿ")
;; #xc3


;;; ==============================
;;; :CHARS-DOCUMENTATION
;;; ==============================

(fundoc 'char-numeric=
"Whether CHAR-X and CHAR-Y are numerically equal.~%~@
CHAR-X and CHAR-Y are each either of type `cl:character' or
`mon:char-code-integer', an error is signaled if not.~%~@
Return value is as if by `cl:values'.~%~@
When codepoints of CHAR-X and CHAR-Y are numerically = return value has the form:~%
 T, <CHARACTER-X|Y>~%~@
When codepoints of CHAR-X and CHAR-Y are not numerically =, return value has form:~%
 T, CHARACTER-X, CHARACTER-Y~%~@
In either case the above syntax <CHARACTER-*> indicates that return value is a
character designator produced as if by `cl:code-char' e.g.:~%~@
 \(code-char 9658\)
 => #\BLACK_RIGHT-POINTING_POINTER~%~@
:EXAMPLE~%
 \(char-numeric= #\\► 9658\)~%
 \(char-numeric= 9658 9658\)~%
 \(char-numeric= 9658 #\\►\)~%
 \(char-numeric= #\\◄ #\\►\)~%
 \(char-numeric= 9657 #\\►\)~%
 \(char-numeric= 9657 9658\)~%
 \(char-numeric= 9657 9658\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'ascii-downcase
"Return as if by `cl:values' the downcased `cl:char-code' of ASCII CHAR-W-CODE.~%~@
CHAR-W-CODE is a character or integer of type `mon:char-code-integer'.~%~@
Second value is t when CHAR-W-CODE was returned as a downcased value.~%~@
:EXAMPLE~%
  \(ascii-downcase 88\)~%
  \(ascii-downcase #\\X\)~%
  \(ascii-downcase #\\►\)~%
  \(ascii-downcase 233\)~%~@
:SEE-ALSO `mon:*whitespace-chars*', `mon:whitespace-char-p',
`mon:ascii-downcase', `mon:ascii-downcase', `mon:ascii-char-p',
`mon:ascii-string-p'.~%►►►")

(fundoc 'ascii-equal
"Return non-nil when CHAR-A and CHAR-B are `cl:eql' ASCII chars.~%~@
To account for case wrt the lisp reader comparison is made by first downcasing both 
CHAR-A and CHAR-B before testing for `cl:eql'.~%~@
:EXAMPLE~%
 \(ascii-equal #\\a #\\a\)
 \(ascii-equal #\\A #\\A\)
 \(ascii-equal #\\A #\\a\)
 \(ascii-equal #\\a #\\A\)~%~@
:SEE-ALSO :SEE-ALSO `mon:whitespace-char-p' `mon:ascii-string-p',
`mon:ascii-char-p', `mon:ascii-downcase', `mon:ascii-equal',
`mon:ascii-control-p', `mon:latin-1-char-p', `mon:latin-1-string-p'.~%►►►")

(fundoc 'ascii-string-p
"Whether every character in the simple-string STRING is `mon:ascii-char-p'.~%~@
:EXAMPLE~%
  \(ascii-string-p \"aeEiou\"\)
  \(ascii-string-p \"àéÉíóü\"\)~%~@
:SEE info node `(ansicl)Character Syntax'~%~@
:SEE-ALSO `mon:whitespace-char-p' `mon:ascii-string-p', `mon:ascii-char-p',
`mon:ascii-downcase', `mon:ascii-equal', `mon:ascii-control-p',
`mon:latin-1-char-p', `mon:latin-1-string-p'.~%►►►")

(fundoc 'ascii-char-p
"Whether character C is `cl:characterp' with char-codeless than or equal to 127.~%~@
:EXAMPLE~%
 \(ascii-char-p \"E\"\)~%
 \(ascii-char-p \"É\"\)~%~@
:SEE info node `(ansicl)Character Syntax'~%~@
:SEE-ALSO `mon:whitespace-char-p' `mon:ascii-string-p', `mon:ascii-char-p',
`mon:ascii-downcase', `mon:ascii-equal', `mon:ascii-control-p',
`mon:latin-1-char-p', `mon:latin-1-string-p'.~%►►►")

(fundoc 'ascii-control-p
  "Whether CHAR is characterp and a control character according to RFC 2616.~%~@
This is the range of ASCII characters in the range 0-31 and 127.~%~@
:EXAMPLE~%
 \(ascii-control-p #\\DC1\)~%
 \(ascii-control-p #\\US\)~%
 \(ascii-control-p #\\NEWLINE\)~%
 \(ascii-control-p #\\DEL\)~%
 \(ascii-control-p #\\SPACE\)~%~@
:SEE info node `(ansicl)Standard Characters'~%~@
:SEE-ALSO `mon:whitespace-char-p', `mon:char-length', `mon:ascii-char-p',
`mon:ascii-string-p', `mon:ascii-equal', `mon:ascii-downcase',
`mon:*whitespace-chars*', `mon:latin-1-char-p', `mon:latin-1-string-p'.~%►►►")

(fundoc 'latin-1-char-p
"Whether character C is `cl:characterp' with char-codeless than or equal to 255.~%~@
:EXAMPLE~%
 \(latin-1-char-p \"É\"\)~%
 \(latin-1-char-p \"E\"\)~%
 \(ascii-char-p \"E\"\)~%
 \(ascii-char-p \"É\"\)~%~@
:SEE info node `(ansicl)Character Syntax'~%~@
:SEE-ALSO `mon:whitespace-char-p' `mon:ascii-string-p', `mon:ascii-char-p',
`mon:ascii-downcase', `mon:ascii-equal', `mon:ascii-control-p',
`mon:latin-1-char-p', `mon:latin-1-string-p'.~%►►►")

(fundoc 'latin-1-string-p
"Whether every character in the simple-string STRING is `mon:latin-1-char-p'.~%~@
:EXAMPLE~%
 \(latin-1-string-p \"àéÉíóü\"\)~%
 \(ascii-string-p \"àéÉíóü\"\)~%~@
:SEE-ALSO `mon:whitespace-char-p' `mon:ascii-string-p', `mon:ascii-char-p',
`mon:ascii-downcase', `mon:ascii-equal', `mon:ascii-control-p'.~%►►►")

(fundoc 'whitespace-char-p
  "Return non-nil when char is a member of `*whitespace-chars*'~%~@
:EXAMPLE~%
 \(whitespace-char-p #\\a\)~%
 \(whitespace-char-p #\\tab\)~%~@
:SEE info node `(ansicl)Character Syntax'~%~@
:SEE-ALSO 
`cl:base-char-p', `cl:char-code-integer', `cl:char-code-limit' `cl:char-code',
`cl:code-char', `cl:char-name', `cl:name-char', `cl:char-upcase', `cl:char-downcase',
`cl:char-int', `cl:schar', `cl:digit-char', `cl:character', `cl:base-char', `cl:standard-char',
`cl:extended-char',`cl:standard-char-p', `cl:graphic-char-p',
`cl:alpha-char-p',`cl:digit-char-p', `cl:alphanumericp', `cl:upper-case-p', `cl:lower-case-p',
`cl:both-case-p', `cl:char=', `cl:char/=', `cl:char<', `cl:char>', `cl:char<=', `cl:char>=',
`cl:char-equal', `char-not-equal'.~%►►►")

(fundoc 'hexadecimal-char-p
"Whether MAYBE-HEX-CHAR is of type `mon:hexadecimal-char'~%~@
:EXAMPLE~%
 \(hexadecimal-char-p #\\f\)~%
 \(hexadecimal-char-p #\\F\)~%
 \(hexadecimal-char-p #\\G\)~%
 \(hexadecimal-char-p #\\0\)~%~@
:SEE-ALSO `mon:*hexadecimal-chars*', `mon:whitespace-char-p', `cl:base-char-p',
`cl:char-code-integer', `cl:char-code-limit' `cl:char-code', `cl:code-char',
`cl:char-name', `cl:name-char', `cl:char-upcase', `cl:char-downcase',
`cl:char-int', `cl:schar', `cl:digit-char', `cl:character', `cl:base-char',
`cl:standard-char', `cl:extended-char',`cl:standard-char-p',
`cl:graphic-char-p', `cl:alpha-char-p',`cl:digit-char-p', `cl:alphanumericp',
`cl:upper-case-p', `cl:lower-case-p', `cl:both-case-p', `cl:char=', `cl:char/=',
`cl:char<', `cl:char>', `cl:char<=', `cl:char>=', `cl:char-equal',
`char-not-equal'.~%►►►")

;; When optional arg stream is provided it should satisfy `streamp' and `open-stream-p'.
;; Signal an error if not.~%~@

(fundoc 'chars-not-in-string-p
        "Return T when none of the characters in CHAR-BAG are in STRING.~%~@
If a character in CHAR-BAG is in STRING return as if by vaules with first value
null and second the position of the character in STRING.~%~@
CHAR-BAG is a proper-list with each element satisfying `cl:characterp'.
STRING is a string.~%~@
Keyword :TEST is a function accepting one arg which returns non-nil when a
character in CHAR-BAG is present in STRING. Default is `cl:char='.~%~@
:EXAMPLE~%
 \(chars-not-in-string-p '\(#\\Q #\\E #\\D\) \"#package.\"\)~%
 \(chars-not-in-string-p '\(#\\Q #\\E #\\D\) \"#package.\" :test #'char-equal\)~%
 \(chars-not-in-string-p '\(#\\# #\\E #\\.\) \"#package.\"\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'char-to-string
  "Convert CHAR to a string containing that character.~%~@
CHAR may be of type character or `mon:char-code-integer'.
When optional arg stream is provided it should satisfy `streamp' and `open-stream-p'.
Signal an error if not.~%~@
:EXAMPLE~%
 \(char-to-string  #\\a\)~%
 \(char-to-string  97\)~%
 \(with-open-stream \(s \(make-string-output-stream\)\) 
   \(char-to-string 97 s\)
   \(char-to-string #\\\\b s\)
   \(char-to-string  99 s\)
   \(get-output-stream-string s\)\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `char-list-to-string', `string-to-char', `string-to-number',
`char-code-limit' `char-code', `code-char', `char-name', `name-char',
`char-upcase', `char-downcase', `char-int', `schar', `digit-char', `character',
`base-char', `standard-char', `extended-char',`standard-char-p',
`graphic-char-p', `alpha-char-p',`digit-char-p', `alphanumericp',
`upper-case-p', `lower-case-p', `both-case-p', `char=', `char/=', `char<',
`char>', `char<=', `char>=', `char-equal', `char-not-equal+'.~%►►►")

(fundoc 'char-code-integer-to-string
"Convert CHAR-CODE-INT to string.~%~@
CHAR-CODE-INT should satisfy `mon:char-code-integer-p'. 
Signal an error if not.~%~@
When optional arg stream is provided it should satisfy `streamp' and `open-stream-p'.
Signal an error if not.~%~@
:EXAMPLE~%
 \(char-code-integer-to-string 9658\)~%
;; Following signals an error:~@
 (char-code-integer-to-string -1)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'char-code-integer-to-char
"Convert CHAR-CODE-INTEGER-TO-CHAR to an object of type `cl:character'.~%~@
When argument is `cl:characterp' return it.
When argument is `mon:char-code-integer-p' return its `code-char' value.
If neither of the above constraints is satsified signal an error.~%~@
When keyword W-NO-ERROR is non-nil do not signal an error, instead return
argument CHAR-CODE-INTEGER-TO-CHAR.
:EXAMPLE~%
 \(char-code-integer-to-char 9658\)~%
 \(char-code-integer-to-char #\\►\)~%
 \(char-code-integer-to-char nil\)~%
 \(char-code-integer-to-char \"string\"\)~%
 \(char-code-integer-to-char \"string\"  :w-no-error t\)~%~@
:SEE-ALSO `mon:char-code-integer-p', `mon:char-code-integer-to-string',
`mon:char-to-string', `char-list-to-string', `mon:char-code-integer'.~%►►►")

(fundoc 'char-list-to-string
"Convert charcters of CHAR-LIST to string.~%~@
CHAR-LIST should satisfy `mon:each-a-character-p'. Signal an error if not.~%~@
Optional arg STREAM is a destination stream default string to.~%~@
:EXAMPLE~%
 \(char-list-to-string '\(#\\a #\\b 99\)
 \(char-list-to-string '\(#\\a #\\b #\\c\)\)
;; (char-list-to-string (list #\a 98 #\c 9658))
;; (char-list-to-string (list #\a #\b #\c))
;; (char-list-to-string (list 97 97 99 9658))

 (with-open-stream (s (make-string-output-stream))
   (char-list-to-string '(#\a 98 #\c 9658) s)
   (char-list-to-string '(#\a #\b #\c)     s)
   (char-list-to-string '(97 98 99 9658)   s)
   (get-output-stream-string s))
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'max-char
 "The upper exclusive bound on values produced by CHAR-CODE~%~@
:EXAMPLE~%
 \(max-char\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `char-code-limit' `char-code', `code-char', `char-name', `name-char',
`char-upcase', `char-downcase', `char-int', `schar', `digit-char', `character',
`standard-char-p', `graphic-char-p', `alpha-char-p',`digit-char-p',
`alphanumericp', `upper-case-p', `lower-case-p', `both-case-p', `char=',
`char/=', `char<', `char>', `char<=', `char>=', `char-equal',
`char-not-equal+'.~%►►►")

(fundoc 'char-for-readtable-case
"Return CASE-FROB-CHAR according to the `cl:readtable-case' of READTABLE~%~@
When optional arg READTABLE is provided it should satisfy `cl:readtable', signal
an error if not. When ommitted default to value of `cl:*readtable*'.~%~@
:EXAMPLE~%
 \(readtable-case *readtable*\)~%
 \(char-for-readtable-case #\\a\)~%
 \(char-for-readtable-case #\\A\)~%
 \(char-for-readtable-case #\\SPACE\)~%~@
:SEE-ALSO `char-invert-case-maybe', `mon:string-invert-case',
`mon:string-for-readtable-case', `mon:read-symbol-name-preserving-case',
`mon:read-symbol-name-preserving-case-if', `cl:char-downcase', `cl:char-upcase',
`cl:upper-case-p', `cl:lower-case-p''.~%►►►")

(fundoc 'char-invert-case-maybe
"Convert CHAR-TO-INVERT accoring to MODE of `cl:readtable-case' for `cl:*readtable*'.~%~@
Keyword CASE is a case sensitivity mode, when non-nil it should be valid value per:~%
  \(setf \(readtable-case <READTABLE\) MODE\)~%~@
Possible arguments for CASE are:~%
 :preserve :upcase :downcase :invert~%~@
Default is :preserve. If CASE is not a member of the above enumeration it
defaults to :preserve.~%~@
:EXAMPLE~%
 \(char-to-invert #\\A\)~%
 \(char-to-invert #\\A :case :downcase\)~%
 \(char-to-invert #\\A :case :upcase\)~%
 \(char-to-invert #\\a :case :upcase\)~%~@
:SEE-ALSO `char-for-readtable-case', `mon:string-invert-case',
`mon:string-for-readtable-case', `mon:read-symbol-name-preserving-case',
`mon:read-symbol-name-preserving-case-if', `cl:char-downcase', `cl:char-upcase',
`cl:upper-case-p', `cl:lower-case-p''.~%►►►")

#+sbcl 
(setf (documentation 'char-length 'function)
      #.(format nil
"Return the number of bytes required to represent MB-CHAR.~%~@
:EXAMPLE~%
 \(char-length #\\►\)~%
 \(char-length #\\a\)~%
 \(char-length #\\FF\)~%~@
:NOTE UTF-8 centric.~%~@
:SEE-ALSO `char-code-limit' `char-code', `code-char', `char-name', `name-char',
`char-upcase', `char-downcase', `char-int', `schar', `digit-char', `character',
`base-char', `standard-char', `extended-char', `standard-char-p',
`graphic-char-p', `alpha-char-p',`digit-char-p', `alphanumericp',
`upper-case-p', `lower-case-p', `both-case-p', `char=', `char/=', `char<',
`char>', `char<=', `char>=', `char-equal', `char-not-equal+'.~%►►►"))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF

