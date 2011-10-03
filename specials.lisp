;;; :FILE-CREATED <Timestamp: #{2011-01-18T19:41:12-05:00Z}#{11032} - by MON>
;;; :FILE mon-systems/specials.lisp
;;; ==============================


(in-package #:mon)
;; *package*

(defmacro defconst (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defmacro defconst* (name type init &optional doc)
  `(progn 
     (declaim (type ,type ,name))
     (,(if (subtypep type '(or symbol number character))
	   'defconst
	   'defvar)
       ,name (the ,type ,init) ,doc)))

;;; :NOTE the `fundoc-*' stuff should come early. All other files evaluate it.
;; :SOURCE SICL/Code/Docstrings/docstrings-en.lisp :WAS `fmt'

(defun doc-set (name object-type string args) ;&rest args)
  (declare (type (or standard-method standard-generic-function (and symbol (not null)))  name) 
           (type (member variable type function generic method) object-type)
           ((or null string) string))
  (let ((doc-or-null 
         (if (null string)
             string
             (apply #'format nil `(,string ,@args)))))
    (ecase object-type
      (function
       (setf (documentation (fdefinition name) object-type) 
             (setf (documentation name object-type) doc-or-null)))
      (variable 
       (locally (declare (special name))
         (setf (documentation name object-type) doc-or-null)))
      (type 
       (setf (documentation name object-type) doc-or-null))
      (method
       (setf (documentation name t) doc-or-null))
      (generic
       (setf (documentation name t) doc-or-null)))))

(defun generic-doc (function-designator &optional doc-string &rest args)
  (when (and doc-string
             (typep function-designator 'standard-generic-function))
    (doc-set function-designator 'generic doc-string args)))

(defun method-doc (generic-function-designator qualifiers specializers &optional doc-string &rest args)
  (when doc-string
    (let ((found-method (find-method generic-function-designator qualifiers specializers nil)))
      (when (and found-method (typep found-method 'standard-method))
        (doc-set found-method 'method doc-string args)))))

(defun fundoc (name &optional string &rest args)
  (declare (type symbol name) ((or null string) string))
  (doc-set name 'function string args))

(defun vardoc (name &optional string &rest args)
  (declare (type symbol name)
           (special name) 
           ((or null string) string))
  (doc-set name 'variable string args))

;;; :SOURCE mcclim/Apps/Scigraph/dwim/extensions.lisp
(defun type-specifier-p (object)
  ;; A somewhat consful implementation, but entirely portable.
  (let ((test #'(lambda (x) (typep 't x))))
    (when (or (symbolp object) (listp object))
      (multiple-value-bind (v errorp) (ignore-errors (funcall test object))
	(declare (ignore v))
	(not errorp)))))
  
;; :NOTE Using mon:type-specifier-p to verify NAME is valid type but that
;; function is not yet in the environment when compiling a fresh system
;; Make sure not calls to this function occur before :FILE types.lisp is present.
(defun typedoc (name &optional string &rest args)
  (declare (type symbol name) 
           ((or null string) string))
  (when (type-specifier-p name)
    (doc-set name 'type string args)))


;;; ==============================
;;; :VARIABLES-CONSTANTS
;;; ==============================

(defvar *user-name*  ())

(defvar *search-path* ())

(defvar *default-class-documentation-table* (make-hash-table))

(defconst* *default-pathname-directory-ignorables* list
  '(".git" ".bzr" ".hg" ".svn" "_darcs" "RCS" "CVS" "rcs" "cvs" "lost+found"))

(defconst* *default-pathname-filename-ignorables* list
  ;; other possible values
  ;; "README" "CHANGELOG" "ChangeLog" "TAGS" "COPYING"
  '(".gitignore" ".hgignore" ".bzrignore"
    ".BridgeCache" ".BridgeCacheT" "Thumbs.db"))

(defconst* *standard-test-functions* list
  (list 'eq     #'eq
        'eql    #'eql
        'equal  #'equal
        'equalp #'equalp))

(defconst* *error-table* list
  (loop 
     :for sym :in '(function macro  variable method class condition)
     :collect (cons sym (concatenate 'string ":" (string sym))) into rslt
     :finally (return (append rslt (list (cons nil ":LOCUS"))))))

;; :SOURCE ecl-11.1.1/src/clos/inspect.lsp :WAS `*valid-documentation-types*'
(defconst* *documentation-types* list
  '(compiler-macro function method-combination
    setf structure type variable
    ;; :NOTE t is a valid <DOC-TYPE> for any docstring method specialized
    ;; on <THING> itself e.g. 
    ;;  (defmethod documentation ((x <THING>) (doc-type (eql 't)))
    ;; But, this prob. isn't particularly useful when using this variable as a
    ;; lookup table for verifying validity...
    ;;
    ;; t 
    ;;
    ;; SBCL specific?
    ;; package-doc-string
    ))

;;; :NOTE multi-byte char functions defined in:
;;; :SEE :FILE sbcl/src/code/external-formats/mb-util.lisp 
;; (eval-when (:compile-toplevel :
(defconst* *whitespace-chars* list
  '(#\SPACE 
    #\NEWLINE
    #\TAB 
    #\RETURN  
    ;; #\LINEFEED == #\NEWLINE ???
    #\NO-BREAK_SPACE
    #\PAGE
    #\VT
    ;; #\NULL ASCII (0, #o0, #x0) ???
    ;; #\NUL ;; ASCII (char-name #\NULL) (char-code #\NULL) (code-char 0) (princ #\NULL)
    ))

(defconst* *hexadecimal-chars* list 
 '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
   #\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f)) 

(defconst* *vowel-chars* list
  '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U))

;; :SOURCE cl-data-format-validation-20101006-git/validation.lisp
(defconst* *roman-numeral-map* list
  '(("M"  . 1000) ("CM" . 900) ("D"  . 500) ("CD" . 400)
    ("C"  . 100)  ("XC" . 90)  ("L"  . 50)  ("XL" . 40)
    ("X"  . 10)   ("IX" . 9)   ("V"  . 5)   ("IV" . 4)
    ("I"  . 1)))

;; :SOURCE CLOCC-cllib/port/sys.lisp
(defconst* *time-zones* list
  '((5 "EDT" . "EST") (6 "CDT" . "CST") (7 "MDT" . "MST")
    (8 "PDT" . "PST") (0 "BST" . "GMT") (-2 "MET DST" . "MET")))

;; :SOURCE CLOCC-cllib/port/sys.lisp
(defconst* *month-names* (simple-array simple-string (12))
  (make-array 12 :initial-contents '("Jan" "Feb" "Mar" "Apr" "May" "Jun" 
				     "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

;; :SOURCE CLOCC-cllib/port/sys.lisp
(defconst* *week-days* (simple-array simple-string (7))
  (make-array 7 :initial-contents '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))


;; :SOURCE cl-docutils-20101006-git/utilities.lisp :WAS `*length-units*'
(defconst* *length-unit* list 
  '((:in . 1)
    (nil . 75)
    (:cm . 254/100)
    (:em . 72/10)
    (:ex . 10)
    (:px . 75)	;; assume 75 dpi
    (:%  . 75/8) ;;  100% is 800 pixels
    (:pt . 72)
    (:pc . 12/72)
    (:mm . 254/10)))

(defconst* *keyword-hash-inverted*  hash-table
  (let ((inverted-pairs '((:pathname-host      . :host)
                          (:pathname-device    . :device)
                          (:pathname-directory . :directory)
                          (:pathname-name      . :name)
                          (:pathname-type      . :type)
                          (:pathname-version   . :version)))
        (hash-pairs (make-hash-table)))
    (dolist (pairs inverted-pairs 
             (progn 
               (maphash #'(lambda (dest-key src-key)
                            (setf (gethash src-key hash-pairs) dest-key))
                        hash-pairs)
               hash-pairs))    
      (setf (gethash (car pairs) hash-pairs) (cdr pairs)))))


;;; ==============================
;;; :TODO Return a pretty-printed representation of global variables and their values.
;; (defun inspect-globals (&key print-lesser
;; "keyword PRINT-LESSER also returns the lesser vars

;;; :NOTE Many of these are used as defaults for `write-to-string's keywords
;; :READ
;; *read-base* 	
;; *read-eval* 	
;; *read-suppress*
;; *read-default-float-format*
;; *read-base*

;; :COMPILE
;; *compile-print*
;; *compile-verbose*

;; :COMPILE-LESSER
;; *compiler-print-variable-alist*
;; *compile-file-pathname*
;; *compile-file-truename*

;; :LOAD
;; *load-verbose*
;; *load-print*
;; *load-pathname*
;; *load-truename*

;; :PRINT
;; *print-case*
;; *print-array*
;; *print-base*
;; *print-radix*
;; *print-circle*
;; *print-escape*
;; *print-pretty*
;; *print-gensym*

;; :PRINT-LESSER
;; --- *print-length*
;; --- *print-level*
;; --- *print-lines*
;; --- *print-readably*
;; --- *print-right-margin*
;; --- *print-miser-width*
;; --- *print-pprint-dispatch*
;; 

;; *gensym-counter*
;; *random-state*
;; *macroexpand-hook*
;; *break-on-signals*

;; *features*
;; *modules*
;; *package*

;; :PATH
;; *default-pathname-defaults*

;; :STREAMS
;; *error-output*
;; *query-io*
;; *standard-input*
;; *standard-output*
;; *terminal-io*

;; :DEBUG
;; *debug-beginner-help-p*
;; *debug-condition*
;; *debug-help-string*
;; *debug-io*
;; *debug-print-variable-alist*
;; *debug-readtable*
;; *debugger-hook*
;; *flush-debug-errors*
;; *in-the-debugger*
;; *max-trace-indentation*
;; *print-location-kind*
;; *only-block-start-locations*
;; *show-entry-point-details*
;; *stack-top-hint*
;; *trace-encapsulate-default*
;; *trace-frame*
;; *trace-indentation-step*
;; *trace-output*
;; *trace-values*
;; *traced-fun-list*

;; :SBCL
;; *core-pathname*
;; *muffled-warnings*
;; *compile-progress*
;; *compiler-print-variable-alist*
;; *efficiency-note-cost-threshold*
;; *efficiency-note-limit*
;; *derive-function-types*
;; *enclosing-source-cutoff*
;; *evaluator-mode*
;; *exit-hooks*
;; *ed-functions*
;; *runtime-pathname*
;; *gc-run-time*
;; *inline-expansion-limit*
;; *init-hooks*
;; *inspected*
;; *intexp-maximum-exponent*
;; *invoke-debugger-hook*
;; *save-hooks*
;; *posix-argv*
;; *module-provider-functions*
;; *muffled-warnings*
;; *stepper-hook*
;; *stack-allocate-dynamic-extent*
;; *undefined-warning-limit*


;;; ==============================
;;; :SPECIALS-DOCUMENTATION
;;; ==============================

(fundoc 'doc-set
	"Set documentation with STRING for symbol with NAME.~%~@
NAME is a symbol designating a function, a type, or a special variable.~%~@
OBJECT-TYPE is a quoted symbol it should be one of:~%
 function type variable~%~@
Optional arg STRING is a docstring possibly with format control specs.~%~@
When STRING is ommitted the symbol with NAME will have its documentation property set to NIL.~%~@
ARGS are format control specs, these are ignored if STRING is ommitted.~%~@
:SEE-ALSO `mon:fundoc', `mon:vardoc', `mon:typedoc', `mon:classdoc'.~%▶▶▶")

(fundoc 'fundoc
"Set documentation for NAME to the format-control STRING with format-arguments ARGS.
NAME is a symbol designating a function.~%~@
Optional arg STRING is a docstring possibly with format control specs.~%~@
When STRING is ommitted the symbol with NAME will have its documentation property set to NIL.~%~@
ARGS are format control specs, these are ignored if STRING is ommitted.~%~@
Set as if by `cl:setf' the documentation properties for both:~%~@
 \(documentation \(fdefinition NAME\) 'function\)~%
 \(documentation \(fdefinition NAME\) 'function\)~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:doc-set', `mon:vardoc', `mon:typedoc', `mon:classdoc'.~%▶▶▶")

(fundoc 'vardoc
"Set documentation for NAME to the format-control STRING with format-arguments ARGS.~%~@
NAME is a symbol designating a special variable.~%~@
Optional arg STRING is a docstring possibly with format control specs.~%~@
When STRING is ommitted the symbol with NAME will have its documentation property set to NIL.~%~@
ARGS are format control specs, these are ignored if STRING is ommitted.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:doc-set' `mon:fundoc', `mon:typedoc', `mon:classdoc'.~%▶▶▶")


(fundoc 'type-specifier-p
        "Determine if OBJECT is a valid type specifier.~%~@
:EXAMPLE~%
 \(type-specifier-p 'mon:index\)~%
 \(type-specifier-p 'bubba\)~%~@
:SEE-ALSO `cl:typep', `cl:type-of', `cl:deftype'.~%▶▶▶")

(fundoc 'typedoc
"Set documentation for NAME to the format-control STRING with format-arguments ARGS.~%~@
NAME is a symbol designating a type.~%~@
Optional arg STRING is a docstring possibly with format control specs.~%~@
When STRING is ommitted the symbol with NAME will have its documentation property set to NIL.~%~@
ARGS are format control specs, these are ignored if STRING is ommitted.~%~@
:SEE-ALSO `mon:doc-set' `mon:fundoc', `mon:vardoc', `mon:classdoc'.~%▶▶▶")

(fundoc 'method-doc
"Set documentation for GENERIC-FUNCTION-DESIGNATOR with the format-control DOC-STRING and format-arguments ARGS.~%~@
Arg GENERIC-FUNCTION-DESIGNATOR may be a setf function.~%~@
When QUALIFERS is non-nil it is a quoted list with the the general format:~%
 '\(:after\) '\(:before\) '\(:around\)~%
When DOC-STRING is non-nil it is a documentation string and may be a `cl:format' control string.~%~@
When ARGS is non-nil the objects are processed as arguments to DOC-STRING.~%~@
Arglist has the following form:~%
 \(method-doc #'<SYMBOL> <QUALIFERS> <SPECIALIZERS> <DOCSTRING> <ARGS>\)~%
 \(method-doc #'\(setf <SYMBOL>\) <QUALIFERS> <SPECIALIZERS> <DOCSTRING> <ARGS>\)~%~@
:SEE-ALSO `generic-doc', `fundoc', `vardoc', `typedoc'.~%▶▶▶")

(fundoc 'generic-doc
        "Set documentation for FUNCTION-DESIGNATOR  with the format-control DOC-STRING and format-arguments ARGS.~%~@
Arg FUNCTION-DESIGNATOR may be a setf function.~%~@
When QUALIFERS is non-nil it is a quoted list with the the general format:~%
 '\(:after\) '\(:before\) '\(:around\)~%
When DOC-STRING is non-nil it is a documentation string and may be a `cl:format' control string.~%~@
When ARGS is non-nil the objects are processed as arguments to DOC-STRING.~%~@
:SEE-ALSO `method-doc', `fundoc', `vardoc', `typedoc'.~%▶▶▶")

;;; ==============================
;;; :SPECIALS-MACROS-DOCUMENTATION
;;; ==============================

(fundoc 'defconst
"Define symbol as a constant variable.~%~@
This is `defconstant' wrapped inside a macro.~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `cl:defconstant', `cl:defparameter', `cl:defvar' `define-constant'.~%▶▶▶")

(fundoc 'defconst*
"Define NAME as a typed constant.~%~@
NAME is a symbol to define as a constant.~%~@
TYPE is a type declaration.~%~@
INIT is the value to bind.~%~@
When evaluated NAME is `declaim'ed as being of TYPE, if TYPE is not `subtypep'
of {symbol|number|character} the variable is defined as if by `defvar', else
name is defined as if by `defconstant'.~%~@
:EXAMPLE~%
 \(macroexpand 
  '\(defconst* +week-days+ \(simple-array simple-string \(7\)\)
    \(make-array 7 :initial-contents '\(\"Mon\" \"Tue\" \"Wed\" \"Thu\" \"Fri\" \"Sat\" \"Sun\"\)\)\)\)~%~@
:NOTE Since constant redefinition must be the same under EQL, there
can be no constants other than symbols, numbers and characters~%~@
:SEE ANSI CL spec 3.1.2.1.1.3~%~@
:SEE info node `(ansicl) The Evaluation Model'~%~@
:SEE-ALSO `cl:defconstant'.~%▶▶▶")

;; (fundoc 'define-constant
;;   "Ensures that the global variable named by NAME is a constant with a value
;; that is equal under TEST to the result of evaluating INITIAL-VALUE.~%~@
;; Keyword TEST is a /function designator/. Default is `\cl:eql'.~%~@
;; If keyword DOCUMENTATION is given, it becomes the documentation string of the
;; constant.~%~@
;; Signal an error if NAME is already a bound non-constant variable.~%~@
;; Signal an error if NAME is already a constant variable whose value is not
;; equal under TEST to result of evaluating INITIAL-VALUE.~%~@
;; The sum affect is that when used in conjunction with `mon:defconst*' it should
;; be quite difficult to define a constant outside the bounds of what the ANIS spec
;; mandates.~%~@
;; :SEE (URL `http://www.sbcl.org/manual/Defining-Constants.html')~%~@
;; :SEE-ALSO `mon:defconst*', `mon:defconst', `cl:defconstant', `cl:defparameter',
;; `cl:defvar'.~%▶▶▶")


;;; ==============================
;;; :VARIABLES
;;; ==============================

(vardoc '*user-name*
"The value of system A consed pair of strings identifying the current-user.~%~@
Initially points to the namestring of file \"loadtime-bind\" in system's path as
per its `load-time-value'.~%~@
When bound as a consed pair, cons has the form:~%
 \(<OS-USER-NAME> . <USER-NICKNAME>\)~%~@
Value of car is the OS' user-name for current-user. Value of cdr is a nickname. ~%~@
The value at car is used to verify user identity.
The value at cdr is used when formatting output messages.
The value of this variable is at loadtime, e.g. _after_ \(asdf:oos 'load :mon\)
with `mon:username-for-system-var-bind'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(vardoc '*search-path*
  "List of paths to search.~%~@
Called by `mon:find-file-search-path'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(vardoc '*default-pathname-directory-ignorables*
"List of directory names which should not be searched.~%~@
When the return value of: 
 (file-namestring (cl-fad:pathname-as-file <PATHNAME>))
is a member of this list it will not \"satisfy the test\" for functions which
rely on the value of this variable to filter.~%~@
:EXAMPLE~%
 \(member \".bzr\" *default-pathname-directory-ignorables* :test 'string=\)~%
 \(member \".BZR\" *default-pathname-directory-ignorables* :test 'string=\)~%
 \(member \"cvs\" *default-pathname-directory-ignorables* :test 'string=\)~%
 \(member \"CVS\" *default-pathname-directory-ignorables* :test 'string=\)~%
:SEE-ALSO `mon:*default-pathname-filename-ignorables*', `file-namestring',
`cl-fad:pathname-as-file''.~%▶▶▶")

(vardoc '*default-pathname-filename-ignorables*
        "List of strings designating filenames which should be ignored with file/directory traversals.~%~@
:EXAMPLE~%
 \(member \".gitignore\" *default-pathname-filename-ignorables* :test 'string=\)~%
 \(member \".GITIGNORE\" *default-pathname-filename-ignorables* :test 'string=\)~%
 \(member \(file-namestring \(make-pathname :directory '\(:absolute \"some\" \"path\" \"to\" \"repo\"\) :name \".gitignore\"\)\)
        *default-pathname-filename-ignorables* :test 'string=\)~%~@
:SEE-ALSO `mon:*default-pathname-directory-ignorables*'.~%▶▶▶")

;; (file-namestring (make-pathname :directory '(:absolute "some" "path" "to" "repo") :name ".gitignore"))
(vardoc '*default-class-documentation-table*
"Hash-table of mapping class-names to documentation strings.~%~@
:EXAMPLE~%
 \(hash-table-p '*default-class-documentation-table*\)~%~@
:SEE-ALSO `mon:classdoc', `mon:documented-class-slot-doc',
`mon:documented-class-with-docs', `mon:make-documented-class',
`mon:documented-class-verify-init',
`mon:*default-class-documentation-table*'.~%▶▶▶")

(vardoc '*whitespace-chars*
 "A list of whitespace chars.~%~@
List values include:~%
- #\\NEWLINE    \\n  LINE FEED \(LF\)       \(10,  #o12,  #xa\)
- #\\PAGE       \\f  FORM FEED \(FF\)       \(12,  #o14,  #xc\)
- #\\TAB        \\t  CHARACTER TABULATION \(9,   #o11,  #x9\)
- #\\RETURN     \\r  CARRIAGE RETURN      \(13,  #o15,  #xd\)
- #\\VT         \\v  LINE TABULATION      \(11,  #o13,  #xb\)
- #\\NO-BREAK_SPACE  NON-BREAKING SPACE  \(160, #o240, #xa0\)
  ;; &nbsp NBSP \"C-x 8 SPC\" => ~C
- #\\SPACE           SPC                  \(32, #o40, #x20\)~%~@
:NOTE This list differs from the specs definition in that all but #\\SPACE are
considered to be non-graphic.~%~@
:NOTE Also, that of the standard characters, newline is non-graphic and all
others are graphic.~%~@
:SEE \(info \"\(ansicl\)Character Categories\"\)~%~@
:SEE \(info \"\(ansicl\)Standard Characters\"\)~%~@
:SEE-ALSO `whitespace-char', `whitespace-char-p', `base-char-p',
`char-code-integer', `graphic-char-p', `sb-int:*default-init-char-form*',
`sb-int:bell-char-code', `sb-int:backspace-char-code', `sb-int:tab-char-code',
`sb-int:line-feed-char-code', `sb-int:form-feed-char-code',
`sb-int:return-char-code', `sb-int:escape-char-code',
`sb-int:rubout-char-code'.~%▶▶▶" #\NO-BREAK_SPACE)

(vardoc '*hexadecimal-chars*
"List of characters which may comprise a string representation of a hexadecimal number.~%~@
Alphabetic characters of list are case-insensitive.~%~@
:EXAMPLE~%~@
 \(member #\\a *hexadecimal-chars*\)~%
 \(member #\\A *hexadecimal-chars*\)~%
 \(member #\\A *hexadecimal-chars* :test #'char-equal\)~%
 \(member #\\A *hexadecimal-chars* :test #'char=\)~%
 \(memq   #\\A *hexadecimal-chars*\)~%
 \(eq     #\\A #\\A\)~%
 \(equalp #\\A #\\a\)~%
 \(eq     #\\A #\\a\)~%
 \(equal  #\\A #\\a\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(vardoc '*roman-numeral-map*
"An alist of strings mapping to Roman number interger values.~%~@
:EXAMPLE~%
 \(assoc \"M\" *roman-numeral-map* :test #'string=\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(vardoc '*week-days*
"A simple-array of the names of the days of the week.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:*month-names*', `mon:*week-days*', ``mon:*time-zones*',
`mon:current-time'.~%▶▶▶")

(vardoc '*month-names* 
"A simple-array of the names of the months.~%~@
:EXAMPLE~%
 \(aref mon:*month-names* 11\)~%~@
:SEE-ALSO `mon:*month-names*', `mon:*week-days*', `mon:*time-zones*',
`mon:current-time'.~%▶▶▶")

(vardoc '*time-zones*
"Keyed index to string representations of the time zones.~%~@
:EXAMPLE~%
 \(mon:assq 5 mon:*+time-zones*\)~%~@
:SEE-ALSO `mon:*month-names*', `mon:*week-days*', `mon:current-time'.~%▶▶▶")

(vardoc '*length-unit*
 "Conversion from various units to inches~%~@
:EXAMPLE~%
 \(assoc :cm *length-unit*\)~%~@
:SEE-ALSO `mon:length-unit-get', `mon:length-unit-convert'.~%▶▶▶")

(vardoc '*documentation-types*
"List of types valid as <DOC-TYPE> arg for \(setf documentation\).~%
 \(setf \(documentation <OBJECT> <DOC-TYPE>\) <NEW-VALUE>\)~%~@
Following enumeration of `cl:documentation's <DOC-TYPE> from the dpans spec:~%~%
The nature of the documentation string returned depends on the
<DOC-TYPE>, as follows:~%
 - compiler-macro
   Return documentation string of the compiler macro whose name
   is function name X.~%
 - function
   If X is a function name, return documentation string of
   function, macro, or special operator whose name is X.~%
   If X is a function, return documentation string associated
   with X.~%
 - method-combination~%
   If X is a symbol, return documentation string of the method
   combination whose name is X.~%
   If X is a method combination, return documentation string
   associated with X.~%
 - setf
   Return documentation string of setf expander whose name
   is symbol X.~%
 - structure
   Return documentation string associated with structure
   name X.~%
 - t
   Return documentation string specialized on class of
   argument X itself.  For example, if X is a function, a
   documentation string associated with function X is returned.~%
 - type
   If X is a symbol, return documentation string of class
   whose name is symbol X, if there is such a class.
   Otherwise, return documentation string of the type which is the
   type specifier symbol X.~%
   If X is a structure class or standard class, return
   documentation string associated with class X.~%
 - variable
   Return documentation string of dynamic variable or
   constant variable whose name is symbol X;~%
A conforming implementation or a conforming program may extend the set
of symbols that are acceptable as <DOC-TYPE>.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE info node `(ansicl)documentation'~%~@
:SEE :FILE sbcl/src/pcl/documentation.lisp
:SEE-ALSO `mon:fundoc', `mon:doc-set', `cl:describe'.~%▶▶▶")

(vardoc '*keyword-hash-inverted*
        "Inverted hash-table of keywords appearing in the COMMON-LISP package.~%~@
:EXAMPLE~%
 \(gethash :name *keyword-hash-inverted*\)~%
 \(gethash :pathname-name *keyword-hash-inverted*\)~%~@
:SEE-ALSO `mon:keyword-property-to-function', 
`mon:pathname-components-funcallable-pairs'.~%▶▶▶")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
