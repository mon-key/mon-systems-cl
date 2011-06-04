;;; :FILE-CREATED <Timestamp: #{2011-04-21T19:04:29-04:00Z}#{11164} - by MON>
;;; :FILE mon-systems/types.lisp
;;; ==============================


(in-package #:mon)
;; *package*


;;; ==============================
;;; :TYPES-DEFINITIONS
;;; ==============================

(deftype not-null ()
  '(not null))

(deftype not-t ()
  '(or null (not boolean)))

(deftype not-boolean ()
  '(not boolean))

(deftype boolean-integer ()
  '(mod 2))

(deftype boolean-or-boolean-integer ()
  '(or boolean boolean-integer))

(defun not-boolean-p (object)
  (typep object 'not-boolean))

(defun boolean-integer-p (object)
  (typep object 'boolean-integer))

(defun not-boolean-integer-p (object)
  (typep object 'not-boolean-integer))

(deftype not-boolean-integer ()
  '(not boolean-integer))

(deftype standard-test-function ()
  ;; *standard-test-functions*
  ;; (typep (function eql) 'standard-test-function)
  '(and not-null not-t
    (and (or function symbol)
     (satisfies standard-test-function-p))))

(deftype string-or-symbol ()
  '(or symbol string))

(deftype string-or-symbol-not-boolean ()
  '(and not-boolean (or symbol string)))

(deftype string-or-symbol-not-empty-nor-boolean ()
  '(and not-boolean (or symbol string-not-empty)))

;; :SOURCE sbcl/src/code/stream.lisp
(declaim (inline vector-with-fill-pointer))
(defun vector-with-fill-pointer-p (object)
  (and (vectorp object)
       (array-has-fill-pointer-p object)))

;; :SOURCE sbcl/src/code/stream.lisp
(deftype string-with-fill-pointer ()
  `(and (or (vector character) (vector base-char))
        (satisfies vector-with-fill-pointer-p)))

(deftype stream-or-boolean ()
  '(or stream boolean))

(deftype stream-or-boolean-or-string-with-fill-pointer ()
  '(or stream-or-boolean string-with-fill-pointer))

(deftype logical-pathname-designator ()
  '(and pathname logical-pathname))

;; :NOTE Should `pathname-designator', `filename-designator', and
;; `pathname-or-namestring' fail for the empty string, e.g.:
;;  (or string-not-empty ...)
;; No. b/c of this:
;;  (make-pathname :host "") => #P"" 
;;  (namestring #P"") => ""
;;  (pathname "") => #P""
(deftype pathname-or-namestring ()
  '(or string pathname))
;;
;; :SOURCE sbcl/src/code/deftypes-for-target.lisp
(deftype pathname-designator ()
  '(or string pathname file-stream)) ;; logical-pathname
;;
;; :SOURCE sbcl/src/code/deftypes-for-target.lisp :WAS `filename'
(deftype filename-designator () 
  '(or string pathname)) ;; logical-pathname


;;; ==============================
;;;
;;; ,---- #lisp 2011-01-07
;;; | 
;;; | <stassats> the most juicy part from ansi-tests is that: 
;;; |              (stringp (make-array 0 :element-type 'nil)) => T
;;; | <stassats> (subtypep '(array nil (*)) 'string) => T 
;;; | <stassats> strings are clearly not arrays with element-type NIL, since
;;; | 	        nothing can be a subtype of NIL
;;; | <stassats> while nil is a SUBTYPEP of everything
;;; | <stassats> "A string is a specialized vector whose elements are of type
;;; | 	         CHARACTER or a subtype of type CHARACTER."
;;; | <stassats> NIL is a subtype of CHARACTER
;;; |
;;; `----
;;;
;;; Apparently it is also true of simple-string:
;;;  (simple-string-p (make-array 0 :element-type 'nil)) => T
;;;
;;; :SEE (URL `http://common-lisp.net/project/ansi-test/')
;;;
;;; ==============================
;;; STRING Class Precedence List:
;;; `string', `vector', `array', `sequence', `t'
;;; (make-array 8 :element-type 'character :fill-pointer 0 )
;;;
;;; (let ((str (make-array 8 :element-type 'character :fill-pointer 0 )))
;;;   (dolist (i '(#\a #\b #\c #\d #\e #\f #\g #\h #\i))
;;;     (vector-push-extend i str))
;;;   (list :string str 
;;; 	:fill-pointer (fill-pointer str)
;;; 	:adjustable   (adjustable-array-p str)
;;; 	:type-of     (type-of str)
;;; 	:is-string   (stringp str)))
;;; 
;;; SIMPLE-STRING is an object of type:
;;;  (simple-array character (size))
;;; Supertypes: `simple-string', `string', `vector', `simple-array', `array', `sequence', `t'
;;; A simple-string has the following constraints 
;;;  - it is of one dimension
;;;  - it has element-type  character (or subtype of)
;;;  - it does not have a fill pointer
;;;  - it is not adjustable
;;;  - it is not displaced
;;;
;;; (simple-string-p (make-string 8 :initial-element #\null))
;;;
;;; (let ((str "bubba"))
;;;      (array-has-fill-pointer-p str))
;;;      (adjustable-array-p str))
;;;
;;; (let ((str (make-array 18 :element-type 'character 
;;;                        :adjustable t
;;;                        :fill-pointer 0
;;;                        )))
;;;   (loop 
;;;      for chars from 65 to 90
;;;      do (vector-push-extend (code-char chars) str))
;;;   (list :as-string (type-of str)
;;;         :as-simple-string (type-of (copy-seq str))))
;;;
;;; ==============================

(deftype string-or-null () 
  '(or 
    null
    (array character (*))   ;; (vector character)
    (array nil (*))         ;; (vector nil)
    (array base-char (*)))) ;; base-string

;; This type doesn't really make much sense b/c every string is by definition of
;; type not-null. However, we keep it here for consistency with the other
;; `string-*' types and under the assumption (perhaps misguided) that the
;; compiler might benefit from checking if object is null _before_ checking if
;; it is `stringp'.
(deftype string-not-null ()
  '(and not-null string))

(deftype string-empty ()
  '(and string
    (or 
     (array character (0)) ;; <- (vector character 0)
     (array nil (0))	      ;; <- (vector nil 0)
     (array base-char (0))))) ;; <- (base-string 0)

(deftype string-not-empty ()
  '(and string (not string-empty)))

(deftype string-null-or-empty ()
  '(or null (and string string-empty)))

(deftype string-null-empty-or-all-whitespace ()
  '(and string-or-null 
    (satisfies string-null-empty-or-all-whitespace-p)))

(deftype string-not-null-or-empty ()
  '(and not-null string-not-empty))

(deftype string-not-null-empty-or-all-whitespace ()
  '(and
    not-null
    string-not-empty
    (not string-null-empty-or-all-whitespace)))

;; (deftype string-not-empty-or-all-whitespace ()
;; (and string string-not-empty 
;; string-not-empty-or-all-whitespace-p

(deftype string-of-length-1 ()
  '(and string
    (array character (1))))

(deftype simple-string-or-null ()
  '(or 
    null
    ;; simple-string
    (simple-array character (*)) 
    (simple-array nil       (*))
    ;; (simple-base-string *)
    (simple-array base-char (*)))) 

(deftype simple-string-not-null ()
  '(and not-null simple-string))

(deftype simple-string-empty ()
  '(and simple-string
    (or 
     (simple-array character (0))
     (simple-array nil (0))
     (simple-array base-char (0))))) ;; <- (simple-base-string 0)

(deftype simple-string-null-or-empty ()
  '(or null simple-string-empty))

(deftype simple-string-not-empty ()
  '(and simple-string (not simple-string-empty)))
  
(deftype simple-string-not-null-or-empty ()
  '(and simple-string-not-null simple-string-not-empty))

(deftype simple-string-of-length-1 ()
  '(and simple-string (simple-array character (1))))

(deftype ascii-string ()
  '(satisfies ascii-string-p))

(deftype simple-ascii-string ()
  '(and ascii-string simple-string))

;;; :NOTE :SEE :FILE mon-systems/arrays.lisp `string-ascii-to-byte-array'
;;;       :SEE :FILE mon-systems/types.lisp  `simple-iso-latin-1-string'
;;;       :SEE :FILE mon-systems/chars.lisp `latin-1-string-p'
(deftype simple-latin-1-string ()
  '(satisfies latin-1-simple-string-p))

;; (and (notany #'characterp "") (every  #'characterp ""))  => t

(deftype integer-or-null () 
  '(or integer null))

(deftype standard-char-or-null ()
  `(or null standard-char))

(deftype digit-char-0-or-1 ()
  `(satisfies digit-char-0-or-1-p))

(deftype string-all-digit-char-0-or-1 ()
  `(satisfies string-all-digit-char-0-or-1-p))


;; :NOTE Keep sequencep before `list-proper-p'
(declaim (inline sequencep))
(defun sequencep (object)
  (declare (optimize (speed 3)))
  (the boolean 
    (typep object 'sequence)))

;;; ==============================
;;; :PASTED-BY PJB 2011-04-11
;;; :SOURCE (URL `http://paste.lisp.org/+2LMR/8')
;;; (defun proper-list-p (object)
;;;   "Whether OBJECT is a proper list
;;; :NOTE Terminates with any kind of list, dotted, circular, etc."
;;;   (labels ((proper (current slow)
;;;              (cond ((null current)       t)
;;;                    ((atom current)       nil)
;;;                    ((null (cdr current)) t)
;;;                    ((atom (cdr current)) nil)
;;;                    ((eq current slow)    nil)
;;;                    (t                    (proper (cddr current) (cdr slow))))))
;;;     (and (listp object) (proper object (cons nil object)))))
;;; ==============================
;;;
;;; :SOURCE alexandria/lists.lisp :WAS `proper-list-p'
(declaim (inline list-proper-p))
(defun list-proper-p (object)
  (declare (inline sequencep)
           (optimize (speed 3)))
  (unless (sequencep object) (return-from list-proper-p (the boolean nil)))
  (locally 
      (declare (sequence object))
    (the boolean
      (cond ((not object) t)
            ((consp object)
             (do ((fast object (cddr fast))
                  (slow (cons (car object) (cdr object)) (cdr slow)))
                 (nil)
               (unless (and (listp fast) (consp (cdr fast)))
                 (return (the boolean (and (listp fast) (not (cdr fast))))))
               (when (eq fast slow)
                 (return (the boolean nil)))))
            (t nil)))))

;;; :SOURCE alexandria/lists.lisp
(deftype proper-list ()
  `(and list (satisfies list-proper-p)))

(deftype proper-plist ()
  `(and list (satisfies plist-proper-p)))

(deftype proper-plist-not-null ()
  `(and not-null proper-plist))

(deftype dotted-list ()
  `(and not-null cons (satisfies list-dotted-p)))

;;; :SOURCE alexandria/lists.lisp
(deftype circular-list ()
  `(satisfies list-circular-p))

(deftype proper-list-not-null ()
  `(and not-null proper-list))

(deftype proper-sequence ()
  '(or vector
    (and list proper-list)))

(declaim (inline sequence-proper-p))
(defun sequence-proper-p (sequence-putatively-proper)
  (declare (inline list-proper-p 
                   sequencep)
           (optimize (speed 3)))
  (unless (sequencep sequence-putatively-proper)
    (return-from sequence-proper-p (the boolean nil)))
  (locally 
      (declare (sequence sequence-putatively-proper))
    (the boolean
      (typep sequence-putatively-proper 'proper-sequence))))

(deftype each-a-sequence () 
  '(and sequence (satisfies each-a-sequence-p)))

(deftype each-a-sequence-proper ()
  '(and sequence (satisfies each-a-sequence-proper-p)))

(deftype each-a-sequence-proper-or-character ()
  '(and sequence (satisfies each-a-sequence-proper-or-character-p)))

;;; BAD BAD BAD BAD BAD BAD BAD BAD BAD BAD BAD BAD! 
;;; Left as a reminder that `list-of-len' is not reasonably possible.
;;; (deftype list-of-len (&optional n) 
;;;   (if (zerop n) 
;;;       'null
;;;       `(cons t (list-of-len ,(1- n)))))

(deftype each-a-string ()
  `(and proper-list-not-null (satisfies each-a-string-p)))

(deftype each-a-string-or-null ()
  '(and list (satisfies each-a-string-or-null-p)))

(deftype each-a-simple-string ()
  '(and proper-list-not-null (satisfies each-a-simple-string-p)))

(deftype each-a-string-of-length-1 ()
  '(and proper-list-not-null (satisfies each-a-string-of-length-1-p)))

(deftype each-a-simple-string-of-length-1 ()
  '(and proper-list-not-null (satisfies each-a-simple-string-of-length-1-p)))

;; (deftype octet-vector (&optional (size '*))
;;   `(simple-array (unsigned-byte 8) (,size)))

;;; `sb-impl::bit-array-same-dimensions-p'
(deftype bool-vector (&optional (size '*))
  `(simple-array bit (,size)))

(deftype bit-vector-octet ()
  '(bool-vector 8))

(deftype byte-array (&optional length)
  (let ((length (or length '*)))
    `(or (simple-array unsigned-byte-8 (,length))
         (%byte-vector ,length))))

(deftype %byte-vector (vec-length)
  `(and (simple-vector ,vec-length)
        (satisfies %byte-vector-each-an-unsigned-byte-8)))

(declaim (inline %byte-vector-each-an-unsigned-byte-8))
(defun %byte-vector-each-an-unsigned-byte-8 (byte-array)
  (declare (simple-vector byte-array)
           (optimize (speed 3)))
  (loop 
     for chk-byte upfrom 0 below (array-dimension byte-array 0)
     always (typep (svref byte-array chk-byte ) 'unsigned-byte-8)))

;;; :SOURCE sbcl/src/code/early-extensions.lisp
(deftype index ()
  #-sbcl '(integer 0 #x1FFFFFFD)
  #+sbcl `(integer 0 ,array-dimension-limit))

(deftype index-plus-1 ()
  #-sbcl '(integer 0 #x1FFFFFFE)
  #+sbcl `(integer 0 ,(1+ array-dimension-limit)))

;; :SOURCE sbcl/src/code/early-extensions.lisp
;; :NOTE most-positive-fixnum is specified as being:
;;  (and (<= (1- (expt 2 15)) <M-P-F>)
;;       (<= array-dimension-limit <M-P-F>))
;;
(deftype index-or-minus-1 ()
  #+sbcl `(integer -1 ,array-dimension-limit)
  #-sbcl `(integer -1  ,(- most-positive-fixnum 2)))

;;; :SOURCE alexandria/types.lisp
(deftype array-index (&optional (length array-dimension-limit))
  `(integer 0 (,length)))

;;; :SOURCE alexandria/types.lisp
(deftype array-length (&optional (length array-dimension-limit))
  ;;  #x110000 1114112
  ;; clisp has 16,777,215 (unsigned-byte 24)
  ;; allegro 32bit is   (unsigned-byte 29)
  `(integer 0 ,length))

;; clisp-2.49/src/array.d for LISPFUN make_array around this comment 
;; /* table for assignment  ATYPE-byte -> vector type info */ 
;; suggests maybe more room could be had?
(deftype fixnum-exclusive ()
  #+sbcl `(integer ,(lognot array-dimension-limit) ,array-dimension-limit)
  #-sbcl `(integer ,(+ most-negative-fixnum 2) ,(- most-positive-fixnum 2)))

(deftype fixnum-0-or-over ()
  #-sbcl `(integer 0 ,most-positive-fixnum)
  ;; #+sbcl `(,@(or (and (= sb-vm:n-machine-word-bits 32)
  ;;                '(unsigned-byte 29))
  ;;           `(integer 0 ,most-positive-fixnum)))
  #+sbcl `(unsigned-byte ,sb-vm:n-positive-fixnum-bits))

;; (type-specifier-p 'fixnum-0-or-over)
;; (typep most-positive-fixnum `(integer 0 ,most-positive-fixnum)) 
;; (typep 536870912 `(integer 0 ,most-positive-fixnum))

;;; ==============================
;; #b00000000000000000000000000001111 0-15          4 bits  1 octet
;; #b00000000000000000000000011111111 0-255         8 bits  1 octet
;; #b00000000000000001111111111111111 0-65535      16 bits  2 octets
;; #b00000000111111111111111111111111 0-16777215   24 bits  3 octets
;; #b00011111111111111111111111111111 0-536870911  29 bits  4 octets
;; #b11111111111111111111111111111111 0-4294967295 32 bits  4 octets
;;           4       3      2       1
;; 
;; 0-18446744073709551615 64 bits 8 octets
;; #b1111111111111111111111111111111111111111111111111111111111111111
;;           8       7      6       5        4       3      2       1

(deftype unsigned-byte-128 ()
  ;; 128 bits 
  ;; 16 Octets
  ;; 128 bits 
  ;; 340282366920938463463374607431768211455
  ;; #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
  ;; #o3777777777777777777777777777777777777777777
  '(unsigned-byte 128))

(deftype unsigned-byte-96 ()
  ;; 96 bits
  ;; 12 octets
  ;; #xFFFFFFFFFFFFFFFFFFFFFFFF
  ;; 79228162514264337593543950335
  ;; #o77777777777777777777777777777777
  '(unsigned-byte 96))

(deftype unsigned-byte-64 ()
  ;; 64 bits 
  ;; 8 Octets
  ;; #xFFFFFFFFFFFFFFFF
  ;; 18446744073709551615
  ;; #o1777777777777777777777
  ;; #b1111111111111111111111111111111111111111111111111111111111111111
  '(unsigned-byte 64))

(deftype unsigned-byte-56 ()
  ;; 56 bits
  ;; 7 octets
  ;; #xFFFFFFFFFFFFFF
  ;; 72057594037927935
  ;; #o3777777777777777777
  '(unsigned-byte 56))

(deftype unsigned-byte-48 ()
  ;; 48 bits
  ;; 6 Octets
  ;; #xFFFFFFFFFFFF
  ;; 281474976710655 
  ;; #o7777777777777777
  ;; #b111111111111111111111111111111111111111111111111
  '(unsigned-byte 48))

(deftype unsigned-byte-32 ()
  ;; 32 bits
  ;; 4 Octets
  ;; #xFFFFFFFF
  ;; 4294967295
  ;; #o37777777777
  ;; #b11111111111111111111111111111111
  '(unsigned-byte 32))

(deftype unsigned-byte-29 ()
  ;; 29 bits
  ;; 4 octets
  ;; #x1FFFFFFF
  ;; 536870911
  ;; #o3777777777
  ;; #b00011111111111111111111111111111
  '(unsigned-byte 29))

(deftype unsigned-byte-24 ()
  ;; 24 bits
  ;; 3 octets
  ;; #xFFFFFF
  ;; 16777215
  ;; #o77777777
  ;; #b111111111111111111111111
  '(unsigned-byte 24))

(deftype unsigned-byte-16 ()
  ;; 16 bits
  ;; 2 octets
  ;; #xFFFF
  ;; 65535
  ;; #o177777
  ;; #b1111111111111111
  '(unsigned-byte 16))

(deftype unsigned-byte-8 ()
  ;; 8 bits
  ;; 1 octet
  ;; #xFF
  ;; 255
  ;; #o377
  ;; #b11111111
  '(unsigned-byte 8))

;; :SOURCE flexi-streams-1.0.7/mapping.lisp
(deftype octet ()
  ;; 8 bits
  ;; #xFF
  ;; 255
  ;; #o377
  ;; #b11111111
  '(unsigned-byte 8))

(deftype nibble ()
  ;; 4 bits
  ;; #xF
  ;; #o17
  ;; 15
  ;; #b1111  
  '(unsigned-byte 4))

;; :SOURCE flexi-streams-1.0.7/mapping.lisp
(deftype code-point ()
  ;; #x110000 => `cl:char-code-limit' (when charset is Unicode)
  ;; (mod 1114112) -> (integer 0 1114111)
  '(mod #x110000)) 
;;
;; (code-char 1114111)

;; :SOURCE flexi-streams-1.0.7/mapping.lisp
(deftype char-code-integer ()
  '(integer 0 #.(1- char-code-limit)))

(deftype char-or-char-code-integer ()
  '(or character char-code-integer))
;;
;; :NOTE An "alias"
(deftype character-or-char-code-integer ()
  '(character-or-char-code-integer))

(deftype char-or-char-code-integer-or-string-1 ()
  '(or char-or-char-code-integer string-of-length-1))

(deftype char-or-char-code-integer-or-simple-string-1 ()
  '(or char-or-char-code-integer simple-string-of-length-1))

(deftype each-a-char-code-integer ()
  '(and proper-list-not-null (satisfies each-a-char-code-integer-p)))

(deftype each-a-character ()
  '(and proper-list-not-null (satisfies each-a-character-p)))

(deftype each-a-character-or-char-code-integer ()
  '(and proper-list-not-null (satisfies each-a-character-or-char-code-integer-p)))

(deftype string-all-whitespace ()
  '(satisfies string-all-whitespace-p))

(deftype string-all-whitespace-safely ()
  '(and 
    string-not-empty
    (satisfies %fast-string-all-whitespace-p)))

;; string-all-whitespace
(deftype whitespace-char () 
  `(member ,@*whitespace-chars*))

(deftype char-not-whitespace-char ()
  '(and character (not whitespace-char)))

(deftype hexadecimal-char () 
  `(and standard-char (member ,@*hexadecimal-chars*)))

(deftype string-or-char ()
  `(and not-null (or string base-char standard-char extended-char)))

(deftype string-symbol-or-char ()
  '(or symbol string character))

(deftype string-or-char-or-code-point-integer ()
  ;; #+sb-unicode (or string-or-char code-point))
  ;; #-sb-unicode (or string-or-char char-code-integer))
  `(or string-or-char char-code-integer))

(deftype string-symbol-or-char-or-code-point-integer ()
  ;; #+sb-unicode (or string-or-char code-point))
  ;; #-sb-unicode (or string-or-char char-code-integer))
  `(or symbol string-or-char-or-code-point-integer))

(deftype symbol-not-a-constant ()
  '(and symbol (satisfies symbol-not-a-constantp)))

(deftype symbol-not-null ()
  '(and symbol not-null))

(deftype symbol-not-null-or-string-not-empty ()
  '(or symbol-not-null string-not-empty))

(deftype hash-table-or-symbol ()
  `(or hash-table symbol))

(deftype hash-table-or-symbol-with-hash ()
  `(and hash-table-or-symbol
        (or hash-table 
            (and not-boolean
                 (satisfies %hash-or-symbol-p-no-error)))))

;; :SOURCE sbcl/src/code/kernel.lisp 
#+sbcl 
(deftype closure-obj ()
  '(satisfies closure-p))


;;; ==============================
;;; :TYPES-INSPECT
;;; ==============================

;; :SOURCE (URL `http://groups.google.com/group/comp.lang.lisp/msg/dc40fdb1e007fbec?hl=en')
;; :FROM PJB :DATE 2006-08-07 comp.lang.lisp :SUBJECT Re: static typing
(defmacro define-list-of-predicate (type)
  (let* ((name-pair (make-list-of-predicate-name type))
         (fun-name (intern (car name-pair)))
         (typ-name (intern (cdr name-pair))))
    `(progn
       (defun ,fun-name (object)
         ,(format nil 
                  "Whether each element of OBJECT is of type ~A.~%~@
                   OBJECT should satisfy `mon:list-proper-p'. Signal an error if not." type)
         (and (or
               (list-proper-p object)
               (error (make-condition (quote proper-list-error)
                                      :w-type (quote function)
                                      :proper-list-error-args (list (quote object) object))))
              (unless (null object)
                (loop 
                   for chk in (the proper-list-not-null object)
                   always (typep chk ',type)))))
       (deftype ,typ-name ()
         (quote (and proper-list-not-null (satisfies ,fun-name)))))))

(defun make-list-of-predicate-name (type)
  ;; No need to declare this i guess.
  ;; (declare (special *vowel-chars*))
  (let ((rtn ()))
    (setf rtn
          (format nil "EACH-~:[AN~;A~]-~A-P"
                  (unless (member (char (symbol-name type) 0) *vowel-chars*) t)
                  type))
    (setf rtn (cons rtn (subseq rtn 0 (- (length rtn) 2))))))

#+sbcl 
(defun type-specifier-valid-p (type-specifier &optional env)
  (sb-ext:valid-type-specifier-p type-specifier env))

;;; :SOURCE flexi-streams-1.0.7/util.lisp
(defun type-equal (type1 type2)  
  ;; (declare #.*standard-optimize-settings*)
  (and (subtypep type1 type2)
       (subtypep type2 type1)))

(defun booleanp (obj &optional as-list)
  (declare (optimize (speed 3)))
  (if as-list
      (values 
       (or (and (typep obj 'boolean) 
		(list obj t))
	   (list nil nil))
       (type-of obj)
       obj)
      (values (typep obj 'boolean) obj)))

(defun symbol-not-a-constantp (symbol)
  ;; generalized-boolean
  (and (symbolp symbol) 
       (not (constantp symbol))
       symbol))

(declaim (inline symbol-not-null-or-string-not-empty-p))
(defun symbol-not-null-or-string-not-empty-p (object) 
  (declare (optimize (speed 3)))
  (typep object 'symbol-not-null-or-string-not-empty))


;;; ============================== 
;;; :SOURCE sbcl/src/code/early-extensions.lisp
#-sbcl 
(defun type-any (op thing list)
  (declare (type function op))
  (let ((certain? t))
    (dolist (i list (values nil certain?))
      (multiple-value-bind (sub-value sub-certain?) (funcall op thing i)
	(if sub-certain?
	    (when sub-value (return (values t t)))
	    (setf certain? nil))))))
#+sbcl 
(defun type-any (op thing list)
  (sb-int:any/type op thing list))
#-sbcl
(defun type-every (op thing list)
  (declare (type function op))
  (let ((certain? t))
    (dolist (i list (if certain? (values t t) (values nil nil)))
      (multiple-value-bind (sub-value sub-certain?) (funcall op thing i)
	(if sub-certain?
	    (unless sub-value (return (values nil t)))
	    (setf certain? nil))))))
#+sbcl 
(defun type-every (op thing list)
  (sb-int:every/type op thing list))

;;; ==============================


;;; ==============================
;;; TYPE-EXPANDERS
;;; ==============================
#+sbcl 
(defun type-expand-all (type-specifier &optional env)
  (sb-ext:typexpand-all type-specifier env))

#+sbcl 
(defun type-expand (type-specifier &optional env)
  (sb-ext:typexpand type-specifier env))

#+sbcl 
(defun type-expand-1 (type-specifier &optional env)
  (sb-ext:typexpand-1 type-specifier env))

;;; ==============================


;;; ==============================
;;; :SEQ-TYPE-PREDICATES
;;; ==============================

;;; :NOTE what about `proper-list-of-length-p'
;;; :FILE sbcl/src/code/primordial-extensions.lisp
;;; "Helper function for macros which expect clauses of a given length, etc.
;;; Return true if X is a proper list whose length is between MIN and
;;; MAX (inclusive)."
;;;
;; (defun proper-list-of-length-p (x min &optional (max min))
;;   ;; FIXME: This implementation will hang on circular list
;;   ;; structure. Since this is an error-checking utility, i.e. its
;;   ;; job is to deal with screwed-up input, it'd be good style to fix
;;   ;; it so that it can deal with circular list structure.
;;   (cond ((minusp max) nil)
;;         ((null x) (zerop min))
;;         ((consp x)
;;          (and (plusp max)
;;               (proper-list-of-length-p (cdr x)
;;                                        (if (plusp (1- min))
;;                                            (1- min)
;;                                            0)
;;                                        (1- max))))
;;         (t nil)))

;; :SOURCE alexandria/lists.lisp :WAS `circular-list-p'
(defun list-circular-p (object)
  (declare (optimize (speed 3)))
  (when (not (listp object))
    (return-from list-circular-p))
  (locally 
      (declare (list object))
    ;;(and (listp object)
    (do ((fast object (cddr fast))
         (slow (cons (car object) (cdr object)) (cdr slow)))
        (nil)
      (unless (and (consp fast) (listp (cdr fast)))
        (return nil))
      (when (eq fast slow)
        (return t)))))

(defun list-dotted-p (list)
  (declare (optimize (speed 3)))
  (if  (null list) 
       (values nil 'null)
       (when (consp list)
         (locally 
             (declare (cons list))
           (if (list-circular-p list)
               (values nil 'circular-list)
               (if (and (last list 0) t)
                   (values t 'dotted-list)
                   (values nil 'proper-list)))))))

(declaim (inline plist-proper-p))
(defun plist-proper-p (object)
  (declare (inline list-proper-p)
           (optimize (speed 3)))
  (when (list-proper-p object) 
    (the boolean
      ;; (evenp (length (the list-length object))))))
      (evenp (length (the list object))))))

(declaim (inline plist-proper-not-null-p))
(defun plist-proper-not-null-p (object)
  (declare (optimize (speed 3)))
  (when (consp object) 
    (the boolean 
      (typep (the cons object) 'proper-plist-not-null))))

(declaim (inline list-proper-not-null-p))
(defun list-proper-not-null-p (object)
  (declare (optimize (speed 3)))
  (the boolean 
    (typep object 'proper-list-not-null)))

;;; :SOURCE clocc/src/simple.lisp
;;; :NOTE :CALLED-BY `mon:freqs' :FILE seqs.lisp
(declaim (inline sequence-zerop))
(defun sequence-zerop (seq)
  (declare (inline sequencep)
           (optimize (speed 3)))
  (when (not (sequencep seq))
    (return-from sequence-zerop))
  (locally 
      (declare (type sequence seq))
    (the boolean
      (or (null seq) 
        ;; :NOTE Don't test for arrayp b/c multi-dimension arrays are not sequences:
        ;;      (typep (make-array '(2 3 4) :adjustable t) 'sequence) => NIL
        ;;      (typep (make-array '(2 3 4)) 'sequence) => NIL
        ;;      (typep (make-array 6 :adjustable t) 'sequence) => T
        (and 
         (vectorp seq)
         (zerop (length seq)))))))

;;; :SOURCE clocc/src/string.lisp
(defun sequence-type (seq &optional (no-error nil))
  (declare (inline sequencep)
           (optimize (speed 3)))
  (when no-error 
    (when (not (sequencep seq)) 
      (return-from sequence-type (the boolean nil))))
  (typecase seq    
    (null        (values nil 'null))
    (proper-list 'list)
    (cons        'cons)
    (string      (values 'string 
                         (or (and (simple-string-p seq) 'simple-string)
                             (type-of seq))))
    (bit-vector  (values (or (and (simple-bit-vector-p seq) 'simple-bit-vector) 'bit-vector) 
                         (type-of seq)))
    (vector (let ((eltype (array-element-type seq)))
              (if (eq eltype t) 'vector (list 'vector eltype))))
    (t 
     (unless no-error
       (error 'case-error
              :w-sym 'sequence-type
              :w-type 'function
              :w-args (list 'seq seq 'string 'null 'list 'cons 'vector))))))
;;
;; (sequence-type "string")
;; (sequence-type 'bubba)
;; (sequence-type (make-array 6 :adjustable t))
;; (sequence-type (make-array '(1 2 3) :adjustable t))
;; (sequence-type (make-array '(1 2 3) :adjustable t) t)

#+sbcl
(defun singleton-p (lst)
  (sb-int::singleton-p lst))



;;; ==============================
;;; :STRING-PREDICATES
;;; ==============================

(declaim (inline string-or-null-p))
(defun string-or-null-p (obj)
  (declare (optimize (speed 3)))
  (the boolean 
    (typep obj 'string-or-null)))

(declaim (inline string-empty-p))
(defun string-empty-p (str)
  (declare (optimize (speed 3)))
  (the boolean 
    (typep str 'string-empty)))

(declaim (inline string-not-empty-p))
(defun string-not-empty-p (string)
  (declare (optimize (speed 3)))
  ;; (funcall (complement #'mon:string-empty-p) string)
  (the boolean
    (when (stringp string) 
      (not (zerop (the array-length (length (the string string))))))))

(declaim (inline string-not-null-or-empty-p))
(defun string-not-null-or-empty-p (str)
  (declare (optimize (speed 3)))
  (the boolean 
    (typep str 'string-not-null-or-empty)))

(declaim (inline string-null-or-empty-p))
(defun string-null-or-empty-p (str)
  (declare (optimize (speed 3)))
  (the boolean 
    (typep str 'string-null-or-empty)))

(declaim (inline string-not-null-p))
(defun string-not-null-p (str)
  (declare (optimize (speed 3)))
  (the boolean 
    (typep str 'string-not-null)))

(declaim (inline string-of-length-1-p))
(defun string-of-length-1-p (string)
  (declare (optimize (speed 3)))
  (typep string 'string-of-length-1))

(declaim (inline string-with-fill-pointer-p))
(defun string-with-fill-pointer-p (putative-string-with-fill-pointer)
  (declare (optimize (speed 3)))
 (the boolean 
   (typep putative-string-with-fill-pointer 'string-with-fill-pointer)))

(declaim (inline simple-string-or-null-p))
(defun simple-string-or-null-p (obj)
  (declare (optimize (speed 3)))
 (the boolean 
   (typep obj 'simple-string-or-null)))

(declaim (inline simple-string-empty-p))
(defun simple-string-empty-p (str)
  (declare (optimize (speed 3)))
 (the boolean 
   (typep str 'simple-string-empty)))

(declaim (inline simple-string-null-or-empty-p))
(defun simple-string-null-or-empty-p (str)
  (declare (optimize (speed 3)))
 (the boolean 
   (typep str 'simple-string-null-or-empty)))

(declaim (inline simple-string-not-null-or-empty-p))
(defun simple-string-not-null-or-empty-p (str)
  (declare (optimize (speed 3)))
  (the boolean 
    (typep str 'simple-string-not-null-or-empty)))

(declaim (inline simple-string-not-null-p))
(defun simple-string-not-null-p (str)
  (declare (optimize (speed 3)))
  (the boolean 
    (typep str 'simple-string-not-null)))

(declaim (inline simple-string-of-length-1-p))
(defun simple-string-of-length-1-p (string)
  (declare (optimize (speed 3)))
  (typep string 'simple-string-of-length-1))

;; :AFTER string-of-length-1-p
(declaim (inline char-or-char-code-integer-or-string-1-p))
(defun char-or-char-code-integer-or-string-1-p (maybe-char-char-code-or-string-1)
  (declare (inline string-of-length-1-p)
           (optimize (speed 3)))
  (typep maybe-char-char-code-or-string-1 'char-or-char-code-integer-or-string-1))

;; :AFTER simple-string-of-length-1-p
(declaim (inline char-or-char-code-integer-or-simple-string-1-p))
(defun char-or-char-code-integer-or-simple-string-1-p (maybe-char-char-code-or-simple-string-1)
  (declare (inline simple-string-of-length-1-p)
           (optimize (speed 3)))
  (typep maybe-char-char-code-or-simple-string-1 'char-or-char-code-integer-or-simple-string-1))

(defun each-a-string-or-vector-in-vector (vector)
  (declare (inline sequencep)
           (optimize (speed 3)))
  (unless (sequencep vector)
    (return-from each-a-string-or-vector-in-vector (the boolean nil)))
  (unless (vectorp (the sequence vector))
    (return-from each-a-string-or-vector-in-vector (the boolean nil)))
  (when (stringp (the vector vector))
    (return-from each-a-string-or-vector-in-vector (the boolean nil)))
  (flet ((chk-string-like (string-like)
           (if (sequencep string-like)
               (if (or (stringp string-like) 
                       (typep string-like '(vector character *)))
                   t
                   (if (simple-vector-p string-like)
                       (loop 
                          for chk-char across (the simple-vector string-like)
                          always (characterp  chk-char)))))))
    (locally 
        (declare (vector vector))
      (the boolean
        (loop
           :for chk-str across vector
           :always (chk-string-like chk-str))))))

(defun each-a-string-or-vector-in-list (list)
  (declare (inline sequencep)
           (optimize (speed 3)))
  (unless (sequencep list)
    (return-from each-a-string-or-vector-in-list (the boolean nil)))
  (when (null list)
    (return-from each-a-string-or-vector-in-list (the boolean nil)))
  (when (vectorp (the sequence list))
    (return-from each-a-string-or-vector-in-list (the boolean nil)))
  (unless (list-proper-p (the sequence list))
    (return-from each-a-string-or-vector-in-list (the boolean nil)))
  (flet ((chk-string-like (string-like)
           (if (sequencep string-like)
               (if (or (stringp string-like) 
                       (typep string-like '(vector character *)))
                   t
                   (if (simple-vector-p string-like)
                       (loop 
                          for chk-char across (the simple-vector string-like)
                          always (characterp  chk-char)))))))
    (locally 
        (declare ((and not-null list) list))
      (the boolean
        (loop
           :for chk-str in list
           :always (chk-string-like chk-str))))))

(declaim (inline each-a-string-p))
(defun each-a-string-p (string-list)
  (declare (inline list-proper-not-null-p  list-proper-p)
           (optimize (speed 3)))
  (unless (list-proper-not-null-p string-list) 
    (return-from each-a-string-p nil))
  (locally 
      (declare (list string-list))
    (the boolean ;; (the boolean (every #'stringp string-list))
      (loop
         :for chk-str :in string-list
         :always (stringp chk-str)))))

(declaim (inline each-a-string-or-null-p))
(defun each-a-string-or-null-p (string-list)
  (declare 
   (inline string-or-null-p)
   (optimize (speed 3)))
  (unless (typep string-list 'proper-list)
    (return-from each-a-string-or-null-p nil))
  (when (null string-list)
    (return-from each-a-string-or-null-p t))
  (locally 
      (declare (proper-list-not-null string-list))
    (the boolean     ;; (the boolean (every #'string-or-null-p string-list))
      (loop
         :for chk-str :in string-list
         :always (string-or-null-p chk-str)))))

(defun each-a-simple-string-p (string-list)
  (declare 
   (inline list-proper-not-null-p)
   (optimize (speed 3)))
  (when (list-proper-not-null-p string-list)
    (locally 
        (declare (list  string-list))
      (the boolean ;; (the boolean (every #'simple-string-p string-list))
        (loop
           :for chk-str :in string-list
           :always (simple-string-p chk-str))))))

(defun each-a-string-of-length-1-p (string-list)
  (declare (inline string-of-length-1-p
                   string-not-null-or-empty-p)
   (optimize (speed 3)))
  (when (typep string-list 'proper-list-not-null)
    (locally 
        (declare (list string-list))
      (the boolean
        (loop
           :for chk-str :in string-list
           :always (and (string-not-null-or-empty-p chk-str)
                        ;; (= (the array-length (string-length (the string-not-empty chk-str))) 1)))))))
                        (string-of-length-1-p (the string-not-empty chk-str))))))))

(defun each-a-simple-string-of-length-1-p (string-list)
  (declare (inline simple-string-not-null-or-empty-p
                   simple-string-of-length-1-p)
   (optimize (speed 3)))
  (when (typep string-list 'proper-list-not-null)
    (locally 
        (declare (list string-list))
      (the boolean
        (loop
           :for chk-str :in string-list
           :always (and (simple-string-not-null-or-empty-p chk-str)
                        ;; (= (the array-length (string-length (the simple-string-not-empty chk-str))) 1)))))))
                        (simple-string-of-length-1-p (the simple-string-not-empty chk-str))))))))


;;; ==============================
;;; :SEQUENCE-PREDICATES
;;; ==============================

(defun each-a-sequence-p (seq)
  (declare  (inline sequencep)
            (optimize (speed 3)))
  (when (null seq) 
    (return-from each-a-sequence-p (the (values boolean symbol) (values t (quote null)))))
  (unless (sequencep seq)
    (return-from each-a-sequence-p (the boolean nil)))
  (locally
      (declare (sequence seq))
    (let ((string-or-bv (typecase seq
                          (string     (quote string))
                          (bit-vector (quote bit-vector)))))
      (declare (symbol string-or-bv))
      (when string-or-bv
        (return-from each-a-sequence-p (the (values boolean symbol) (values  nil string-or-bv))))
      (locally
          (declare ((and sequence (not string) (not bit-vector)) seq))
        (the (values boolean symbol)
          (etypecase seq
            (list 
             (setf string-or-bv (quote list))
             (values (loop 
                        for elt in seq
                        always (sequencep elt))
                     string-or-bv))
            (simple-array 
             (setf string-or-bv (quote simple-array))
             (values (loop 
                        for elt across (the simple-array seq)
                        always (sequencep elt))
                     string-or-bv))
            (array 
             (setf string-or-bv (quote array))
             (values (loop 
                        for elt across (the array seq)
                        always (sequencep elt))
                     string-or-bv))))))))

(defun each-a-sequence-proper-p (seq)
  (declare  (inline sequencep 
                    list-proper-p
                    sequence-proper-p)
            (optimize (speed 3)))
  (when (null seq) 
    (return-from each-a-sequence-proper-p (the (values boolean symbol) (values t (quote null)))))
  (unless (sequencep seq)
    (return-from each-a-sequence-proper-p (the boolean nil)))
  (locally
      (declare (sequence seq))
    (let ((string-or-bv (typecase seq
                          (string     (quote string))
                          (bit-vector (quote bit-vector)))))
      (declare (symbol string-or-bv))
      (when string-or-bv
        (return-from each-a-sequence-proper-p (the (values boolean symbol) (values nil string-or-bv))))
      ;; (locally 
      ;;     (declare ((and sequence (not string) (not bit-vector)) seq))
      ;;   (the boolean (every #'sequence-proper-p seq))))))
      (locally
          (declare ((and sequence (not string) (not bit-vector)) seq))
        (the (values boolean symbol)
          (etypecase seq
            (list 
             (setf string-or-bv (quote list))
             (values (loop 
                        for elt in seq
                        always (sequencep elt))
                     string-or-bv))
            (simple-array 
             (setf string-or-bv (quote simple-array))
             (values (loop 
                        for elt across (the simple-array seq)
                        always (sequencep elt))
                     string-or-bv))
            (array 
             (setf string-or-bv (quote array))
             (values (loop 
                        for elt across (the array seq)
                        always (sequencep elt))
                     string-or-bv))))))))

(defun each-a-sequence-proper-or-character-p (seq)
  (declare (inline sequencep list-proper-p)
           (optimize (speed 3)))
  (when (null seq) 
    (return-from each-a-sequence-proper-or-character-p t))
  (unless (sequencep seq)
    (return-from each-a-sequence-proper-or-character-p nil))
  (flet ((chk-all (x)
           (or (and (sequencep x)
                    (if (listp (the sequence x))
                        (list-proper-p (the list x))
                        t))
               (characterp x)))) 
    (declare (sequence seq))
    (the boolean (every #'chk-all seq))))


;;; ==============================
;;; :CHAR-TYPE-PREDICATES
;;; ==============================
;; (declaim (inline string-null-empty-or-all-whitespace-p))
(defun string-null-empty-or-all-whitespace-p (maybe-empty-string)
  (declare (optimize (speed 3)))
  (when (null maybe-empty-string)
    (return-from string-null-empty-or-all-whitespace-p t))
  (unless (stringp maybe-empty-string)
    (return-from string-null-empty-or-all-whitespace-p nil))
  (the boolean
    (when (or (string-empty-p maybe-empty-string)
              (string-all-whitespace-p maybe-empty-string))
      t)))

(defun string-not-null-empty-or-all-whitespace-p (maybe-good-string)
  (declare (optimize (speed 3)))
  (when (null maybe-good-string)
    (return-from string-not-null-empty-or-all-whitespace-p nil))
  (unless (stringp maybe-good-string)
    (return-from string-not-null-empty-or-all-whitespace-p nil))
  (when (string-empty-p (the string-not-null maybe-good-string))
    (return-from string-not-null-empty-or-all-whitespace-p nil))
  (locally 
      (declare (string-not-null-or-empty maybe-good-string)) 
    (the boolean
      (typep maybe-good-string 'string-not-null-empty-or-all-whitespace))))

(defun string-not-empty-or-all-whitespace-p (maybe-good-string)
  (declare (inline string-not-empty-p)
           (optimize (speed 3)))
  (when (stringp maybe-good-string)
    (when (the boolean (string-not-empty-p (the string maybe-good-string)))
      (locally 
          (declare (string-not-empty maybe-good-string))
        (not (the (values boolean array-length array-length)
               (string-all-whitespace-p maybe-good-string)))))))

;;  mon:string-empty-p (mon:string-not-empty-or-all-whitespace-p "")

;;; ==============================
(defun %fast-string-all-whitespace-p (maybe-good-string)
  ;; Like `mon:string-all-whitespace-p' but doesn't signal its type assertion
  ;; error when maybe-good-string is not `cl:stringp'
  ;; (%fast-string-all-whitespace-p (format nil "~{~C~}" *whitespace-chars*))
  ;; (%fast-string-all-whitespace-p " ")
  ;; (%fast-string-all-whitespace-p "")
  ;; (%fast-string-all-whitespace-p "     a")
  ;; (%fast-string-all-whitespace-p 8)
  (declare ;; (string-or-null maybe-good-string)
   (inline string-not-empty-p)
   (optimize (speed 3)))
  (unless (stringp maybe-good-string)
    (return-from %fast-string-all-whitespace-p nil))
  (unless (string-not-empty-p (the string maybe-good-string))
    (return-from %fast-string-all-whitespace-p nil))
  (locally 
      (declare (string-not-empty maybe-good-string))
    (loop 
       :for chars :across maybe-good-string
       :always (whitespace-char-p chars))))

;; (string-empty-p nil)
;;; ==============================  
(defun string-all-whitespace-p (string)
  (declare ((and not-null string) string)
           (optimize (speed 3)))
  (values-list 
   (if (string-empty-p string)
       (list nil 0 0)
       (locally 
           (declare (string-not-null-or-empty string))
         (let ((chk-if 
                (the array-length
                  (loop
                     ;; :WAS :for chars :across (the string-not-null-or-empty string)
                     :for chars :across string
                     ;; :when (not (whitespace-char-p chars)) 
                     :unless (whitespace-char-p chars)
                     :do (loop-finish)
                     :count chars)))
               ;; :WAS (chk-len (length string)))
               (chk-len (the array-length (length string))))
           (declare (array-length chk-if chk-len))
           (or (and (eq chk-len chk-if) 
                    (list t chk-len chk-len))
               (list nil chk-if chk-len)))))))

(defun string-contains-whitespace-p (string)
  (declare ((and not-null string) string)
           (inline string-empty-p)
           (optimize (speed 3)))
  (values-list 
   (if (string-empty-p string)
       (list nil 0 0)
       (let ((chk-if (loop 
                        :for chars :across (the string-not-null-or-empty string)
                        :when (whitespace-char-p chars)
                        :do   (loop-finish)
                        :count chars))
             (chk-len (the array-length (length string))))
         (or (and (eq chk-len chk-if) 
                  (list nil chk-len))
             (list t chk-if chk-len))))))
         
(defun string-no-whitespace-p (string)
  (declare ((and not-null string) string)
           (inline string-empty-p)
           (optimize (speed 3)))
  (values-list
   (if (string-empty-p string)
       (list t 0 0)
       (let ((chk-if (loop 
                        :for chars :across (the string-not-null-or-empty string)
                        :when (whitespace-char-p chars)
                        :do (loop-finish)
                        :count chars))
             (chk-len (length string)))
         (or (and (eq chk-len chk-if) 
                  (list t chk-len)) 
             (list nil chk-if chk-len))))))

(declaim (inline char-or-char-code-integer-p))
(defun char-or-char-code-integer-p (char-or-char-code-integer)
  (declare (optimize (speed 3)))
  (the boolean
    (typep char-or-char-code-integer 'char-or-char-code-integer)))

(defun each-a-character-or-char-code-integer-p (char-or-char-code-list)
  (declare (inline sequencep 
                   char-or-char-code-integer-p
                   list-proper-not-null-p)
           (optimize (speed 3)))
  (when (sequencep char-or-char-code-list)
    ;; (list-proper-not-null-p
    (when (list-proper-not-null-p (the sequence char-or-char-code-list))
      ;; (loop 
      ;;    :for char-or-code :in char-or-char-code-list
      ;;    ;; :always (typecase char-or-code
      ;;    ;;           (character t)
      ;;    ;;           (char-code-integer t)))))
      ;;    :always (char-or-char-code-integer-p char-or-code))
      (locally 
          (declare (list char-or-char-code-list))
        (the boolean
          (every #'char-or-char-code-integer-p char-or-char-code-list))))))

(defun each-a-character-p (char-list)
  (declare (inline sequencep 
                   list-proper-not-null-p)
           (optimize (speed 3)))
  (when (sequencep char-list)
    (when (list-proper-not-null-p (the sequence char-list))
      (locally 
          (declare (list char-list))
        ;; (loop
        ;;    :for chars :in char-list
        ;;    :always (characterp chars))
        (the boolean
        (every #'characterp char-list))))))

(declaim (inline char-code-integer-p))
(defun char-code-integer-p (char-code-int)
  (declare (optimize (speed 3)))
  (the boolean
    (typep char-code-int 'char-code-integer)))

(defun each-a-char-code-integer-p (char-code-integer-list)
  (declare (inline sequencep 
                   list-proper-not-null-p)
           (optimize (speed 3)))
  (when (sequencep char-code-integer-list)
    (when (list-proper-not-null-p (the sequence char-code-integer-list))
      (locally 
          (declare (list char-code-integer-list))
        (the boolean
          (loop
             :for char-code-integer-list :in char-code-integer-list
             :always (char-code-integer-p char-code-integer-list)))))))

(declaim (inline base-char-p))
(defun base-char-p (chr)
  (declare (optimize (speed 3)))
  (the boolean
    (typep chr 'base-char)))

(declaim (inline %digit-char-0-or-1-p))
(defun %digit-char-0-or-1-p (char-1or0)
  (declare (type standard-char-or-null char-1or0)
           (optimize (speed 3)))
  (the boolean
    (and char-1or0 
         (the (or (mod 2) null)
           (digit-char-p char-1or0 2))
         t)))

(declaim (inline digit-char-0-or-1-p))
(defun digit-char-0-or-1-p (char-1or0)
  (declare (inline %digit-char-0-or-1-p)
           (optimize (speed 3)))
  (when 
      (typep char-1or0 'standard-char-or-null)
    (the boolean
      (%digit-char-0-or-1-p char-1or0))))

(defun string-all-digit-char-0-or-1-p (1-and-0-string)
  (declare (string-or-null 1-and-0-string)
           (inline digit-char-0-or-1-p)
           (optimize (speed 3)))
  (when 1-and-0-string ;; allow null and bail
    ;; :WAS (not (simple-string-empty-p 1-and-0-string))       
    ;; 
    (when (string-not-empty-or-all-whitespace-p (the string 1-and-0-string))
      ;; (loop 
      ;;   :for 1or0 :across 1-and-0-string
      ;;   ;; :unless (digit-char-0-or-1-p 1or0)
      ;;   ;; :return nil
      ;;   ;; :finally (return t))))))
      ;;   :always (digit-char-0-or-1-p 1or0))
      (if (simple-string-p 1-and-0-string)
          (every #'digit-char-0-or-1-p (the simple-string-not-empty 1-and-0-string))
          (every #'digit-char-0-or-1-p (the string-not-empty 1-and-0-string))))))

(defun string-all-hex-char-p (maybe-hex-string)
  (declare (string-or-null maybe-hex-string)
           (optimize (speed 3)))
  (when maybe-hex-string ;; allow null and bail
    (when (string-not-null-empty-or-all-whitespace-p (the string maybe-hex-string))
      ;; (loop 
      ;;    :for chk-hex :across (the string maybe-hex-string)
      ;;    :unless (hexadecimal-char-p chk-hex)
      ;;    :return nil
      ;;    :finally (return t))
      (the boolean
        (if (simple-string-p maybe-hex-string)
          (every #'hexadecimal-char-p (the simple-string-not-empty maybe-hex-string))
          (every #'hexadecimal-char-p (the string-not-empty maybe-hex-string)))))))


;;; ==============================
;;; :PATH-PREDICATES
;;; ==============================

(declaim (inline logical-pathname-p))
(defun logical-pathname-p (maybe-logical-pathname)
    (declare (optimize (speed 3)))
  (typep maybe-logical-pathname 'logical-pathname-designator))

(declaim (inline filename-designator-p))
(defun filename-designator-p (maybe-filename-designator)
    (declare (optimize (speed 3)))
  (typep maybe-filename-designator 'filename-designator))

(declaim (inline pathname-designator-p))
(defun pathname-designator-p (maybe-pathname-designator)
    (declare (optimize (speed 3)))
  (typep maybe-pathname-designator 'pathname-designator))

(declaim (inline pathname-or-namestring-p))
(defun pathname-or-namestring-p (maybe-pathname-or-namestring)
  (declare (optimize (speed 3)))
  (typep maybe-pathname-or-namestring 'pathname-or-namestring))


;;; ==============================
;;; :NUMBER-PREDICATES
;;; ==============================
(declaim (inline fixnump))
(defun fixnump (fixnum-maybe)
  (declare (optimize (speed 3)))
  (the boolean
    (typep fixnum-maybe 'fixnum)))

(declaim (inline bignump))
(defun bignump (bignum-maybe)
  (declare (optimize (speed 3)))
  (the boolean
    (typep bignum-maybe 'bignum)))

;;; ==============================


;;; ==============================
;;; :SYMBOL-PREDICATES (functionp (function ))
;;; ==============================
(declaim (inline standard-test-function-p))
(defun standard-test-function-p (test-fun)
  (declare 
   ;; (special *standard-test-functions*)
   (optimize (speed 3)))
  (memq test-fun *standard-test-functions*))

;; :SOURCE sbcl/src/code/kernel.lisp 
#+sbcl 
(defun closure-p (object)
  (sb-impl::closurep object))

#+sbcl 
(defun variable-special-p (symbol)
  ;; Alternatively:
  ;; #+sbcl (eq (sb-int:info :variable :kind symbol) :special))
  (sb-walker:var-globally-special-p symbol))

;; :SOURCE clack-doc/src/doc/util.lisp
(defun declared-special-p (symbol)
  #+lispworks (sys:declared-special-p symbol)
  #+allegro (eq (sys:variable-information symbol) :special)
  #+clozure (ccl:proclaimed-special-p symbol)
  #+sbcl (variable-special-p symbol))

;;; ==============================
;; (defun variablep (symbol)
;;   (and (symbolp symbol) 
;;        (boundp  symbol)
;;        (not (constantp symbol))))


;;; ==============================
;;; :NOTE Alexandria has `featurep' in :FILE alexandria/features.lisp
;;; SBCL has `sb-int:featurep' in :FILE sbcl/src/code/early-extensions.lisp
;;; These are basically the same, but w/ Alexandria's indirecting through more
;;; macrology around `alexandria:eswitch'. 
;;; We normalize SBCL's X argument to reflect alexandria's FEATURE-EXPRESSION.
#-sbcl 
(defun featurep (feature-expression)
  (etypecase feature-expression
    (symbol (not (null (member feature-expression *features*))))
    (cons (check-type (first feature-expression) symbol)
	  (alexandria:eswitch ((first feature-expression) :test 'string=)
	    (:and (every #'featurep (rest feature-expression)))
	    (:or  (some #'featurep (rest feature-expression)))
	    (:not (assert (= 2 (length feature-expression)))
		  (not (featurep (second feature-expression))))))))
#+sbcl 
(defun featurep (feature-expression)
  (sb-int:featurep feature-expression))


;;; ==============================
;;; :TYPES-DOCUMENTATION
;;; ==============================

(typedoc 'stream-or-boolean
"Whether object is of type `cl:boolean' or `cl:stream'.~%~@
:EXAMPLE~%
 (typep nil 'stream-or-boolean)~%
 (typep t 'stream-or-boolean)~%
 (typep *standard-output* 'stream-or-boolean)~%
 \(typep \"bubba\" 'stream-or-boolean\)~%~@
:SEE-ALSO `mon:open-stream-output-stream-p',
`mon:open-stream-output-stream-error', `open-stream-output-stream-error-report',
`open-stream-output-stream-error', `cl:streamp', `cl:open-stream-p',
`cl:output-stream-p'.~%►►►")

(typedoc 'stream-or-boolean-or-string-with-fill-pointer
         "An object which is of type `mon:stream-or-boolean', or `mon:string-with-fill-pointer'.~%~@
:EXAMPLE~%~@
 (typep nil 'stream-or-boolean-or-string-with-fill-pointer)~%
 (typep t 'stream-or-boolean-or-string-with-fill-pointer)~%
 \(with-output-to-string \(s\)
   \(format s \"is stream-or-boolean-or-string-with-fill-pointer ~~S\"
           \(typep s 'stream-or-boolean-or-string-with-fill-pointer\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(typedoc 'logical-pathname-designator
"An object which is both `cl:pathnamep' and of type `cl:logical-pathname'.~%~@
:EXAMPLE~%
 \(typep #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\" 'logical-pathname-designator\)~%~@
;; Following fail successfully:~%
 \(typep \"SYS:SRC;CODE;TARGET-PATHNAME.LISP\" 'logical-pathname-designator\)~%
 \(typep \(translate-logical-pathname #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\)
        'logical-pathname-designator\)~%
:NOTE ANSI says the following w/r/t to the system class LOGICAL-PATHNAME:~%
 ,----
 | A pathname that uses a namestring syntax that is
 | implementation-independent, and that has component values that are
 | implementation-independent.  Logical pathnames do not refer directly to
 | filenames.
 `---- :SEE \(info \"\(ansicl\)logical-pathname \(System Class\)\"\)~%~@
:SEE-ALSO `mon:filename-designator-p' `mon:pathname-designator-p',
`mon:pathname-or-namestring-p', `mon:logical-pathname-p',
`mon:logical-pathname-designator', `mon:pathname-designator',
`mon:filename-designator', `mon:pathname-or-namestring'.~%►►►")

(typedoc 'pathname-designator
         "An object of this type is a legal arg to a pathname function including `cl:file-stream's.~%~@
:EXAMPLE~%~@
 \(typep *default-pathname-defaults* 'pathname-designator\)~%
 \(typep \(namestring *default-pathname-defaults*\) 'pathname-designator\)~%
 \(let \(\(pd-strm \(make-pathname :directory '\(:absolute \"tmp\"\)
                               :name \"pd-strm\"\)\)\)
   \(unwind-protect
        \(with-open-file \(strm pd-strm 
                              :direction :output 
                              :if-exists :supersede
                              :if-does-not-exist :create\)
          \(typep strm 'pathname-designator\)\)
     \(and \(probe-file pd-strm\)
          \(delete-file pd-strm\)\)\)\)~%~@
:SEE-ALSO `mon:filename-designator-p' `mon:pathname-designator-p',
`mon:pathname-or-namestring-p', `mon:logical-pathname-p',
`mon:logical-pathname-designator', `mon:pathname-designator',
`mon:filename-designator', `mon:pathname-or-namestring'.~%►►►")

(typedoc 'filename-designator
"An object of this type names a \"file\".~%~@
Like `mon:pathname-designator' but does not specifiy `cl:file-stream'.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:filename-designator-p' `mon:pathname-designator-p',
`mon:pathname-or-namestring-p', `mon:logical-pathname-p',
`mon:logical-pathname-designator', `mon:pathname-designator',
`mon:filename-designator', `mon:pathname-or-namestring'.~%►►►")

(typedoc 'pathname-or-namestring 
"An object which is either `cl:stringp' and `cl:pathnamep'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:filename-designator-p' `mon:pathname-designator-p',
`mon:pathname-or-namestring-p', `mon:logical-pathname-p',
`mon:logical-pathname-designator', `mon:pathname-designator',
`mon:filename-designator', `mon:pathname-or-namestring'.~%►►►")

(typedoc 'string-or-symbol
"An object that is of either type `cl:string' or `cl:symbol'.~%~@
:EXAMPLE~%
 \(typep \"string\" 'string-or-symbol\)~%
 \(typep :directory 'string-or-symbol\)~%
 \(typep 'directory 'string-or-symbol\)~%
 \(typep t 'string-or-symbol\)~%
 \(typep nil 'string-or-symbol\)~%
 \(typep 8 'string-or-symbol\)~%~@
:SEE-ALSO `mon:string-or-symbol-not-empty-nor-boolean',
`mon:string-or-symbol-not-boolean'.~%►►►")

(typedoc 'not-boolean
"An object that is neither T nor NIL.~%~@
:EXAMPLE~%
 \(typep t 'not-boolean\)~%
 \(typep nil 'not-boolean\)~%
 \(typep '\(\) 'not-boolean\)~%
 \(typep \(\) 'not-boolean\)~%
 \(typep \(not \(\)\) 'not-boolean\)~%
 \(typep 8 'not-boolean\)~%~@
:SEE-ALSO `mon:not-t', `mon:not-null', `cl:boolean'.~%►►►")

(typedoc 'boolean-integer
         "An integer value  either 1 or 0.
Where 1 is equivavlent to CL:T and 0 to CL:NIL
:EXAMPLE~%
 \(typep 0 'boolean-integer\)~%
 \(typep 1 'boolean-integer\)~%
 \(typep t 'boolean-integer\)~%
 \(typep nil 'boolean-integer\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(typedoc 'string-or-symbol-not-boolean
"An object of type: \(and not-boolean \(or symbol string\)\)~%~@
:EXAMPLE~%~@
 \(typep \"string\" 'string-or-symbol-not-boolean\)~%
 \(typep :directory 'string-or-symbol-not-boolean\)~%
 \(typep 'directory 'string-or-symbol-not-boolean\)~%
 \(typep t 'string-or-symbol-not-boolean\)~%
 \(typep nil 'string-or-symbol-not-boolean\)~%
 \(typep 8 'string-or-symbol-not-boolean\)~%~@
:SEE-ALSO `mon:string-or-symbol-not-empty-nor-boolean',
`mon:string-or-symbol-not-boolean', `mon:string-or-symbol',
`mon:not-boolean'.~%►►►")

(typedoc 'hash-table-or-symbol
"Whether object is either of the types:~%~@
 `cl:hash-table' or `cl:symbol'
:EXAMPLE~%
 \(typep \(make-hash-table\) 'hash-table-or-symbol\)~%
 \(typep nil 'hash-table-or-symbol\)~%
 \(typep t 'hash-table-or-symbol\)
 \(typep 'hash-table-or-symbol 'hash-table-or-symbol\)
 \(progn
  \(setq *tt--hash-table-symbol* \(make-hash-table\)\)
  \(prog1
      `\(\(HASH-TABLE-OR-SYMBOL ,\(typep '*tt--hash-table-symbol* 'hash-table-or-symbol\)\)
        \(HASH-TABLE-P ,\(typep '*tt--hash-table-symbol* 'hash-table\)\)
        \(SYMBOL-P ,\(typep '*tt--hash-table-symbol* 'symbol\)\)
        \(\"UNQUOTED-HASH-TABLE-P\" ,\(typep *tt--hash-table-symbol* 'hash-table\)\)\)
    \(unintern '*tt--hash-table-symbol*\)\)\)~%~@
:NOTE When declaring an object type and object alway evaluate to a hash-table or
a symbol evaluating to one it may be useful to declare it
`mon:hash-table-or-symbol-with-hash'.~%~@
:SEE-ALSO `mon:hash-or-symbol-ensured', `mon:hash-or-symbol-p'.~%►►►")

(typedoc 'hash-table-or-symbol-with-hash
         "Whether object is a hash-table or symbol evalauating to one.~%~@
:EXAMPLE~%~@
 \(typep t 'hash-table-or-symbol-with-hash\)~%
 \(typep nil 'hash-table-or-symbol-with-hash\)~%
 \(typep \(make-hash-table\) 'hash-table-or-symbol-with-hash\)~%
 \(progn
   \(setq *tt--hash-table-symbol* \(make-hash-table\)\)
   \(prog1 \(typep '*tt--hash-table-symbol* 'hash-table-or-symbol-with-hash\)
     \(unintern '*tt--hash-table-symbol*\)\)\)~%~@
:NOTE When used as a declaration and object may be NULL it is better to declare 
`mon:hash-table-or-symbol'.~%~@
:SEE-ALSO `mon:hash-or-symbol-ensured', `mon:hash-or-symbol-p'.~%►►►")

(typedoc 'string-or-symbol-not-empty-nor-boolean
"An object of type: \(and not-boolean \(or symbol string-not-empty\)\)~%~@
:EXAMPLE~%~@
 \(typep \"string\" 'string-or-symbol-not-boolean\)~%
 \(typep :directory 'string-or-symbol-not-boolean\)~%
 \(typep 'directory 'string-or-symbol-not-boolean\)~%
 \(typep "" 'string-or-symbol-not-boolean\)~%
 \(typep t 'string-or-symbol-not-boolean\)~%
 \(typep nil 'string-or-symbol-not-boolean\)~%
 \(typep 8 'string-or-symbol-not-boolean\)~%~@
:SEE-ALSO `mon:string-or-symbol-not-boolean', `mon:string-or-symbol',
`mon:not-boolean'.~%►►►")

(typedoc 'symbol-not-a-constant
"Whether object satisfies `mon:symbol-not-a-constantp'.~%~@
:EXAMPLE~%
 \(typep nil 'symbol-not-a-constant\)~%
 \(typep pi 'symbol-not-a-constant\)~%
 \(typep 'bubba 'symbol-not-a-constant\)~%~@
:SEE-ALSO `cl:defconstant', `cl:constantp'.~%►►►")

(typedoc 'symbol-not-null-or-string-not-empty
"Whether object is of type `mon:symbol-not-null' or `mon:string-not-empty'.~%~@
:EXAMPLE~%
 \(typep t   'symbol-not-null-or-string-not-empty\)~%
 \(typep 'b  'symbol-not-null-or-string-not-empty\)~%
 \(typep \"b\" 'symbol-not-null-or-string-not-empty\)~%
 \(typep \"\"  'symbol-not-null-or-string-not-empty\)~%
 \(typep nil 'symbol-not-null-or-string-not-empty\)~%~@
:SEE-ALSO `mon:symbol-not-null-or-string-not-empty-p',
`mon:string-not-empty-p'.~%►►►")

(typedoc 'string-or-null
 "Type is either `cl:stringp' or null.~%~@
:SEE-ALSO `mon:simple-string-or-null', `mon:string-or-null-p',
`mon:string-null-or-empty-p'.~%►►►")

(typedoc 'string-null-empty-or-all-whitespace
         "Whether object is either of the types:~%
 `mon:string-null-or-empty', or `mon:string-all-whitespace'~%~@
:EXAMPLE~%
 \(typep \"\"     'string-null-empty-or-all-whitespace\)~%
 \(typep \"\"     'string-null-empty-or-all-whitespace\)~%
 \(typep \"    \" 'string-null-empty-or-all-whitespace\)\)~%
 \(type-expand-all 'string-null-empty-or-all-whitespace\)~%~@
:SEE-ALSO `string-null-empty-or-all-whitespace-p'.~%►►►")


;; (setf (documentation 'string-null-empty-or-all-whitespace 'type)
;;       #.(format nil
(typedoc 'string-null-empty-or-all-whitespace
                "Whether object is either of the types:~%
 `mon:string-null-or-empty', or `mon:string-all-whitespace'~%~@
:EXAMPLE~%
 \(typep \"\"     'string-null-empty-or-all-whitespace\)~%
 \(typep \"\"     'string-null-empty-or-all-whitespace\)~%
 \(typep \"    \" 'string-null-empty-or-all-whitespace\)~%
 \(type-expand-all 'string-null-empty-or-all-whitespace\)~%~@
:SEE-ALSO `string-null-empty-or-all-whitespace-p'.~%►►►")


(typedoc 'string-not-null
"Whether object is of both types `mon:not-null' and `cl:string'.~%~@
:EXAMPLE~%
 \(string-not-null \"\"\)~%
 \(string-not-null \"a\"\)~%
 \(string-not-null nil\)~%~@
:SEE-ALSO `mon:string-or-null', `mon:string-null-or-empty',
`mon:simple-string-not-null', `mon:simple-string-or-null',
`mon:simple-string-null-or-empty'.~%►►►")

(typedoc 'each-a-string-or-null
         "Whether object is of type `mon:each-a-string-or-null-p'.~%~@
:EXAMPLE~%
 \(typep  '\(\"a\" \"b\" \"c\"\) 'each-a-string-or-null\)~%
 \(typep '\(nil nil nil\) 'each-a-string-or-null\)~%
 \(typep '\(\"a\" nil \"b\" nil\) 'each-a-string-or-null\)~%
 \(typep nil 'each-a-string-or-null\)~%
 \(typep 42 'each-a-string-or-null\)~%
 \(typep '\(\"a\" . \"b\"\) 'each-a-string-or-null\)~%
 \(typep '\(\"a\" \"b\" . \"c\"\) 'each-a-string-or-null\)~%~@
:SEE-ALSO `mon:each-a-string-or-null-p', `mon:each-a-string',
`mon:each-a-simple-string', `mon:each-a-string-of-length-1',
`mon:each-a-sequence', `mon:each-a-sequence-proper',
`mon:each-a-sequence-proper-or-character', `mon:each-a-simple-string-p',
`mon:each-a-string-of-length-1-p'.~%►►►")

(typedoc 'simple-string-or-null
"An object of type `mon:string-or-null' satisfying `cl:simple-string-p'~%~@
:EXAMPLE~%
 \(let \(\(bubba \"bubba\"\)\)
  \(declare \(type simple-string-or-null bubba\)\) bubba\)~%~@
:SEE-ALSO `mon:string-or-null-p', `mon:string-null-or-empty-p'.~%►►►")

(typedoc 'string-empty
"Whether object is a string of length 0.~%~@
:EXAMPLE~%
 \(not \(typep \"a\" 'string-empty\)\)~%
 \(typep \"\" 'string-empty\)~%
 \(typep \(make-array 0 :element-type 'character\) 'string-empty\)~%~@
:SEE-ALSO `mon:string-empty', `mon:string-not-empty', `mon:string-not-null-or-empty',
`mon:simple-string-empty', `mon:simpele-string-not-empty',
`mon:simple-string-not-null-or-empty', `mon:string-or-null',
`mon:simple-string-or-null'.~%►►►")

(typedoc 'string-not-empty
"Whether object is a string not of length 0.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:string-empty', `mon:string-not-empty',
`mon:string-not-null-or-empty', `mon:simple-string-empty',
`mon:simpele-string-not-empty', `mon:simple-string-not-null-or-empty',
`mon:string-or-null', `mon:simple-string-or-null'.~%►►►")

(typedoc 'string-not-null-or-empty
"Whether object is a string not of length 0 and not null.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:string-empty', `mon:string-not-empty',
`mon:string-not-null-or-empty', `mon:simple-string-empty',
`mon:simpele-string-not-empty', `mon:simple-string-not-null-or-empty',
`mon:string-or-null', `mon:simple-string-or-null'.~%►►►")

(typedoc 'simple-string-empty
"Whether object is a `cl:simple-string' type of length 0.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:string-empty', `mon:string-not-empty',
`mon:string-not-null-or-empty', `mon:simple-string-empty',
`mon:simpele-string-not-empty', `mon:simple-string-not-null-or-empty',
`mon:string-or-null', `mon:simple-string-or-null'.~%►►►")

(typedoc 'simple-string-not-empty
"Whether object is a `cl:simple-string' not type of length 0.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:string-empty', `mon:string-not-empty',
`mon:string-not-null-or-empty', `mon:simple-string-empty',
`mon:simpele-string-not-empty', `mon:simple-string-not-null-or-empty',
`mon:string-or-null', `mon:simple-string-or-null'.~%►►►")

(typedoc 'simple-string-not-null-or-empty
"Whether object is a `cl:simple-string' not type of length 0 nor null.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:string-empty', `mon:string-not-empty',
`mon:string-not-null-or-empty', `mon:simple-string-empty',
`mon:simpele-string-not-empty', `mon:simple-string-not-null-or-empty',
`mon:string-or-null', `mon:simple-string-or-null'.~%►►►")

(typedoc 'string-of-length-1
"An object with the type signature: \(array character \(1\)\).~%~@
Unlike objects of type `mon:simple-string-of-length-1' an object of this type
includes adjustable arrays and those with fill-pointers.~%~@
:EXAMPLE~%
 \(typep  \"a\"  'string-of-length-1\)~%
 \(typep \(make-array 1 :element-type 'character :initial-element #\\a\) 'string-of-length-1\)~%
 \(typep \(make-array 1 :element-type 'character :initial-element #\\a :adjustable t :fill-pointer 0\) 'string-of-length-1\)~%
;; Following fail successfully:~%
 \(typep \"\" 'string-of-length-1\)~%
 \(typep  \"ab\" 'string-of-length-1\)~%
 \(typep 42 'string-of-length-1\)~%
:SEE-ALSO `mon:simple-string-of-length-1' `mon:simple-string-of-length-1-p',
`mon:each-a-string-of-length-1', `mon:each-a-string-of-length-1-p'.~%►►►")

(typedoc 'simple-string-of-length-1
"An object with the type signature: \(simple-array character \(1\)\).~%~@
:EXAMPLE~%
 \(typep  \"a\" 'simple-string-of-length-1\)~%
 \(typep \(make-array 1 :element-type 'character :initial-element #\\a\) 'simple-string-of-length-1\)~%
 \(typep \(make-array 1 :element-type 'character :initial-element #\\a :adjustable t :fill-pointer 0\) 'simple-string-of-length-1\)~%~@
;; Following fail successfully:~%
 \(typep \"\" 'simple-string-of-length-1\)~%
 \(typep  \"ab\" 'simple-string-of-length-1\)~%
 \(typep 42 'simple-string-of-length-1\)~%~@
:SEE-ALSO `mon:string-of-length-1' `mon:string-of-length-1-p',
`mon:each-a-string-of-length-1', `mon:each-a-string-of-length-1-p'.~%►►►")

(typedoc 'string-all-whitespace-safely
"Like type `mon:string-all-whitespace', but doesn't signal a type-error
error when object is not `cl:stringp'.~%~@
:EXAMPLE~%
 \(typep \(format nil \"~~{~~C~~}\" *whitespace-chars*\) 'string-all-whitespace-safely\)~%
 \(typep \(format nil \"~~{~~C~~}good-string\" *whitespace-chars*\) 'string-all-whitespace-safely\)~%
 \(typep \" \" 'string-all-whitespace-safely\)~%
 \(typep \"\" 'string-all-whitespace-safely\)~%
 \(typep \"     a\" 'string-all-whitespace-safely\)~%
 \(typep 8 'string-all-whitespace-safely\)~%
 \(typep nil 'string-all-whitespace-safely\)~%~@
:NOTE Checking for this type may run in linear time!~%~@
:NOTE There are good use-cases for both `mon:string-all-whitespace-p' and its
associated type `mon:string-all-whitespace' when we have already distinguished
that object is of type `mon:string-not-null'.~%~@
:SEE-ALSO `<XREF>'.~%►►►")

;; #.(format nil
(typedoc 'string-not-null-empty-or-all-whitespace
          "Whether object is of type `string-not-null-or-empty' and does not satisfy
`mon:string-all-whitespace-p'.~%~@
bubba
:EXAMPLE~%
 \(typep \"\" 'string-not-null-empty-or-all-whitespace\)~%
 \(typep \" \" 'string-not-null-empty-or-all-whitespace\)~%
 \(typep \"A good string\" 'string-not-null-empty-or-all-whitespace\)~%
 \(typep nil 'string-not-null-empty-or-all-whitespace\)~%
 \(typep 8 'string-not-null-empty-or-all-whitespace\)~%
:SEE-ALSO `mon:string-empty', `mon:string-not-empty',
`mon:string-not-null-or-empty', `mon:simple-string-empty',
`mon:simpele-string-not-empty', `mon:simple-string-not-null-or-empty',
`mon:string-or-null', `mon:simple-string-or-null'.~%►►►")

(typedoc 'integer-or-null
 "Type is either `cl:integerp' or null.~%~@
:SEE-ALSO `mon:string-or-null-p', `mon:string-null-or-empty-p'.~%►►►")

(typedoc 'proper-list
 "Type designator for proper lists.~%~@
Implemented as a SATISFIES type constraint w/ `mon:list-proper-p' and not
recommended for performance intensive use.~%~@
Main usefulness as a type designator of the expected type in a TYPE-ERROR.~%~@
:NOTE ANSI Glossary defines the following terms with regards to \"proper list\":~%
 ,----
 |
 | proper list
 | n. A list terminated by the empty list. \(The empty list is a proper list.\)
 | 
 | improper list
 | n. A list which is not a proper list: a circular list or a dotted list.
 |
 | dotted list  n.
 | n. A list which has a terminating atom that is not NIL.
 |    \(An atom by itself is not a dotted list, however.\)
 |
 | dotted pair  n.
 |  1. a cons whose cdr is a non-list.
 |  2. any cons, used to emphasize the use of the cons as a symmetric data pair.
 |
 | circular list
 | n. A chain of conses that has no termination because some cons in the
 | chain is the cdr of a later cons.
 |
 | circular  adj.
 |  1. \(of a list\) a circular list.  
 |  2. \(of an arbitrary object\) having a component, element, constituent, or
 |      subexpression \(as appropriate to the context\) that is the object itself.
 `----~%~@
:SEE-ALSO `mon:dotted-list', `mon:circular-list', `mon:proper-list-not-null-p',
`mon:proper-list-p'.~%►►►")

(typedoc 'proper-plist
 "Whether object is of type `mon:proper-list' with length `evenp'.~%~@
A \"proper plist\" includes the emtpy list.
A \"proper plist\" may sharpsign back references e.g. #N# so long as these do
not create circularity.~%~@
:EXAMPLE~%
 \(typep '\(:key \"val\") 'proper-plist\)~%
 \(typep nil 'proper-plist\)~%
 \(typep '\(\) 'proper-plist\)~%
 \(typep (list) 'proper-plist\)~%
 \(typep  \(list #1='a #1#\) 'proper-plist\)~%
;; Following fail succesfully:~%
 \(typep '\(a . b\) 'proper-plist\)~%
 \(typep \(cons 'a  'b\) 'proper-plist\)~%
 \(typep \(list* 'a 'b\) 'proper-plist\)~%
 \(typep \(list* 'a 'b 'c\) 'proper-plist\)~%
 \(typep  \(list* #1='a #1#\) 'proper-plist\)~%~@
:NOTE ANSI Glossary defines the following terms with regards to whether an object is \"proper\":~%
 ,----
 | proper sequence
 | n. A sequence which is not an improper list; that is, a vector or a proper list.
 |
 | proper list
 | n. A list terminated by the empty list. \(The empty list is a proper list.\)
 | 
 | improper list
 | n. A list which is not a proper list: a circular list or a dotted list.
 |
 | dotted list  n.
 | n. A list which has a terminating atom that is not NIL.
 |    \(An atom by itself is not a dotted list, however.\)
 |
 | dotted pair  n.
 |  1. a cons whose cdr is a non-list.
 |  2. any cons, used to emphasize the use of the cons as a symmetric data pair.
 |
 | circular list
 | n. A chain of conses that has no termination because some cons in the
 | chain is the cdr of a later cons.
 |
 | circular  adj.
 |  1. \(of a list\) a circular list.  
 |  2. \(of an arbitrary object\) having a component, element, constituent, or
 |      subexpression \(as appropriate to the context\) that is the object itself.
 `----~%~@
:SEE-ALSO `mon:dotted-list', `mon:circular-list', `mon:proper-list-not-null-p',
`mon:proper-list-p', `mon:plist-error'.~%►►►")

(typedoc 'proper-plist-not-null
         "Like type spec for `mon:proper-list' but does allow the empty list.~%~@
:EXAMPLE~%
 \(typep '\(:key \"val\") 'proper-plist\)~%
 \(typep nil 'proper-plist\)~%
 \(typep '\(a . b\) 'proper-plist\)~%
:SEE-ALSO `mon:dotted-list', `mon:circular-list', `mon:proper-list-not-null-p',
`mon:proper-list-p', `mon:plist-error'.~%►►►")

(typedoc 'dotted-list
"Whether object is of type cons and satisfies `mon:list-dotted-p'.~%~@
:EXAMPLE~%
 \(typep \(cons 'a 'b\) 'dotted-list\)~%
 \(typep '\(a b . c\) 'dotted-list\)~%
 \(typep \(list 'a 'b 'c\) 'dotted-list\)~%
 \(typep nil 'dotted-list\)~%~@
:NOTE A dotted list is an \"improper list\".
ANSI Glossary defines the following terms with regards to dotted lists:~%
 ,----
 | proper list
 | n. A list terminated by the empty list. \(The empty list is a proper list.\)
 | 
 | improper list
 | n. A list which is not a proper list: a circular list or a dotted list.
 |
 | dotted list  n.
 | n. A list which has a terminating atom that is not NIL.
 |    \(An atom by itself is not a dotted list, however.\)
 |
 | dotted pair  n.
 |  1. a cons whose cdr is a non-list.
 |  2. any cons, used to emphasize the use of the cons as a symmetric data pair.
 `----~%~@
:SEE-ALSO `mon:proper-list', `mon:circular-list'.~%►►►")

(typedoc 'circular-list
"Type designator for circular lists.~%~@
Type implemented as a satisfies `mon:list-circular-p' and not recommended for
performance intensive use.~%~@
Main usefullness as the expected-type designator of a TYPE-ERROR.~%~@
The glossary of the ANSI spec defines a circular list as:~%
 ,----
 | circular list  n. 
 | A chain of conses that has no termination because some cons in the
 | chain is the cdr of a later cons.
 |
 | circular  adj.
 |  1. \(of a list\) a circular list.  
 |  2. \(of an arbitrary object\) having a component, element, constituent, or
 |      subexpression \(as appropriate to the context\) that is the object itself.
 `----~%
:SEE-ALSO `mon:circular-list-error', `mon:dotted-list', `mon:list-dotted-p',
`mon:proper-list', `mon:list-proper-p', `mon:circular-list-error',
`cl:*print-circle*'.~%►►►")

(typedoc 'bool-vector
"A bool-vector is of type `cl:simple-bit-vector' with element-type bit.~%~@
:EXAMPLE~%
 \(typep \(make-bool-vector 8 1\) 'bool-vector\)~%
 \(typep \(make-array 8 :element-type 'bit :initial-element 1\) 'bool-vector\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE info node \(info \"\(ansicl\)simple-bit-vector\"\)~%~@
:SEE-ALSO `mon:bit-vector-octet', `mon:byte-array'.~%►►►")

(typedoc 'bit-vector-octet 
         "An object of type: \(simple-bit-vector 8\).~%~@
An object of this type is a bit-vector with the following properties:~%
 - it is not displaced to another array
 - it does not have a fill pointer
 - it is not expressly adjustable~%~@
:EXAMPLE~%
 \(typep \(make-array 8 :element-type 'bit\) 'bit-vector-octet\)~%
 \(typep \(make-array 8 :element-type 'bit :fill-pointer 0\) 'bit-vector-octet\)~%
 \(typep \(make-array 8 :element-type 'bit :adjustable t\) 'bit-vector-octet\)~%
 \(typep \(make-array 8 :element-type '\(unsigned-byte 8\)\) 'bit-vector-octet\)~%
 \(mon:type-expand-all 'bit-vector-octet\)~%~@
:SEE info node \(info \"\(ansicl\)simple-bit-vector\"\)~%~@
:SEE-ALSO `mon:bool-vector', `mon:byte-array'.~%►►►")

(typedoc 'byte-array
"An object of type (simple-array 'unsigned-byte-8 ([* | LENGTH])).~%~@
Length is the upper bounds of the array dimension.~%~@
:NOTE object may also be a simple-vector, if so it must be of type
`mon::%byte-vector' and satisfy `mon::%byte-vector-each-an-unsigned-byte-8'.~%~@
:EXAMPLE~%~@
 \(typep \(sb-ext:string-to-octets \"bubba\"\) 'byte-array\)~%
 \(let \(\(len \(length \"bubba\"\)\)\)~%
   \(typep \(sb-ext:string-to-octets \"bubba\"\) `\(byte-array ,len\)\)\)~%~@
 \(let \(\(len \(length \"bubba\"\)\)\)
   \(typep \(sb-ext:string-to-octets \"bubba\"\) `\(byte-array ,\(1+ len\)\)\)\)~%
 \(typep #\(255 255 255 255\) '\(byte-array 4\)\)~%
 \(typep #\(255 255 255 255\) 'byte-array\)~%~@
:SEE-ALSO `mon:unsigned-byte-8', `mon:bool-vector'.~%►►►")

(typedoc '%byte-vector
"An object of this type is a SIMPLE-VECTOR of the specified LENGTH.~%~@
and satisfying `mon::%byte-vector-each-an-unsigned-byte-8'.~%~@
LENGTH is either '* or a positive integer value.~%~@
Helper composite type for `mon:byte-array'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(typedoc 'fixnum-0-or-over
"The range of fixnums from 0 to `cl:most-positive-fixnum'.~%~@
On a 32bit machine this is equivalent to \(unsigned-byte 29\).~%~@
:EXAMPLE~%
 \(typep 0 'fixnum-0-or-over\)~%
 \(typep most-positive-fixnum 'fixnum-0-or-over\)~%
 \(typep \(1+ most-positive-fixnum\) 'fixnum-0-or-over\)~%
 \(typep -1 'fixnum-0-or-over\)~%~@
:SEE-ALSO `mon:fixnum-exclusive', `mon:octet', `mon:index-or-minus-1'.~%►►►")

(typedoc 'unsigned-byte-128
"An object of type: \(unsigned-byte 128\)~%~@
Octets:        16
Bits:          128
Hex value:     #xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
Decimal value: 340282366920938463463374607431768211455
Octal value:   #o3777777777777777777777777777777777777777777
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `unsigned-byte-64', `unsigned-byte-48', `unsigned-byte-32',
`unsigned-byte-29', `unsigned-byte-16', `unsigned-byte-8'.~%►►►")

(typedoc 'unsigned-byte-96
         "An object of type: \(unsigned-byte 96\)~%~@
Octets:        12 octets
Bits:          96 bits
Hex value:     #xFFFFFFFFFFFFFFFFFFFFFFFF
Decimal value: 79228162514264337593543950335
Octal value:   #o77777777777777777777777777777777
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(typedoc 'unsigned-byte-64
"An object of type: \(unsigned-byte 64\)~%~@
Octets:        8
Bits:          64
Hex value:     #xFFFFFFFFFFFFFFFF
Decimal value: 18446744073709551615
Octal value:   #o1777777777777777777777
Binary value:  #b1111111111111111111111111111111111111111111111111111111111111111~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `unsigned-byte-64', `unsigned-byte-48', `unsigned-byte-32',
`unsigned-byte-29', `unsigned-byte-16', `unsigned-byte-8'.~%►►►")

(typedoc 'unsigned-byte-56
 "An object of type: \(unsigned-byte 56\)~%~@
Octets:        7
Bits:          56
Hex value:     #xFFFFFFFFFFFFFF
Decimal value: 72057594037927935
Octal value:   #o3777777777777777777
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(typedoc 'unsigned-byte-48
"An object of type: \(unsigned-byte 48\)~%~@
Octets:         6
Bits:          48
Hex value:     #xFFFFFFFFFFFF
Decimal value: 281474976710655
Octal value:   #o7777777777777777
Binary value: #b111111111111111111111111111111111111111111111111~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `unsigned-byte-64', `unsigned-byte-48', `unsigned-byte-32',
`unsigned-byte-29', `unsigned-byte-16', `unsigned-byte-8'.~%►►►")

(typedoc 'unsigned-byte-32
"An object of type: \(unsigned-byte 32\)~%~@
Octets:         3
Bits:          32 
Hex value:     #xFFFFFFFF
Decimal value: 4294967295
Octal value:   #o37777777777
Binary value:  #b11111111111111111111111111111111~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `unsigned-byte-64', `unsigned-byte-48', `unsigned-byte-32',
`unsigned-byte-29', `unsigned-byte-16', `unsigned-byte-8'.~%►►►")

(typedoc 'unsigned-byte-29
"An object of type: \(unsigned-byte 29\).~%~@
Octets:         4
Bits:          29
Hex value:     #x1FFFFFFF
Decimal value: 536870911 
Octal value:   #o3777777777
Binary value:  #b00011111111111111111111111111111
:EXAMPLE~%
 \(typep most-positive-fixnum 'unsigned-byte-29\)~%~@
:SEE-ALSO `unsigned-byte-64', `unsigned-byte-48', `unsigned-byte-32',
`unsigned-byte-29', `unsigned-byte-24', `unsigned-byte-16', `unsigned-byte-8',
`mon:fixnum-0-or-over'.~%►►►")

(typedoc 'unsigned-byte-24
"An object of type: \(unsigned-byte 24\)~%~@
Octets:          3
Hex value:      #xFFFFFF
Decimal value:  16777215
Octal value:    #o77777777
Binary value:   #b111111111111111111111111~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `unsigned-byte-64', `unsigned-byte-48', `unsigned-byte-32',
`unsigned-byte-29', `unsigned-byte-16', `unsigned-byte-8'.~%►►►")

(typedoc 'unsigned-byte-16
"An object of type: \(unsigned-byte 16\)~%~@
Octets:          1
Bits:           16 
Hex value:      #xFFFF
Decimal value:  65535
Octal value:    #o177777
Binary value:   #b1111111111111111~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `unsigned-byte-64', `unsigned-byte-48', `unsigned-byte-32',
`unsigned-byte-29', `unsigned-byte-16', `unsigned-byte-8'.~%►►►")

(typedoc 'unsigned-byte-8
"An object of type: \(unsigned-byte 8\)~%~@
Octets:         1
Bits:           8
Hex value:      #xFF
Decimal value:  255
Octal value:    #o377
Binary value:   #b11111111~%~@
:EXAMPLE~%~@
 \(typep 88 'unsigned-byte-8\)~%~@
:SEE-ALSO `unsigned-byte-64', `unsigned-byte-48', `unsigned-byte-32',
`unsigned-byte-29', `unsigned-byte-16', `unsigned-byte-8'.~%►►►")

(typedoc 'octet
"An object of type: \(unsigned-byte 8\)~%~@
Octets:         1
Bits:           8
Hex value:      #xFF
Decimal value:  255
Octal value:    #o377
Binary value:   #b11111111~%~@
:EXAMPLE~%
 \(typep 88 'octet\)~%~@
:SEE-ALSO `nibble', `unsigned-byte-128', `unsigned-byte-64', `unsigned-byte-48',
`unsigned-byte-32', `unsigned-byte-29', `unsigned-byte-16',
`unsigned-byte-8'.~%►►►")

(typedoc 'nibble
         "An object of type \(unsigned-byte 4\).~%~@
Bits: 4 bits
Hex value:  #xF
Decimal Value 16
Octal value: #o17 
Binary value #b1111~%~@
:EXAMPLE~%
 \(typep 15 'nibble\)~%
 \(not \(typep 16 'nibble\)\)~%~@
:NOTE word/octet/nibble/bit 
Equivalent to \(mod 16\)~%
:SEE-ALSO `octet', `unsigned-byte-128', `unsigned-byte-64', `unsigned-byte-48',
`unsigned-byte-32', `unsigned-byte-29', `unsigned-byte-16',
`unsigned-byte-8'.~%►►►")

(typedoc 'code-point
 "The subtype of integers just big enough to hold all Unicode codepoints.~%~@
This is specified as \(mod #x110000\), e.g. \(integer 0 1114111\) where the
upper bounds on the Unicode range is \(mod #x110000\). 
On SBCL with UTF enabled character set this \(1- `cl:char-code-limit'\)~%~@
:EXAMPLE~%
 (typep 1114111 'code-point)
 (not (typep 1114112 'code-point)) ~%~@
:NOTE The type `mon:char-code-integer' and `mon:code-point' are not the same b/c
`cl:char-code-limit' is not necessarily tied to Unicode character set.
:SEE (URL `http://unicode.org/glossary/#C').~%~@
:SEE-ALSO `mon:octet', `cl:char-int'.~%►►►")

(typedoc 'char-code-integer
"Subtype of integers bounded by the range 0 - `cl:char-code-limit'.~%~@
The is 1- upper exclusive bound on values produced by `cl:char-code'.~%~@
:EXAMPLE~%~@
 \(typep \(1- char-code-limit\) 'char-code-integer\)~%
 \(not \(typep char-code-limit 'char-code-integer\)\)~%~@
:NOTE The type `mon:char-code-integer' and `mon:code-point' are not the same b/c
`cl:char-code-limit' is not necessarily tied to Unicode character set.
:SEE-ALSO `mon:whitespace-char', `mon:base-char-p', `cl:graphic-char-p'.~%►►►")

(typedoc 'char-or-char-code-integer
"An object of type `cl:character' or `mon:char-code-integer'.~%~@
:EXAMPLE~%
 \(typep #\\a 'char-or-char-code-integer\)~%
 \(typep 1114111 'char-or-char-code-integer\)~%~@
;; Following fail successfully:~%~
 \(typep char-code-limit 'char-or-char-code-integer\)~%
 \(typep -1 'char-or-char-code-integer\)~%~@
:SEE-ALSO `mon:code-point', `cl:char-code-limit', `mon:each-a-char-code-integer',
`mon:each-a-character-or-char-code-integer'.~%►►►")

(typedoc 'whitespace-char
  "A character is of this type if `eql' a member of `mon:*whitespace-chars*'~%~@
:EXAMPLE~%
 \(and  \(typep #\\NEWLINE 'whitespace-char\)
       \(not \(typep #\\a 'whitespace-char\)\)\)~%~@
:SEE-ALSO `mon:whitespace-char-p', `cl:char-code-limit' `cl:char-code',
`cl:code-char', `cl:char-name', `cl:name-char', `cl:char-upcase',
`cl:char-downcase', `cl:char-int', `cl:schar', `cl:digit-char', `cl:character',
`cl:base-char', `cl:standard-char', `cl:extended-char',`cl:standard-char-p',
`cl:graphic-char-p', `cl:alpha-char-p',`cl:digit-char-p', `cl:alphanumericp',
`cl:upper-case-p', `cl:lower-case-p', `cl:both-case-p', `cl:char=', `cl:char/=',
`cl:char<', `cl:char>', `cl:char<=', `cl:char>=', `cl:char-equal',
`cl:char-not-equal'.~%►►►")

(typedoc 'char-not-whitespace-char
"Whether object is of type cl:character and not a member `mon:*whitespace-chars*'.~%~@
:EXAMPLE~%
 \(char-not-whitespace-char #\\a\)~%
 \(char-not-whitespace-char #\\Tab\)~%
 \(notany #'\(lambda \(x\) \(typep x 'char-not-whitespace-char\)\) *whitespace-chars*\)~%~@
:SEE-ALSO `mon:whitespace-char', `mon:whitespace-char-p',
`mon:*whitespace-chars*'.~%►►►")

(typedoc 'hexadecimal-char
"An object that is of type `cl:standard-char' and member of `mon:*hexadecimal-chars*'.~%~@
:EXAMPLE~%
 \(typep #\\F 'hexadecimal-char\)~%
 \(typep #\\f 'hexadecimal-char\)~%
 \(typep #\\0 'hexadecimal-char\)~%
 \(typep #\\► 'hexadecimal-char\)~%~@
:SEE-ALSO `mon:whitespace-char', `cl:char-code-limit' `cl:char-code',
`cl:code-char', `cl:char-name', `cl:name-char', `cl:char-upcase',
`cl:char-downcase', `cl:char-int', `cl:schar', `cl:digit-char', `cl:character',
`cl:base-char', `cl:standard-char', `cl:extended-char',`cl:standard-char-p',
`cl:graphic-char-p', `cl:alpha-char-p',`cl:digit-char-p', `cl:alphanumericp',
`cl:upper-case-p', `cl:lower-case-p', `cl:both-case-p', `cl:char=', `cl:char/=',
`cl:char<', `cl:char>', `cl:char<=', `cl:char>=', `cl:char-equal',
`cl:char-not-equal'.~%►►►")

(typedoc 'string-or-char
"An object that is either of type `cl:string' or a `cl:character'.~%~@
:EXAMPLE~%
 \(typep \"stringy\" 'string-or-char\)~%
 \(typep #\\► 'string-or-char\)~%
 \(typep 9658 'string-or-char\)~%~@
:SEE-ALSO `mon:string-or-char-or-code-point-integer',
`mon:string-or-char-or-code-point-integer-if'.~%►►►")

(typedoc 'string-or-char-or-code-point-integer
"An object of type `mon:string-or-char' or `mon:code-point'.~%~@
Or, when not sb-unicode an object of type `char-code-integer'.~%~@
:EXAMPLE~%
 \(typep \"stringy\" 'string-or-char-or-code-point-integer\)~%
 \(typep #\\► 'string-or-char-or-code-point-integer\)~%
 \(typep 9658 'string-or-char-or-code-point-integer\)~%~@
:SEE-ALSO `mon:string-or-char', `mon:string-or-char-or-code-point-integer',
`mon:string-or-char-or-code-point-integer-if'.~%►►►")

#+sbcl 
(typedoc 'closure-obj
"Non-nil when object satisfies predicate `sb-impl::closure-p'.~%~@
:EXAMPLE~%
 \(prog2 
     \(progn \(defparameter *my-clsr* nil\)
	    \(defun mk-clsr \(my-clsr\) \(lambda \(\) \(incf my-clsr\)\)\)
	    \(setf *my-clsr* \(mk-clsr 10\)\)\)
     \(typep *my-clsr* 'closure-obj\)
   \(unintern '*my-clsr*\)\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(typedoc 'array-length
"Type designator for a dimension of an array of LENGTH.~%~@
LENGTH is an integer between 0 (inclusive) and LENGTH (inclusive).~%~@
LENGTH defaults to ARRAY-DIMENSION-LIMIT.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:index', `index-or-minus-1', `mon:array-index',
`mon:array-length', `cl:array-dimension-limit',
`cl:most-positive-fixnum'.~%►►►")

(typedoc 'array-index
  "Type designator for an index into array.~%~@
When provided LENGTH an integer between 0 (inclusive) and LENGTH (exclusive).
LENGTH defaults to `cl:array-dimension-limit'.~%~@
For SBCL on a 32bit system LENGTH defaults to the integer range:~%
 [0,\(- most-positive-fixnum 2\)]~%~@
Type for indexing into arrays, and \"stepped\" quantities, e.g. list lengths.~%~@
This type def is courtesy SBCL, which says:~%
 ,----
 | It's intentionally limited to one less than the
 | `array-dimension-limit' for efficiency reasons, because in SBCL
 | `array-dimension-limit' is `most-positive-fixnum', and staying below
 | that lets the system know it can increment a value of this type
 | without having to worry about using a bignum to represent the
 | result.
 |  
 | \(It should be safe to use ARRAY-DIMENSION-LIMIT as an exclusive
 |  bound because ANSI specifies it as an exclusive bound.\)
 |  
 `---- :SEE :FILE sbcl/src/code/early-extensions.lisp~%~@
:EXAMPLE~%
 \(typep 0 'array-index\)~%
 \(typep -1 'array-index\)~%
 \(typep array-dimension-limit 'array-index\)~%
 \(typep (1- array-dimension-limit) 'array-index\)~%
 \(typep most-positive-fixnum 'array-index\)~%
 \(typep \(-  most-positive-fixnum 3\) 'array-index\)~%~@
:SEE-ALSO `mon:index', `index-or-minus-1', `mon:array-index',
`mon:array-length', `cl:array-dimension-limit',
`cl:most-positive-fixnum'.~%►►►")

(typedoc 'index
"Type for indexing into arrays, and \"stepped\" quantities, e.g. list lengths.~%~@
On SBCL, an index this is integer range:~%
 [0,CL:ARRAY-DIMENSION-LIMIT]~%~@
On non-SBCL systems this is the integer range:~%~@
 [0,#x1FFFFFFD]~%~@
:EXAMPLE
 \(typep cl:array-dimension-limit 'index\)~%
 \(typep \(1+ cl:array-dimension-limit\) 'index\)~%
 \(typep #x1FFFFFFD 'index\)~%
 \(typep \(1+ #x1FFFFFFD\) 'index\)~%~@
:SEE-ALSO `mon:index-or-minus-1', `mon:array-index', `mon:fixnump',
`mon:bignump', `cl:array-dimension-limit' `cl:most-positive-fixnum'.~%►►►")

(typedoc 'index-plus-1 
"Type for indexing into arrays in cl:loop forms where the stepped quantity may
increment from 0 \(or above\) by 1 to a value one beyond `cl:array-dimension-limit'~%~@
On SBCL this is the range:~%
 [0,\(1+ array-dimension-limit\)]~%~@
On non-SBCL systems this is the range:~%
 [0,#x1FFFFFFE]~%~@
:EXAMPLE~%
 \(typep cl:array-dimension-limit 'index-plus-1\)~%
 \(typep \(1+ cl:array-dimension-limit\) 'index-plus-1\)~%
 \(typep #x1FFFFFFE 'index-plus-1\)~%
 \(typep \(1- #x1FFFFFFF\) 'index\)~%
 \(typep #x1FFFFFFF 'index\)~%~@
:SEE-ALSO `mon:index-or-minus-1', `mon:array-index', `mon:index',
`mon:array-length', `mon:fixnump', `mon:bignump', `cl:array-dimension-limit'
`cl:most-positive-fixnum'.~%►►►")

(typedoc 'index-or-minus-1
"Type for indexing into arrays, and \"stepped\" quantities, e.g. list lengths.~%~@
Like `mon:index', but with the lower bounds at -1.~%~@
Useful when stepping an index by count downwards to 0, e.g.:~%
 \(loop for (the index-or-minus-1 i) from n downto 0\)~%~@
where the implementation might terminate the loop by testing an index
leaving the loop range.~%~@
:NOTE `cl:most-positive-fixnum' is specified as being:~%~
  \(and \(<= \(1- \(expt 2 15\)\) <M-P-F>\)
       \(<= array-dimension-limit <M-P-F>\)\)~%~@
:SEE-ALSO `mon:index-plus-1', `mon:array-index', `mon:index', `mon:fixnump', `mon:bignump'.~%►►►")

(typedoc 'fixnum-exclusive
"A bounded range of integer values one less than unary stepping iterators can access.~%~@
Like `cl:fixnum' but the range of integers is between:~%
 \(lognot array-dimension-limit\) and `cl:array-dimension-limit'~%~@
On a 32bit machine this is likely from -536870910 to 536870909.~%~@
:EXAMPLE~%~@
 \(typep  42 'fixnum-exclusive\)~%
 \(typep \(+ most-negative-fixnum 2\) 'fixnum-exclusive\)~%
 \(typep \(- most-positive-fixnum 2\) 'fixnum-exclusive\)~%
 \(typep most-negative-fixnum 'fixnum-exclusive\)~%
 \(typep most-positive-fixnum 'fixnum-exclusive\)~%~@
:SEE-ALSO `mon:fixnump', `mon:bignump', `mon:index', `mon:array-index',
`mon:index-or-minus-1'.~%►►►")

(typedoc 'standard-char-or-null
"An object of type `cl:standard-char' or null.~%~@
:EXAMPLE~%
 \(typep #\\a 'standard-char-or-null\)~%
 \(typep nil 'standard-char-or-null\)~%
 \(typep #\\é 'standard-char-or-null\)~%~@
:SEE-ALSO `mon:proper-list-not-null', `mon:not-null', `mon:base-char-p'.~%►►►")

(typedoc 'digit-char-0-or-1
"An object which is null or satisfies `mon:digit-char-0-or-1-p'.~%~@
:EXAMPLE~%~@
 \(typep #\\1 'digit-char-0-or-1-p\)~%
 \(typep #\\0 'digit-char-0-or-1-p\)~%
 \(typep nil  'digit-char-0-or-1-p\)~%
 \(typep #\\2 'digit-char-0-or-1-p\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(typedoc 'string-all-digit-char-0-or-1
"An object which is is null or a simple-string of which every character
satisfies `mon:digit-char-0-or-1-p'.~%~@
:EXAMPLE~%
 \(let* \(\(chars-ok  \"010101\"\)
        \(chars-no \(concatenate 'string chars-ok \(list #\\8\)\)\)\)
   \(and \(not \(assert \(typep chars-ok 'string-all-digit-char-0-or-1\)\)\)
        \(format t \"Local var `chars-ok` is OK, got: ~~a\" chars-ok\)\)
   \(let \(\(not-ok chars-no\)\)
     \(declare \(string-all-digit-char-0-or-1 not-ok\)\)
     not-ok\)\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(typedoc 'not-null
"An object witch is not null.~%~@
:EXAMPLE~%
 \(typep \(\)  'not-null\)~%
 \(typep nil 'not-null\)~%
 \(typep t   'not-null\)~%
:SEE-ALSO `mon:proper-list-not-null', `mon:standard-char-or-null',
`mon:booleanp', `cl:null'.~%►►►")

(typedoc 'symbol-not-null
"Whether OBJECT is symbolp and not null.~%~@
:EXAMPLE~%
 \(typep nil     'symbol-not-null\)~%
 \(typep \(\)     'symbol-not-null\)~%
 \(typep \(cons nil nil\) 'symbol-not-null\)~%
 \(typep 'bubba 'symbol-not-null\)~%
 \(typep 't     'symbol-not-null\)~%
 \(typep '8     'symbol-not-null\)~%~@
:SEE-ALSO `mon:not-null', `mon:symbol-not-a-constant'.~%►►►")

(typedoc  'proper-list-not-null
"A list of type `cl:not-null' satisfying `mon:list-proper-p'.~%~@
:EXAMPLE~%
 \(typep '\(a b c\) 'proper-list-not-null\)~%
 \(typep  nil 'proper-list-not-null\)~%
 \(typep  \(\) 'proper-list-not-null\)~%
 \(typep  '\(\) 'proper-list-not-null\)~%
 \(typep  '\(a b . c\) 'proper-list-not-null\)~%
 \(typep  '\(a . c\) 'proper-list-not-null\)~%~@
:SEE-ALSO `mon:proper-list-not-null', `mon:booleanp', `cl:null'.~%►►►")

(typedoc 'each-a-simple-string
"A proper list of type `mon:proper-list-not-null' satisfying  `mon:each-a-simple-string-p'.~%~@
:EXAMPLE~%
 \(typep '\(\"a\" \"b\" \"c\" \"d\"\) 'each-a-simple-string\)~%~@
:SEE-ALSO `mon:proper-list', `mon:proper-list-p', `mon:not-null',
`cl:simple-string-p', `cl:simple-string', `cl:null'.~%►►►")

(typedoc 'string-with-fill-pointer 
"Whether object is of type `cl:vector' with element of type either
`cl:base-char' or `cl:character' and satisfies `mon:vector-with-fill-pointer-p'.~%~@
:EXAMPLE~%
 \(typep \"string\" 'string-with-fill-pointer\)~%
 \(typep \(make-array 6 
                    :element-type 'base-char
                    :initial-contents \"string\" 
                    :fill-pointer 6\)
        'string-with-fill-pointer\)~%~@
:SEE-ALSO `mon:string-with-fill-pointer-p', `mon:vector-with-fill-pointer-p',
`cl:vectorp', `cl:array-has-fill-pointer-p'.~%►►►")

(typedoc 'each-a-sequence
 "Whether each elt in object satisfies `mon:each-a-sequence-p'.~%~@
:EXAMPLE~%
 \(typep nil 'each-a-sequence\)~%
 \(typep 42 'each-a-sequence\)~%
 \(typep '\(\"a\"  #\(b\) '\(\) \(b\) nil\) 'each-a-sequence\)~%
 \(typep '\(\"a\" #\(b\) \(b\) \(a . b\)  \(a b . c\)\) 'each-a-sequence\)~%~@
:SEE-ALSO `mon:each-a-sequence', `mon:each-a-sequence-proper',
`mon:each-a-sequence-proper-or-character', `mon:each-a-sequence-p',
`mon:each-a-sequence-proper-or-character-p',
`mon:each-a-sequence-proper-p'.~%►►►")

(typedoc 'each-a-sequence-proper
 "Whether object is `sequencep' and satisfies `mon:each-a-sequence-proper-p'~%~@
:EXAMPLE~%
 \(typep nil 'each-a-sequence-proper\)~%
 \(typep 42 'each-a-sequence-proper\)~%
 \(typep '\(\"a\"  #\(b\) '\(\) \(b\) nil\) 'each-a-sequence-proper\)~%
 \(typep '\(\"a\" #\(b\) \(b\) \(a . b\)  \(a b . c\)\) 'each-a-sequence-proper\)~%~@
:NOTE ANSI Glossary defines the following terms with regards to whether an object is \"proper\":~%
 ,----
 | proper sequence
 | n. A sequence which is not an improper list; that is, a vector or a proper list.
 |
 | proper list
 | n. A list terminated by the empty list. \(The empty list is a proper list.\)
 | 
 | improper list
 | n. A list which is not a proper list: a circular list or a dotted list.
 |
 | dotted list  n.
 | n. A list which has a terminating atom that is not NIL.
 |    \(An atom by itself is not a dotted list, however.\)
 |
 | dotted pair  n.
 |  1. a cons whose cdr is a non-list.
 |  2. any cons, used to emphasize the use of the cons as a symmetric data pair.
 |
 | circular list
 | n. A chain of conses that has no termination because some cons in the
 | chain is the cdr of a later cons.
 |
 | circular  adj.
 |  1. \(of a list\) a circular list.  
 |  2. \(of an arbitrary object\) having a component, element, constituent, or
 |      subexpression \(as appropriate to the context\) that is the object itself.
 `----~%~@
:SEE-ALSO `mon:each-a-sequence', `mon:each-a-sequence-proper',
`mon:each-a-sequence-proper-or-character', `mon:each-a-sequence-p',
`mon:each-a-sequence-proper-or-character-p',
`mon:each-a-sequence-proper-p'.~%►►►")

(typedoc 'each-a-string-of-length-1-p
         "An object which is a proper list with each element satisfying `mon:string-of-length-1-p'.
:EXAMPLE~%
 \(typep '\(\"a\"\)  'each-a-string-of-length-1\)~%
 \(typep '\(\"a\" \"b\" \"c\" \"d\"\)  'each-a-string-of-length-1\)~%
 \(typep `\(\"a\" ,\(make-array 1 :element-type 'character :adjustable t\)\) 
        'each-a-string-of-length-1\)~%
 \(typep `\(\"a\" ,\(make-array 1 :element-type 'character :fill-pointer 0\)\) 
        'each-a-string-of-length-1\)~%~@
;; Following fail successfully:~%~
 \(typep '\(\"\" \"b\" \"c\" \"d\"\)  'each-a-simple-string-of-length-1\)~%
 \(typep '\(\" a\" \"b\" \"c\" \"d\"\)  'each-a-simple-string-of-length-1\)~%
 \(typep '\(\"a\" \"b\" \"c\" 100\)  'each-a-string-of-length-1\)~%
 \(typep '\(\"a\" \"b\" \"c\" #\\d\)  'each-a-string-of-length-1\)~%
 \(typep  '\(\"a\" \"b\" \"c\" nil \"d\"\) 'each-a-string-of-length-1\)~%
 \(typep nil 'each-a-string-of-length-1\)~%~@
:SEE-ALSO `mon:each-a-simple-string-of-length-1-p',
`mon:each-a-simple-string-p', `mon:each-a-simple-string-p'.~%►►►")

(typedoc 'each-a-simple-string-of-length-1
	"An object which is a proper list with each element satisfying `mon:simple-string-of-length-1-p'.
:EXAMPLE~%
 \(typep '\(\"a\")  'each-a-simple-string-of-length-1\)~%
 \(typep '\(\"a\" \"b\" \"c\" \"d\"\)  'each-a-simple-string-of-length-1\)~%~@
;; Following fail successfully:~%
 \(typep '\(\"\" \"b\" \"c\" \"d\"\)  'each-a-simple-string-of-length-1\)~%
 \(typep '\(\" a\" \"b\" \"c\" \"d\"\)  'each-a-simple-string-of-length-1\)~%
 \(typep `\(\"a\" \"b\" \"c\" ,\(make-array 1 :element-type 'character :adjustable t\)\) 
        'each-a-simple-string-of-length-1\)~%
 \(typep `\(\"a\" \"b\" \"c\" ,\(make-array 1 :element-type 'character :fill-pointer 0\)\) 
        'each-a-simple-string-of-length-1\)
 \(typep '\(\"a\" \"b\" \"c\" 100\)  'each-a-simple-string-of-length-1\)~%
 \(typep '\(\"a\" \"b\" \"c\" #\\d\)  'each-a-simple-string-of-length-1\)~%
 \(typep  '\(\"a\" \"b\" \"c\" nil \"d\"\) 'each-a-simple-string-of-length-1\)~%
 \(typep nil 'each-a-simple-string-of-length-1\)~%~@
:SEE-ALSO `mon:each-a-string-of-length-1-p', `mon:each-a-simple-string-p',
`mon:each-a-simple-string-p'.~%►►►")

(typedoc 'mon:each-a-sequence-proper-or-character
 "Whether object is `sequencep' and satisfies `mon:each-a-sequence-proper-or-character-p'.~%~@
:EXAMPLE~%
 \(typep '\(\"a\" #\\a #\(b\) '\(\) \(b\) nil\) 'each-a-sequence-or-character\)~%
 \(typep '\(\"a\" #\\a #\(b\) \(b\) \(a . b\)  \(a b . c\)\) 'each-a-sequence-or-character\)~%~@
:SEE-ALSO `mon:each-a-sequence', `mon:each-a-sequence-proper',
`mon:each-a-sequence-proper-or-character', `mon:each-a-sequence-p',
`mon:each-a-sequence-proper-or-character-p',
`mon:each-a-sequence-proper-p'.~%►►►")


;;; ==============================
;;; :TYPES-FUNCTIONS
;;; ==============================

(fundoc 'symbol-not-a-constantp
"Whether SYMBOL is `cl:symbolp' and not `cl:constantp'.~%~@
When true return SYMBOL else nil.
:EXAMPLE~%
 (symbol-not-a-constantp nil)
 (symbol-not-a-constantp 'bubba)
 \(symbol-not-a-constantp \"bubba\"\)~%
:SEE-ALSO `mon:symbol-not-null-or-string-not-empty-p'.~%►►►")

(fundoc 'symbol-not-null-or-string-not-empty-p
"Whether OBJECT is of type `mon:symbol-not-null-or-string-not-empty'.~%~@
:EXAMPLE~%
 \(symbol-not-null-or-string-not-empty-p t\)~%
 \(symbol-not-null-or-string-not-empty-p 'b\)~%
 \(symbol-not-null-or-string-not-empty-p \"b\"\)~%
 \(symbol-not-null-or-string-not-empty-p \"\"\)~%
 \(symbol-not-null-or-string-not-empty-p nil\)~%~@
:SEE-ALSO `mon:symbol-not-a-constantp'.~%►►►")

(fundoc 'define-list-of-predicate
"Return a function object which checks if elements of its OBJECT arg are of TYPE.
Name of returned function object is as per `mon:make-list-of-predicate-name'.~%~@
Suitable for use with `cl:deftype' in a satisfies form.~%~@
:EXAMPLE~%
 \(macroexpand-1 '\(define-list-of-predicate integer\)\)~%
 \(define-list-of-predicate integer\)
 ;=> each-an-integer-p~%
 (deftype each-an-integer ()
   '(satisfies each-an-integer-p))
 ; => each-an-integer~%
 \(typep '\(1 2 3\) 'each-an-integer\)
 ;=> T~%
 \(each-an-integer-p '\(1 2 3\)\) 
 ;=> T~%
 \(each-an-integer-p '\(1 2 a 3\)\)
 ;=> NIL~%
 \(typep '\(1 2 a 3\) 'each-an-integer\)
 ;=> NIL~%
 \(typep '\(1 2 . 3\) 'each-an-integer\)~%~@
;; :NOTE Following signals an error whereas above `typep' form does not:~%
 \(each-an-integer-p '\(1 2 . 3\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'make-list-of-predicate-name
"Make an internable string from symbol-name of TYPE.~%~@
Helper function for `mon:define-list-of-predicate' macro.~%~@
When char at index 0 of TYPE's symbol-name is a member of `mon:*vowel-chars*'
return value has the format:~%
 (\"EACH-AN-<TYPE>-P\" . (\"EACH-AN-<TYPE>\"\)
Else, return value has the format:~%
 (\"EACH-A-<TYPE>-P\" . (\"EACH-A-<TYPE>\"\)
:EXAMPLE~%
 \(make-list-of-predicate-name 'integer\)
 \(make-list-of-predicate-name 'function\)
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'type-any
"Like `cl:notany' except for type predicates and:~%
 - Handles two-`cl:values' predicate functions, as `cl:subtypep' does.
   And, if the result is uncertain returns (values nil nil)
   as `cl:subtypep' does;~%
 - Arg THING is just an atom;
 - The funcall on OP \(an arity-2 function\) occurs successively to THING and each
   element of LIST.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:type-not', `mon:type-any', `mon:type-every', `cl:typep',
`cl:subtypep', `cl:type-of', `cl:typecase', `cl:etypecase'.~%►►►")

(fundoc 'type-every
"Like `cl:every', except for type predicates and:
 - Handles two-`cl:values' predicate functions, as `cl:subtypep' does.
   And, if the result is uncertain returns (values nil nil)
   as `cl:subtypep' does;~%
 - Arg THING is just an atom;~%
 - The apply to OP (an arity-2 function) occurs successively to THING and each
   element of LIST.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:type-not', `mon:type-any', `mon:type-every', `cl:typep',
`cl:subtypep', `cl:type-of', `cl:typecase', `cl:etypecase'.~%►►►")

(fundoc 'type-equal
"Whether TYPE1 and TYPE2 denote the same type.~%~@
:EXAMPLE~%
 \(type-equal 'string 'string\)~%~@
:SEE-ALSO `mon:type-every', `mon:type-any' `mon:type-not', `cl:typep',
`cl:subtypep', `cl:type-of', `cl:typecase', `cl:etypecase'.~%►►►")

(fundoc 'list-circular-p
 "Returns true if OBJECT is a circular list, NIL otherwise.~%~@
:EXAMPLE~%
 \(let \(\(*print-circle* t\)\)
   \(labels \(\(make-circular  \(list\) \(setf \(cdr \(last list\)\) list\) list\)\)
     \(list-circular-p \(make-circular '\(a b c\) \)\)\)\)~%
:NOTE A circular list is an improper list.
ANSI Glossary defines \"proper list\" and \"improper list\" as follows:~%
 ,----
 | proper list
 | n. A list terminated by the empty list. \(The empty list is a proper list.\)
 | 
 | improper list
 | n. A list which is not a proper list: a circular list or a dotted list.
 `----~%~@
:SEE-ALSO `mon:circular-list', `mon:circular-list-error', `mon:list-proper-p',
`mon:list-dotted-p'.~%►►►")

(fundoc 'list-dotted-p
 "Whether LIST is a dotted pair, i.e. an improper list.~%~@
When LIST is `cl:listp' return value is as if by `cl:values'.~@
When LIST a dotted pair first value is T, second is 'dotted-list.~%~@
When LIST is null first value is NIL, second is 'null.~%~@
When LIST is `mon:list-cicular-p' first is NIL, second is 'circular-list.~%~@
When LIST is not null and `mon:list-proper-p' first is NIL, second is 'proper-list.~%~@
When list is none of the above return one value NIL.~%~@
:EXAMPLE~%
 \(list-dotted-p \(cons 'a 'b\)\)~%
 \(list-dotted-p \(cons 'q \(cons 'a 'b\)\)\)~%
 \(list-dotted-p \(list 'q 'a 'b\)\)~%
 \(list-dotted-p \(list 'a nil\)\)~%
 \(list-dotted-p \(cons 'a nil\)\)~%
 \(list-dotted-p \(cons nil 'a\)\)~%
 \(list-dotted-p nil\)~%
 \(labels \(\(make-circular \(list\) 
            \(setf \(cdr \(last list\)\) list\) list\)\)
   \(list-dotted-p \(make-circular '\(a b c\)\)\)\)~%~@
:NOTE ANSI Glossary defines \"proper list\" and \"improper list\" as follows:~%
 ,----
 | improper list
 | n. A list which is not a proper list: a circular list or a dotted list.
 |
 | proper list
 | n. A list terminated by the empty list. \(The empty list is a proper list.\)
 `----~%~@
:SEE-ALSO `list-proper-not-null-p', `mon:list-proper-p', `mon:list-circular-p',
`mon:circular-list', `mon:circular-list-error'.~%►►►")

(fundoc 'list-proper-p
  "Returns true if OBJECT is a proper list.~%~@
:EXAMPLE~%
 \(list-proper-p '\(a . b\)\)~%
 \(list-proper-p '\(a b c\)\)~%
 \(list-proper-p '\(\)\)~%
 \(list-proper-p nil\)~%~@
:NOTE ANSI Glossary defines the following terms with regards to whether an object is \"proper\":~%
 ,----
 | proper sequence
 | n. A sequence which is not an improper list; that is, a vector or a proper list.
 |
 | proper list
 | n. A list terminated by the empty list. \(The empty list is a proper list.\)
 | 
 | improper list
 | n. A list which is not a proper list: a circular list or a dotted list.
 |
 | dotted list  n.
 | n. A list which has a terminating atom that is not NIL.
 |    \(An atom by itself is not a dotted list, however.\)
 |
 | dotted pair  n.
 |  1. a cons whose cdr is a non-list.
 |  2. any cons, used to emphasize the use of the cons as a symmetric data pair.
 |
 | circular list
 | n. A chain of conses that has no termination because some cons in the
 | chain is the cdr of a later cons.
 |
 | circular  adj.
 |  1. \(of a list\) a circular list.  
 |  2. \(of an arbitrary object\) having a component, element, constituent, or
 |      subexpression \(as appropriate to the context\) that is the object itself.
 `----~%~@
:SEE-ALSO `mon:proper-list', `mon:list-proper-not-null-p', `mon:sequencep',
`mon:list-dotted-p', `mon:list-circular-p', `mon:circular-list',
`mon:circular-list-error', `mon:sequence-zerop', `mon:sequence-type',
`cl:list-length', `cl:list*', `cl:cons', `cl:consp', `cl:atom'.~%►►►")

(fundoc 'list-proper-not-null-p
"Whether OBJECT is of type `mon:proper-list-not-null'.~%~@
An object is of this type if it is of type `mon:not-null' and satisfies
`mon:list-proper-p'.~%~@
:EXAMPLE~%
 \(list-proper-not-null-p '\(a proper list\)\)~%
 \(list-proper-not-null-p \(cons nil nil\)\) ;; Maybe no what you expected.~%
 \(list-proper-not-null-p \(cons nil t\)\)~%
 \(list-proper-not-null-p  \(list nil t\)~%
 \(list-proper-not-null-p \(list* nil t\)\)~%
 \(list-proper-not-null-p \(\)\)~%
 \(list-proper-not-null-p nil\)~%
 \(list-proper-not-null-p \(list\)\)~%
 \(list-proper-not-null-p \(list\)\)~%
 \(list-proper-not-null-p #\(\)\)~%~@
:SEE-ALSO `mon:sequencep', `sequence-zerop', `sequence-type', `cl:list-length',
`cl:list*', `cl:cons', `cl:consp', `cl:atom'.~%►►►")

(fundoc 'plist-proper-p
"Whether object is a plist.~%~@
Return T when object satisfies `mon:list-proper-p' and is of `cl:length' which
is `cl:evenp'.~%~@
:EXAMPLE~%~@
 \(typep '\(:key \"val\") 'proper-plist\)~%
 \(typep nil 'proper-plist\)~%
 \(typep '\(a . b\) 'proper-plist\)~%~@
:NOTE ANSI defines the following terms with regards to property lists:
,----
| Property list
|  The property list of a symbol provides a mechanism for associating
|  named attributes with that symbol.
| 
|  The operations for adding and removing entries are destructive to the
|  property list.
| 
|  Common Lisp provides operators both for direct manipulation of property
|  list objects \(e.g., `cl:getf', `cl:remf', and `cl:symbol-plist'\) and
|  for implicit manipulation of a symbol's property list by reference to
|  the symbol \(e.g., `cl:get' and `cl:remprop'\).
| 
|  The property list associated with a fresh symbol is initially null.
|
| property list n.
|  1. A list containing an even number of elements that are alternating
|  names \(sometimes called indicators or keys\) and values \(sometimes
|  called properties\).
|
|  When there is more than one name and value pair with the identical
|  name in a property list, the first such pair determines the property.
|
|  2. \(of a symbol\) The component of the symbol containing a property
|  list.
`----~%~@
:NOTE ANSI Glossary defines the following terms with regards to whether an object is \"proper\":~%
 ,----
 | proper sequence
 | n. A sequence which is not an improper list; that is, a vector or a proper list.
 |
 | proper list
 | n. A list terminated by the empty list. \(The empty list is a proper list.\)
 | 
 | improper list
 | n. A list which is not a proper list: a circular list or a dotted list.
 |
 | dotted list  n.
 | n. A list which has a terminating atom that is not NIL.
 |    \(An atom by itself is not a dotted list, however.\)
 |
 | dotted pair  n.
 |  1. a cons whose cdr is a non-list.
 |  2. any cons, used to emphasize the use of the cons as a symmetric data pair.
 |
 | circular list
 | n. A chain of conses that has no termination because some cons in the
 | chain is the cdr of a later cons.
 |
 | circular  adj.
 |  1. \(of a list\) a circular list.  
 |  2. \(of an arbitrary object\) having a component, element, constituent, or
 |      subexpression \(as appropriate to the context\) that is the object itself.
 `----~%~@
:SEE \(info \"\(ansicl\)symbol\"\)~%
:SEE-ALSO `mon:proper-plist', `mon:plist-proper-not-null-p',
`mon:proper-plist-not-null', `mon:plist-error', `cl:get-properties'.~%►►►")

(fundoc 'plist-proper-not-null-p 
"Like `mon:plist-proper-p' but do not consider empty list as of type `mon:proper-list'~%~@
:EXAMPLE~%~@
 \(plist-proper-p '\(:key \"val\"\)\)
 \(plist-proper-not-null-p '\(:key \"val\"\)\)
 \(plist-proper-p '\(\)\)
 \(plist-proper-not-null-p '\(\)\)
:SEE-ALSO `mon:plist-proper-not-null', `mon:plist-error'.~%►►►")

(fundoc 'sequence-zerop
"Returns non-nil iff the sequence SEQ has zero length.~%~@
Works in constant time even with lists.~%~@
:EXAMPLE~%
 \(sequence-zerop \(\)\)~%
 \(sequence-zerop nil\)~%
 \(sequence-zerop '\(\)\)~%
 \(sequence-zerop \"\"\)~%
 \(sequence-zerop #\(\)\)~%
 \(sequence-zerop \(make-array 0\)\)~%
 \(sequence-zerop '\(nil . nil\)\)~%
 \(sequence-zerop #*00000\)~%~@
:SEE-ALSO `mon:proper-list-not-null', `mon:not-null', `mon:list-length-n-p'.~%►►►")

(fundoc 'sequence-type
 "Return the symbol representing the type of the sequence SEQ.~%~@
SEQ should have one of the supertypes string, vector, or list.~%~@
Signal a condition of type `mon:case-error'.~%~@
Return value is one of \(string, vector, or list\).~%~@
:EXAMPLE~%
 \(sequence-type \"string\"\)~%
 \(sequence-type \(make-array 6
                            :element-type 'character 
                            :initial-contents \"string\"\)\)
 \(sequence-type \(make-array 6
                            :element-type 'character
                            :initial-contents \"string\"
                            :adjustable t\)\)~%
 \(sequence-type nil\)~%
 \(sequence-type '\(l i s t\)\)~%
 \(sequence-type '\(consed . pair\)\)~%
 \(sequence-type #*0101\)~%
 \(sequence-type \(make-array 6
                            :element-type 'bit
                            :fill-pointer t\)\)~%
 \(sequence-type #\(v e c t o r\)\)~%
 \(sequence-type 'symbol-not-in-error t\)~%
 \(sequence-type 'symbol-in-error\)~@
:SEE-ALSO `mon:sequencep'.~%►►►")

(fundoc 'sequencep
  "Return t if OBJECT is a sequence \(list or vector\).~%~@
:EXAMPLE~%
 \(sequencep '\(l i s t\)\)~%
 \(sequencep #\(v e c t o r\)\)~%
 \(sequencep \"string\"\)~%~@
:NOTE ANSI Glossary defines the following terms with regards to sequences:~%
,----
| sequence  n. 
| 1. An ordered collection of elements.
| 2. a vector or a list.
|
| proper sequence
| n. A sequence which is not an improper list; that is, a vector or a proper list.
|
| sequence function
| n. One of the following standardized sequence functions:
|
|  `cl:concatenate'         `cl:length'               `cl:remove'
|  `cl:copy-seq'            `cl:map'                  `cl:remove-duplicates'
|  `cl:count'               `cl:map-into'             `cl:remove-if'
|  `cl:count-if'            `cl:merge'                `cl:remove-if-not'
|  `cl:count-if-not'        `cl:mismatch'             `cl:replace'
|  `cl:delete'              `cl:notany'               `cl:reverse'
|  `cl:delete-duplicates'   `cl:notevery'             `cl:search'
|  `cl:delete-if'           `cl:nreverse'             `cl:some'
|  `cl:delete-if-not'       `cl:nsubstitute'          `cl:sort'
|  `cl:elt'                 `cl:nsubstitute-if'       `cl:stable-sort'
|  `cl:every'               `cl:nsubstitute-if-not'   `cl:subseq'
|  `cl:fill'                `cl:position'             `cl:substitute'
|  `cl:find'                `cl:position-if'          `cl:substitute-if'
|  `cl:find-if'             `cl:position-if-not'      `cl:substitute-if-not'
|  `cl:find-if-not'         `cl:reduce'
| 
| Or, an implementation-defined function that operates on one or more
| sequences.  and that is defined by the implementation to be a
| sequence function.
`----~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:sequence-type'.~%►►►")

(fundoc 'booleanp
"Whether OBJ is of type `cl:boolean'.~%~@
Return two values as if by `cl:values'.~%~@
- The first return value is t if OBJ is a boolean else nil.
- The second value is OBJ.~%~@
Values returned will have one of the following forms:~%
 ;=> T, T~%
 ;=> T, NIL~%
 ;=> NIL, <OBJ>~%~@
When optional arg AS-LIST is non-nil return three values as if by `cl:values'.~%~@
- The first value is as a two elt list:~%
   its car is OBJ; 
   its cdr is t if OBJ is a boolean else nil;~%~@
- The second value value is the `cl:type-of' OBJ.~%~@
- The third value is OBJ.~%~@
Values returned will have one of the following forms:~%
 ;=> (T T), BOOLEAN, T~%
 ;=> (NIL T), NULL, NIL~%
 ;=> (NIL NIL), <TYPE>, <OBJ>~%~@
:EXAMPLE~%
 \(booleanp t\)
 \(booleanp 't\)
 \(booleanp t t\)
 \(multiple-value-list \(booleanp t\)\)
 \(multiple-value-list \(booleanp t t\)\)
 \(booleanp nil\)
 \(booleanp nil t\)
 \(booleanp 'nil t\)
 \(booleanp \(\) t\)
 \(booleanp '\(\) t\)
 \(multiple-value-list \(booleanp nil\)\)
 \(multiple-value-list \(booleanp nil t\)\)
 \(booleanp 8\)
 \(booleanp 8 t\)
 \(multiple-value-list \(booleanp 8 t\)\)~%~@
:SEE-ALSO `mon:type-specifier-p', `cl:null', `cl:multiple-value-list'.~%►►►")

(fundoc 'each-a-string-p
"Whether each element of STRING-LIST is `stringp'.~%~@
STRING-LIST is a proper list.
:EXAMPLE~%
 \(each-a-string-p '\(\"a\" \"b\" \"c\"\)\)~%
 \(each-a-string-p '\(\"a\" \"b\" #\(#\\c\)\)\)~%
 \(each-a-string-p '\(\"a\" . \"b\"\)\)~%
:SEE-ALSO `mon:each-a-simple-string', `each-a-string-of-length-1-p',
`mon:each-a-string-or-vector-in-vector'.~%►►►")

(fundoc 'each-a-simple-string-p
"Return non-nil when each elt in STRING-LIST is `simple-string-p'.~%~@
STRING-LIST is a proper-list of type `proper-list-not-null'.~%~@
:EXAMPLE~%~@
 \(each-a-simple-string-p '\(\"a\" \"b\" \"c\" \"d\"\)\)~%
 \(each-a-simple-string-p '\(\"a\" . \"b\"\)\)~%
 \(each-a-simple-string-p t\)~%
 \(each-a-simple-string-p \(\)\)~%
 \(each-a-simple-string-p nil\)~%
 \(each-a-simple-string-p \"a string\"\)~%~@
:SEE-ALSO `mon:each-a-simple-string', `each-a-string-p',
`each-a-string-of-length-1-p', `mon:each-a-string-or-vector-in-vector'.~%►►►")

(fundoc 'each-a-string-of-length-1-p
	"Whether each element of STRING-LIST is a non-empty string of length 1.~%~@
Return non-nil when STRING-LIST is a proper list and each element satisfies `mon:string-of-length-1-p'.~%~@
:EXAMPLE~%
 \(each-a-string-of-length-1-p '\(\"a\" \"b\" \"c\" \"d\"\)\)~%
 \(each-a-string-of-length-1-p '\(\"a\" \"b\" \"c\" 100\)\)~%
 \(each-a-string-of-length-1-p '\(\"a\" \"b\" \"c\" #\\d\)\)~%
 \(each-a-string-of-length-1-p '\(\"a\" \"b\" \"c\" nil \"d\"\)\)~%
 \(each-a-string-of-length-1-p nil\)~%~@
:SEE-ALSO `mon:each-a-simple-string-of-length-1-p',
`mon:each-a-simple-string-p', `mon:each-a-simple-string-p',
`mon:each-a-string-or-vector-in-vector'.~%►►►")

(fundoc 'each-a-simple-string-of-length-1-p
	"Whether each element of STRING-LIST is a non-empty simple-string of length 1.~%~@
Return non-nil when STRING-LIST is a proper list and each element satisfies `mon:simple-string-of-length-1-p'.~%~@
:EXAMPLE~%
 \(each-a-simple-string-of-length-1-p '\(\"a\" \"b\" \"c\" \"d\"\)\)~%
 \(each-a-simple-string-of-length-1-p '\(\"a\" \"b\" \"c\" 100\)\)~%
 \(each-a-simple-string-of-length-1-p '\(\"a\" \"b\" \"c\" #\\d\)\)~%
 \(each-a-simple-string-of-length-1-p '\(\"a\" \"b\" \"c\" nil \"d\"\)\)~%
 \(each-a-simple-string-of-length-1-p nil\)~%~@
:SEE-ALSO `mon:each-a-string-of-length-1-p', `mon:each-a-simple-string-p',
`mon:each-a-simple-string-p', `mon:each-a-string-or-vector-in-vector'.~%►►►")

(fundoc 'each-a-string-or-vector-in-vector
        "Whether VECTOR is `cl:vectorp' with every element either `stringp' or a vector
with each element of type `cl:chararacter'~%~@
An object that is `cl:stringp' does not satisfy the test.~%~@
:EXAMPLE~%~@
 \(each-a-string-or-vector-in-vector #\(\"abc\" \"abc\" \"abc\"\)\)~%
 \(each-a-string-or-vector-in-vector \(make-array 3 :initial-element \"abc\"\)\)~%
 \(each-a-string-or-vector-in-vector \(make-array 3 :initial-element \"abc\" :adjustable t\)\)~%
 \(each-a-string-or-vector-in-vector \(make-array 3 :initial-element \"abc\" :fill-pointer 0\)\)~%~@
:SEE-ALSO `mon:each-a-string-p'.~%►►►")

(fundoc '%byte-vector-each-an-unsigned-byte-8
"Whether each element of the simple-vector BYTE-ARRAY is of type 'unsigned-byte-8.~%~@
Helper function for the type-definition of `mon::%byte-vector'
:EXAMPLE~%
 \(%byte-vector-each-an-unsigned-byte-8 #\(255 255 255 255\)\)~%~@
;; Following fails successfully:~%~@
 \(%byte-vector-each-an-unsigned-byte-8 #\(255 255 255 256\)\)~%~@
:SEE-ALSO `mon:byte-array'.~%►►►")

(fundoc  'string-or-null-p 
  "Return non-nil if object is `cl:stringp' or null.~%~@
:EXAMPLE~%
 \(simple-string-or-null-p nil\)~%
 \(string-or-null-p \"bubba\"\)~%
 \(string-or-null-p 'bubba\)~%~@
:SEE-ALSO `mon:string-or-null', `mon:simple-string-or-null',
`mon:simple-string-or-null-p', `mon:string-null-or-empty-p'.~%►►►")

(fundoc  'simple-string-or-null-p 
  "Return non-nil if object is null or `cl:simple-string-p'.~%~@
:EXAMPLE~%
 \(simple-string-or-null-p nil\)~%
 \(simple-string-or-null-p \"bubba\"\)~%
 \(simple-string-or-null-p 'bubba\)~%~@
:SEE-ALSO `mon:string-or-null', `mon:simple-string-or-null',
`mon:simple-string-or-null-p', `mon:string-null-or-empty-p'.~%►►►")

(fundoc 'string-not-null-p
"Whether STR is of type `mon:string-not-null'~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'simple-string-not-null-p
"Whether STR is of type `mon:simple-string-not-null'~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'string-empty-p
"Whether  STR is `cl:stringp' with length `cl:zerop'~%~@
:EXAMPLE~%
 \(string-empty-p \"\"\)~%
 \(string-empty-p \"m\"\)~%
 \(string-empty-p 'bubba\)~%~@
:SEE-ALSO `mon:simple-string-empty-p'.~%►►►")

(fundoc 'string-not-empty-p
"Whether string is stringp and greater than length 0.
:EXAMPLE~%~@
 \(string-not-empty-p \"\"\)~%
 \(string-not-empty-p \"B\"\)~%
 \(string-not-empty-p \" \"\)~%
 \(string-not-empty-p nil\)~%~@
:NOTE This is effectively:~%
 \(funcall \(complement #'mon:string-empty-p\) <STRING>\)~%~@
:SEE-ALSO `mon:string-empty-p'.~%►►►")

(fundoc 'simple-string-empty-p
"Whether STR is `mon:simple-string-null-p' with length `cl:zerop'~%~@
:EXAMPLE~%
 \(string-empty-p \"\"\)~%
 \(string-empty-p \"m\"\)~%
 \(string-empty-p 'bubba\)~%~@
:SEE-ALSO `mon:string-empty-p'.~%►►►")

(fundoc 'string-null-or-empty-p
  "Whether STR is `mon:string-or-null-p' with length `cl:zerop'.~%~@
:EXAMPLE~%
 \(string-null-or-empty-p nil\)
 \(string-null-or-empty-p \"\"\)
 \(string-null-or-empty-p \"not empty\"\)
 \(string-null-or-empty-p 0\)
 \(string-null-or-empty-p 'bubba\)~%~@
:SEE-ALSO `mon:simple-string-null-or-empty-p', `mon:string-or-null-p',
`mon:simple-string-or-null-p', `mon:string-or-null',
`mon:simple-string-or-null'.~%►►►")

(fundoc 'simple-string-null-or-empty-p
"Whether STR is `simple-string-or-null-p' with length not `cl:zerop'~%~@
:EXAMPLE~%
 \(simple-string-null-or-empty-p nil\)
 \(simple-string-null-or-empty-p \"\"\)
 \(simple-string-null-or-empty-p \"not empty\"\)
 \(simple-string-null-or-empty-p 0\)
 \(simple-string-null-or-empty-p 'bubba\)~%~@
:SEE-ALSO `mon:simple-string-null-or-empty-p', `mon:string-or-null-p',
`mon:simple-string-or-null-p', `mon:string-or-null',
`mon:simple-string-or-null', `mon:string-null-or-empty-p',
`mon:simple-string-not-null-or-empty-p', `mon:string-not-null-or-empty-p',
`mon:simple-string-not-null-or-empty-p'.~%►►►")

(fundoc 'simple-string-not-null-or-empty-p
"Whether STR is of type `mon:simple-string-not-null-or-empty'.~%~@
:EXAMPLE~%
 \(simple-string-not-null-or-empty-p \"m\"\)
 \(simple-string-not-null-or-empty-p nil\)
 \(simple-string-not-null-or-empty-p \"\"\)
 \(simple-string-not-null-or-empty-p 42\)~%~@
:SEE-ALSO `mon:simple-string-null-or-empty-p', `mon:string-or-null-p',
`mon:simple-string-or-null-p', `mon:string-or-null',
`mon:simple-string-or-null', `mon:string-null-or-empty-p',
`mon:simple-string-not-null-or-empty-p', `mon:string-not-null-or-empty-p',
`mon:simple-string-not-null-or-empty-p'.~%►►►")

(fundoc 'string-not-null-or-empty-p
"Whether STR is of type `mon:string-not-null-or-empty'.~%~@
:EXAMPLE~%~@
 \(string-not-null-or-empty-p \"m\"\)
 \(string-not-null-or-empty-p nil\)
 \(string-not-null-or-empty-p \"\"\)
 \(string-not-null-or-empty-p 42\)~%~@
:SEE-ALSO `mon:simple-string-null-or-empty-p', `mon:string-or-null-p',
`mon:simple-string-or-null-p', `mon:string-or-null',
`mon:simple-string-or-null', `mon:string-null-or-empty-p',
`mon:simple-string-not-null-or-empty-p', `mon:string-not-null-or-empty-p',
`mon:simple-string-not-null-or-empty-p'.~%►►►")

(fundoc 'simple-string-of-length-1-p
"Whether STRING is of type `mon:simple-string-of-length-1'.~%~@
:EXAMPLE~%
 \(simple-string-of-length-1-p \"a\"\)~%
 \(simple-string-of-length-1-p \(make-array 1 :element-type 'character :initial-element #\\a\)\)~%
 \(simple-string-of-length-1-p \(make-array 1 :element-type 'character :initial-element #\\a :adjustable t :fill-pointer 0\)\)~%~@
;; Following fail successfully:~%
 \(simple-string-of-length-1-p \"\"\)~%
 \(simple-string-of-length-1-p  \"ab\"\)~%
 \(simple-string-of-length-1-p 42\)~%~@
:SEE-ALSO `mon:string-of-length-1' `mon:string-of-length-1-p',
`mon:each-a-string-of-length-1', `mon:each-a-string-of-length-1-p'.~%►►►")

(fundoc 'string-of-length-1-p
"Whether STRING is of type `mon:string-of-length-1'.~%~@
Unlike objects of type `mon:simple-string-of-length-1' an object of this type
includes adjustable arrays and those with fill-pointers.~%~@
:EXAMPLE~%
 \(string-of-length-1-p \"a\"\)~%
 \(string-of-length-1-p \(make-array 1 :element-type 'character :initial-element #\\a\)\)~%
 \(string-of-length-1-p \(make-array 1 :element-type 'character :initial-element #\\a :adjustable t :fill-pointer 0\)\)~%~@
;; Following fail successfully:~%
 \(string-of-length-1-p \"\"\)~%
 \(string-of-length-1-p  \"ab\"\)~%
 \string-of-length-1-p( 42\)~%
:SEE-ALSO `mon:simple-string-of-length-1' `mon:simple-string-of-length-1-p',
`mon:each-a-string-of-length-1', `mon:each-a-string-of-length-1-p'.~%►►►")

(fundoc 'vector-with-fill-pointer-p
"Whether OBJECT is satisfies both `cl:vectorp' and `cl:array-has-fill-pointer-p'.~%~@
:EXAMPLE~%
 \(vector-with-fill-pointer-p \"string\"\)~%
 \(vector-with-fill-pointer-p \(make-array 6 
                                        :element-type 'base-char
                                        :initial-contents \"string\" 
                                        :fill-pointer 6\)\)~%~@
:SEE-ALSO `mon:string-with-fill-pointer', `mon:string-with-fill-pointer-p'.~%►►►")

(fundoc 'string-with-fill-pointer-p
"Whether PUTATIVE-STRING-WITH-FILL-POINTER is of type `mon:string-with-fill-pointer'.~%~@
:EXAMPLE~%
 \(string-with-fill-pointer-p \"I got no fill boss\"\)~%
 \(let \(\(has-fp \(make-array 11
                           :element-type 'character 
                           :initial-contents \"I got fill!\"
                           :fill-pointer 11\)\)\)
   \(format has-fp \" Cuz, is a string-with-fill-pointer-p: ~~S\" 
           \(string-with-fill-pointer-p has-fp\)\)
   has-fp\) ~%~@
:NOTE The purpose of this prediciate is b/c the DESTINATION argument to
`cl:format's may be a string so long as that string has a fill-pointer, e.g.:~%
 ,----
 | `cl:format' sends the output to DESTINATION.
 | 
 |  - If DESTINATION is NIL, `cl:format' creates and returns a string
 |    containing the output from CONTROL-STRING.
 | 
 |  - If DESTINATION is non-nil, it must be a string with a fill
 |    pointer, a stream, or the symbol T.
 | 
 |  - If DESTINATION is a string with a fill pointer, the output is
 |    added to the end of the string.
 | 
 |  - If DESTINATION is a stream, the output is sent to that stream.
 | 
 |  - If DESTINATION is T, the output is sent to standard output.
 `----
:SEE \(info \"\(ansicl\)Formatted Output\"\)~%~@
:NOTE However, there is the proviso in \"exceptional situations\" of the spec:~%
 ,----
 | If DESTINATION is a string with a fill pointer, the consequences are
 | undefined if destructive modifications are performed directly on the
 | string during the dynamic extent of the call.
 `---- 
:SEE \(info \"\(ansicl\)format\"\)~%~@
:SEE-ALSO `mon:vector-with-fill-pointer-p', `mon:stream-or-boolean',
`mon:open-stream-output-stream-p', `cl:array-has-fill-pointer-p',
`cl:vectorp'.~%►►►")

(fundoc 'string-all-whitespace-p
"Whether each character in STRING is `mon:whitespace-char-p'.~%~@
Return value is as if by `cl:values'.~%~@
First value is boolean.~%~@
When first value is T, second value is the length of string
When first value is NIL, second value is the position in STRING of the char of
type `mon:whitespace-char' third value is the length of string.~%~@
:EXAMPLE~%
 \(string-all-whitespace-p \(format nil \"~~{~~C~~}\" *whitespace-chars*\)\)~%
 \(string-all-whitespace-p \" \"\)~%
 \(string-all-whitespace-p \"\"\)~%
 \(string-all-whitespace-p \"     a\"\)~%~@
:SEE-ALSO `mon:string-contains-whitespace-p', `mon:string-no-whitespace-p',
`mon:string-trim-whitespace', `mon:*whitespace-chars*'.~%►►►")

(fundoc 'string-null-empty-or-all-whitespace-p
        "Whether MAYBE-EMPTY-STRING is null, `mon:string-empty-p',
`mon:string-all-whitespace-p'.~%~@
MAYBE-EMPTY-STRING is any type of object.~%~@
:EXAMPLE~%
 \(string-null-empty-or-all-whitespace-p  nil\)~%
 \(string-null-empty-or-all-whitespace-p  \"\"\)~%
 \(string-null-empty-or-all-whitespace-p  \(format nil \"~~8T \"\)\)~%
 \(string-null-empty-or-all-whitespace-p  42\)~%
 \(string-null-empty-or-all-whitespace-p  '\(a\)\)~%~@
:SEE-ALSO `string-not-null-empty-or-all-whitespace-p',
`string-contains-whitespace-p', `string-no-whitespace-p'.~%►►►")

(fundoc 'string-not-null-empty-or-all-whitespace-p
"Whether MAYBE-GOOD-STRING is of type `string-not-null-empty-or-all-whitespace'.~%~@
:EXAMPLE~%
 \(string-not-null-empty-or-all-whitespace-p \"A fine string\"\)~%
 \(string-not-null-empty-or-all-whitespace-p nil\)~%
 \(string-not-null-empty-or-all-whitespace-p \"\"\)~%
 \(string-not-null-empty-or-all-whitespace-p \(format nil \"~~8T \"\)\)~%
 \(string-not-null-empty-or-all-whitespace-p 42\)~%
 \(string-not-null-empty-or-all-whitespace-p '\(a\)\)~%~@
:SEE-ALSO `string-null-empty-or-all-whitespace-p',
`string-contains-whitespace-p', `string-no-whitespace-p'.~%►►►")

(fundoc 'string-not-empty-or-all-whitespace-p
        "Whether MAYBE-GOOD-STRING is a non-empty string without whitespace characters.~%~@
Return non-nil when the first two predicates are true and third is false:~%
 `cl:stringp' `mon:string-not-empty-p' `mon:string-all-whitespace-p'~%~@
:EXAMPLE~%
 \(string-not-empty-or-all-whitespace-p \"\"\)~%
 \(string-not-empty-or-all-whitespace-p 
  \(format nil \"~~{~~C~~}\" mon:*whitespace-chars*\)\)~%
 (string-not-empty-or-all-whitespace-p 
  (format nil \"~~{~~C~~}good-string\" mon:*whitespace-chars*))~%
 \(string-not-empty-or-all-whitespace-p nil\)~%
 \(string-not-empty-or-all-whitespace-p 42\)~%~@
:SEE-ALSO `mon:string-contains-whitespace-p', `mon:string-no-whitespace-p',
`mon:*whitespace-chars*'.~%►►►")

(fundoc 'string-contains-whitespace-p 
"Whether there is a char in STRING which is `mon:whitespace-char-p'.
Return value is as if by `cl:values'.~%~@
First value is boolean.~%~@
When first value is T, second value is the position in STRING of the char of
type `mon:whitespace-char' third value is the length of string.~%~@
When first value is NIL, second value is the length of string.~%
:EXAMPLE~%
 \(string-contains-whitespace-p \" \"\)~%
 \(string-contains-whitespace-p \"abc \"\)~%
 \(string-contains-whitespace-p \"ab c\"\)~%
 \(string-contains-whitespace-p \"\"\)~%
 \(string-contains-whitespace-p \"abc\"\)~%~@
:SEE-ALSO `mon:string-all-whitespace-p', `mon:string-no-whitespace-p',
`mon:string-all-whitespace-p', `mon:string-trim-whitespace',
`mon:*whitespace-chars*'.~%►►►")

(fundoc 'string-no-whitespace-p
"Whether no chars in STRING are `mon:whitespace-char-p'.~%~@
Return value is as if by `cl:values'.~%~@
First value is boolean.
When first value is T, second value is the length of string.~%
When first value is NIL, second value is the position in STRING of the char of
type `mon:whitespace-char' third value is the length of string.~%~@
:EXAMPLE~%
 \(string-no-whitespace-p \"\"\)~%
 \(string-no-whitespace-p \"abcdefghi\"\)~%
 \(string-no-whitespace-p \"abcde fgh\"\)~%
 \(string-no-whitespace-p \"abcdefgh \"\)~%
 \(string-no-whitespace-p \" abcdefgh\"\)~%
 \(string-no-whitespace-p \" \"\)~%
:SEE-ALSO `mon:string-no-whitespace-p', `mon:string-all-whitespace-p',
`mon:string-contains-whitespace-p', `mon:string-trim-whitespace',
`mon:*whitespace-chars*'.~%►►►")

(fundoc 'string-all-hex-char-p
"Whether every character in MAYBE-HEX-STRING satsifies `mon:hexadecimal-char-p'.~%~@
:EXAMPLE~%
 \(string-all-hex-char-p \"6ba7b8109dad11d180b400c04fd430c8\"\)~%
 \(string-all-hex-char-p \"6BA7B8109DAD11D180B400C04FD430C8\"~%
 \(string-all-hex-char-p \"-6ba7b8109dad11d180b400c04fd430c8\"\)~%~@
:SEE-ALSO `mon:*hexadecimal-chars*'.~%►►►")

(fundoc 'base-char-p
 "Return non-nil if character CHR is of type `cl:base-char'.~%~@
:EXAMPLE~%
 \(base-char-p \(char \"abcd\" 0\)\)~%~@
:SEE-ALSO `cl:char', `cl:standard-char-p', `cl:characterp', `cl:character',
`cl:standard-char', `cl:char-int'.~%►►►")

(fundoc 'char-code-integer-p
"Whether CHAR-CODE-INT is of type `mon:char-code-integer'.~%~@
:EXAMPLE~%
 \(char-code-integer-p 1114111\)~%
 \(char-code-integer-p 1114112\)~%~@
:SEE-ALSO `mon:each-a-char-code-integer-p', `cl:char-int'.~%►►►")

(fundoc 'digit-char-0-or-1-p
"Whether CHAR-1OR0 is `cl:digit-char-p' and either #\\0 or #\\1.~%~@
CHAR-1OR0 should be of type `mon:standard-char-or-null', signal an error if not.~%~@
:EXAMPLE~%
 \(digit-char-0-or-1-p #\\1\)~%
 \(digit-char-0-or-1-p #\\0\)~%
 \(digit-char-0-or-1-p nil\)~%
 \(digit-char-0-or-1-p #\\2\)~%
 \(let \(\(not-1or0 \"00001b\"\)\)
   \(position-if-not #'digit-char-0-or-1-p not-1or0\)\)~%~@
:SEE-ALSO `mon:string-all-digit-char-0-or-1-p'.~%►►►")

(fundoc 'each-a-character-p
 "Whether each element in CHAR-LIST is `cl:characterp'.~%~@
CHAR-LIST should satisfy `mon:list-proper-not-null-p'.~%
:EXAMPLE~%
 \(each-a-character-p \(list #\\a #\\b #\\c #\\d\)\)~%
 \(each-a-character-p \(list #\\a #\\b #\\c 100\)\)~%
 \(each-a-character-p #\\a\)~%
 \(each-a-character-p nil\)~%
:SEE-ALSO `mon:digit-char-0-or-1-p', `mon:base-char-p',
`mon:string-all-digit-char-0-or-1-p'.~%►►►")

(fundoc 'each-a-char-code-integer-p
"Whether each element of CHAR-CODE-INTEGER-LIST is `mon:char-code-integer-p'.~%~@
:EXAMPLE~%~@
 \(each-a-char-code-integer-p \(loop for ccil from 9658 to 9700 collect ccil\)\)
 \(each-a-char-code-integer-p 111411\)~%~@
:SEE-ALSO `mon:each-a-character-or-char-code-integer-p', `cl:char-int'.~%►►►")

(fundoc 'each-a-character-or-char-code-integer-p
"Whether each element of CHAR-OR-CHAR-CODE-LIST is `characterp'
 or `char-code-integer-p'.~%~@
CHAR-OR-CHAR-CODE-LIST should satisfy `mon:list-proper-not-null-p'.'
:EXAMPLE~%
 \(each-a-character-or-char-code-integer-p '\(#\\a 98 #\\c 9658\)\)~%
 \(each-a-character-or-char-code-integer-p nil\)~%
 \(each-a-character-or-char-code-integer-p #\\a\)~%
 \(each-a-character-or-char-code-integer-p \"abc\"\)~%~@
:SEE-ALSO `mon:each-a-char-code-integer-p', `cl:char-int'.~%►►►")

(fundoc 'each-a-sequence-p
        "Whether each elt of SEQ is `cl:sequencep'.~%~@
Return a boolean.~%~@
When SEQ is `cl:null', `cl:stringp', `cl:bit-vector-p', `arrayp', `listp,  return multiple values:~%~@
 NIL, { string | bit-vector }~%
 T,  { null | simple-array | array | list }~%~@
:EXAMPLE~%
 \(each-a-sequence-p nil\)~%
 \(each-a-sequence-p 42\)~%
 \(each-a-sequence-p '\(\"a\"  #\(b\) '\(\) \(b\) nil\)\)~%
 \(each-a-sequence-p '\(\"a\" #\(b\) \(b\) \(a . b\)  \(a b . c\)\)\)~%~@
 \(each-a-sequence #*00000\)~%
 \(each-a-sequence \"string\"\)~%
 \(each-a-sequence #\(\(a\) \(b\) \(c\)\)\)~%
 \(each-a-sequence \(make-array 3 
                              :fill-pointer 3 
                              :adjustable t 
                              :initial-contents '\(\(a\) \(b\) \(c\)\)\)\)~%~@
:SEE-ALSO `mon:each-a-sequence-proper-p',
`mon:each-a-sequence-proper-or-character-p', `mon:each-a-sequence',
`mon:each-a-sequence-proper', `mon:each-a-sequence-proper-or-character'.~%►►►")

 

(fundoc 'each-a-sequence-proper-p
        "Whether each elt of SEQ is a proper sequence.~%~@
Return a boolean.~%
When SEQ is `cl:null', `cl:stringp', `cl:bit-vector-p', `arrayp', `listp,  return multiple values:~%~@
 NIL, { string | bit-vector }~%
 T,  { null | simple-array | array | list }~%~@
:EXAMPLE~%
 \(each-a-sequence-proper-p nil\)~%
 \(each-a-sequence-proper-p 42\)~%
 \(each-a-sequence-proper-p '\(\"a\"  #\(b\) '\(\) \(b\) nil\)\)~%
 \(each-a-sequence-proper-p '\(\"a\" #\(b\) \(b\) \(a . b\)  \(a b . c\)\)\)~%~@
 \(each-a-sequence-proper-p #*00000\)~%
 \(each-a-sequence-proper-p \"string\"\)~%~@
 \(each-a-sequence-proper-p #\(\(a\) \(b\) \(c\)\)\)
 \(each-a-sequence-proper-p \(make-array 3 
                                       :fill-pointer 3 
                                       :adjustable t 
                                       :initial-contents '\(\(a\) \(b\) \(c\)\)\)\)~%~@
:SEE-ALSO `mon:each-a-sequence-p', `mon:each-a-sequence-proper-or-character-p',
`mon:each-a-sequence', `mon:each-a-sequence-proper',
`mon:each-a-sequence-proper-or-character'.~%►►►")

(fundoc 'each-a-sequence-proper-or-character-p
        "Whether each elt of SEQ is either a proper sequence or `cl:characterp'.~%~@
:EXAMPLE~%
 \(each-a-sequence-proper-or-character-p '\(\"a\" #\\a #\(b\) '\(\) \(b\) nil\)\)~%
 \(each-a-sequence-proper-or-character-p '\(\"a\" #\\b #\\c #\(#\\d #\\e\) #\\f\)\)~%
 \(each-a-sequence-proper-or-character-p '\(\"a\" #\\a #\(b\) \(b\) \(a . b\)  \(a b . c\)\)\)~%~@
:SEE-ALSO `mon:each-a-sequence-p', `mon:each-a-sequence-proper-p',
`mon:each-a-sequence', `mon:each-a-sequence-proper',
`mon:each-a-sequence-proper-or-character'.~%►►►")

(fundoc 'each-a-string-or-null-p
        "Whether each elt of STRING-LIST `mon:string-or-null-p'.~%~@
STRING-LIST is a proper-list.~%~@
:EXAMPLE~%
 \(each-a-string-or-null-p '\(\"a\" \"b\" \"c\"\)\)~%
 \(each-a-string-or-null-p '\(nil nil nil\)\)~%
 \(each-a-string-or-null-p '\(\"a\" nil \"b\" nil\)\)~%
 \(each-a-string-or-null-p nil\)~%
 \(each-a-string-or-null-p 8\)~%
 \(each-a-string-or-null-p '\(\"a\" . \"b\"\)\)~%
 \(each-a-string-or-null-p '\(\"a\" \"b\" . \"c\"\)\)~%~@
:SEE-ALSO `mon:each-a-string-or-null-p', `mon:each-a-string',
`mon:each-a-simple-string', `mon:each-a-string-of-length-1',
`mon:each-a-sequence', `mon:each-a-sequence-proper',
`mon:each-a-sequence-proper-or-character', `mon:each-a-simple-string-p',
`mon:each-a-string-of-length-1-p'.~%►►►")

;; `cl:char-int'
(fundoc 'string-all-digit-char-0-or-1-p
"Return non-nil if every character in 1-AND-0-STRING is `mon:digit-char-0-or-1-p'.~%~@
1-AND-0-STRING should be of type `mon:standard-char-or-null'.
Signal an error if not.
:EXAMPLE~%~@
 \(string-all-digit-char-0-or-1-p \"000001\"\)
 \(string-all-digit-char-0-or-1-p  nil\)
 \(string-all-digit-char-0-or-1-p  \"\"\)
 \(string-all-digit-char-0-or-1-p \"000001e\"\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'fixnump
"Whether FIXNUM-MAYBE is of type `cl:fixnum'~%~@
:EXAMPLE~%~@
 \(fixnump most-negative-fixnum\)~%
 \(fixnump most-positive-fixnum\)~%
 \(fixnump \(1+ most-positive-fixnum\)\)~%
 \(fixnump \(1- most-negative-fixnum\)\)~%~@
:NOTE ANSI spec says that a fixnum is an integer whose value is between
`cl:most-negative-fixnum' and `cl:most-positive-fixnum' inclusive.~%
While exactly which integers constitutue a fixnum is implementation-defined the
spec says that the `cl:fixnum' type is required to be a supertype of
\(signed-byte 16\).\
:SEE-ALSO `mon:bignump', `cl:bignum', `cl:integer', `cl:rational', `cl:real',
`cl:number', `cl:rationalp', `cl:realp', `cl:numberp'.~%►►►")

(fundoc 'standard-test-function-p
 "Return non-nil when TEST-FUN is one of the standard equality test-funcions.~%~@
These include:~% 
 `eq', `eql', `equal', `equalp'~%~@
:EXAMPLE~%
 \(standard-test-function-p 'eq\)~%
 \(standard-test-function-p 'string-equal\)~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'logical-pathname-p
"Whether object is of type: `mon:logical-pathname-designator'.~%~@
:EXAMPLE~%
 \(logical-pathname-p #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\)~%
;; Following fail successfully:~%
 \(logical-pathname-p \"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\)~%
 \(logical-pathname-p \(translate-logical-pathname #P\"SYS:SRC;CODE;TARGET-PATHNAME.LISP\"\)\)~%
:SEE-ALSO `mon:filename-designator-p' `mon:pathname-designator-p',
`mon:pathname-or-namestring-p', `mon:logical-pathname-designator',
`mon:pathname-designator', `mon:filename-designator',
`mon:pathname-or-namestring'.~%►►►")

(fundoc 'filename-designator-p
"Whether object is of type: `mon:filename-designator'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:logical-pathname-p', `mon:pathname-designator-p'
`mon:pathname-or-namestring-p', `mon:logical-pathname-designator',
`mon:pathname-designator', `mon:pathname-or-namestring'.~%►►►")

(fundoc 'pathname-designator-p
"Whether object is of type: `mon:pathname-designator'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:logical-pathname-p' `mon:filename-designator-p'
`mon:pathname-or-namestring-p', `mon:logical-pathname-designator',
`mon:filename-designator', `mon:pathname-or-namestring'.~%►►►")

(fundoc 'pathname-or-namestring-p
"Whether object is of type: `mon:pathname-or-namestring'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:logical-pathname-p' `mon:filename-designator-p'
`mon:pathname-designator-p',
`mon:logical-pathname-designator', `mon:pathname-designator',
`mon:filename-designator'.~%►►►")

#+sbcl
(fundoc 'singleton-p
"Is LST a list containing one elt.~%~@
:EXAMPLE~%
 \(singleton-p '(a))~%~@
:SEE-ALSO `list-proper-p', `cl:atom' ,`cl:null', `cl:consp'.~%►►►")

#+sbcl 
(fundoc 'closure-p
"Return non-nil if OBJECT is a closure.~%~@
:EXAMPLE~%
 \(prog2 
     \(progn \(defparameter *my-clsr* nil\)
	    \(defun mk-clsr \(my-clsr\) \(lambda \(\) \(incf my-clsr\)\)\)
	    \(setf *my-clsr* \(mk-clsr 10\)\)\)
     \(closure-p *my-clsr*\)
   \(unintern '*my-clsr*\)\)~%~@
:SEE-ALSO `closure-obj', `sb-impl::closurep', `functionp',
`standard-test-function-p', `symbol-function', `fdefinition'.~%►►►")

#+sbcl 
(fundoc 'variable-special-p
        "Return non-nil if the symbol names a global special variable.~%~@
:EXAMPLE~%
 \(variable-special-p 'mon:*error-table*\)~%
 \(variable-special-p 'bubba\)~%~@
:SEE-ALSO `mon:boundp', `symbol-value', `mon:bound-and-true-p' 
`sb-walker:var-globally-special-p'.~%►►►")

(fundoc 'declared-special-p
        "Return T if SYMBOL is declared special.~%~@
:EXAMPLE~%
 \(unwind-protect
      \(progn 
        \(defparameter *tt--bubb* nil\)
        \(declared-special-p '*tt--bubb*\)\)
   \(unintern '*tt--bubb*\)\)
 \(find-symbol \"*TT--BUBB*\"\)~%~@
:SEE-ALSO `mon:variable-special-p', `sb-walker:var-globally-special-p',
`sb-int:info'.~%►►►")

#+sbcl
(setf (documentation 'featurep 'function)
      #.(format nil 
 "Returns T when argument FEATURE-EXPRESSION matches state of the `*features*' list.~%~@
If FEATURE-EXPRESSION is a symbol, test whether it is present in `*features*'.~%~@
FEATURE-EXPRESSION may be any atom or list acceptable to the reader macros `#+' and `#-'.~%~@
Handles arbitrary combinations of atoms using `and', `or'.~%~@

;;; If X is a symbol, see whether it is present in *FEATURES*. Also
;;; handle arbitrary combinations of atoms using NOT, AND, OR.
:EXAMPLE~%
 \(featurep :COMMON-LISP\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `sb-int:featurep', `alexandria:featurep'~%►►►"))

#+sbcl
(setf (documentation 'type-specifier-valid-p 'function)
      #.(documentation 'sb-ext:valid-type-specifier-p 'function))

#+sbcl
(setf (documentation 'type-expand-1 'function)
      #.(documentation 'sb-ext:typexpand-1 'function))

#+sbcl
(setf (documentation 'type-expand 'function)
      #.(documentation 'sb-ext:typexpand 'function))

#+sbcl
(setf (documentation 'type-expand-all 'function)
      #.(documentation 'sb-ext:typexpand-all 'function))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
