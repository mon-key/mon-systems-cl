;;; :FILE-CREATED <Timestamp: #{2010-07-06T19:04:44-04:00Z}#{10272} - by MON>
;;; :FILE mon-systems/strings.lisp
;;; ==============================


(in-package #:mon)
;; *package*

(defun string-delimited-to-list (string &optional (separator #\space)
                                 skip-terminal)
  (declare
   (type string string)
   (type character separator)) 
  (do* ((len (length string))
        (output '())
        (pos 0)
        ;; (end (mon::position-char separator string pos len)
        ;;      (mon::position-char separator string pos len)))
        (end (char-position separator string pos len)
             (char-position separator string pos len)))
       ((null end)
        (if (< pos len)
            (push (subseq string pos) output)
            (when (or (not skip-terminal) (zerop len))
              (push "" output)))
        (nreverse output))
    (declare (type fixnum pos len)
             (type (or null fixnum) end))
    (push (subseq string pos end) output)
    (setq pos (1+ end))))

(defun string-from-delimited-list (list &optional (separator " "))
  (format nil (concatenate 'string "~{~A~^" (string separator) "~}") list))

(defun simple-string-ensure (thing)
;; Signals an error if THING cannot be coerced with `cl:string'.
  (let ((s (string thing)))
    (make-array (length s)
                :element-type 'character
                :initial-contents s)))

(defun string-explode (str)
  (declare (type string str))
  (loop 
     :for i :across (the string str)
     :collect (format nil "~a" i)))

;;; :SOURCE clocc/src/onlisp-util.lisp
;; (defun symbol-name-explode-string (explode-sym)
;;   (declare (type symbol explode-sym))
;;   (map 'list #'(lambda (c)
;;                  ;;(intern 
;; 		  (make-string 1 :initial-element c));))
;;        (symbol-name explode-sym)))

(defun symbol-name-explode (explode-sym &optional as-chars)
  (declare (type symbol explode-sym))
  (or 
   (and (booleanp explode-sym) explode-sym)
   (loop
      :with expld-sym = (symbol-name explode-sym)
      :for expld :across (the string expld-sym)
      :if as-chars
       :collect expld
      :else 
       :collect (make-string 1 :initial-element expld))))

;;; :COURTESY Drew Mcdermott's ytools/nilscompat.lisp
(defun string-elt (str idx) 
  ;; (string-elt "bubba" 2)
  (elt (the string str) (the index idx)))

;;; :COURTESY Drew Mcdermott's ytools/base.lisp
(defun string-length (str) 
  (declare (string str))
  (the array-length (length str)))

;;; :COURTESY Drew Mcdermott's ytools/misc.lisp
(defun string-begins (str putative-start)
  (let ((n (mismatch str putative-start)))
    (or (not n)
        (= n (string-length putative-start)))))

;;; :SOURCE xit/cl-utilities/cl-utilities.lisp :WAS `convert-to-string'
;; (defun convert-to-string (value)
;;   (if (stringp value)
;;       value
;;     (let ((new-string (format nil "~A" value)))
;; 	(string-downcase new-string))))


;; :COURTESY Drew Mcdermott ytools/base.lisp :WAS `coerce-to-string'
(defun string-coerce-from (x)
  ;; :WAS (cond ((stringp x) x)
  ;;        ((characterp x) (string x))
  ;;        ((numberp x)
  ;;         (princ-to-string x))
  ;;        ((symbolp x)
  ;;         (symbol-name x))
  ;;        (t (format nil "~a" x)))
  (typecase x 
    (string x)
    (character (string x))
    (number (princ-to-string x))
    (symbol (symbol-name x))
    (t (format nil "~a" x))))

(defun string-to-char (string &key (w-code-char nil))
  (declare (type string string))
  (with-input-from-string  (inpt-str string :start 0 :end 1)
    (if w-code-char
	(read-char inpt-str)
	(char-code (read-char inpt-str)))))

(defun string-first-char (string)
  (declare (type string-or-null string))
  (unless (or (null string)
	      (string-empty-p (the string string)))
    (char string 0)))

(defun string-replace-all (string from-string to-string &key (test #'char=) ) ;(w-case 'preserve)) 
  ;; (declare (optimize (speed 0) (safety 3) (compilation-speed 0) (debug 3)))
  (with-output-to-string (out) 
    (loop 
       :with part-length = (length from-string) 
       :for old-pos = 0 :then (+ pos part-length) 
       :for pos = (search from-string string  :start2 old-pos :test test) 
       :do (write-string  string out :start old-pos :end (or pos (length string))) 
       :when pos 
       :do  ;; (break "POS was ~S" pos)
           (write-string to-string out) :while pos)))

;;; ==============================
;;; :SOURCE s-sql.lisp :WAS `implode'
(defun string-implode (sep list)
  (declare (type string sep))
  (string-cat (loop
		 :for element :on list
		 :collect (car element)
		 :if (cdr element)
		 :collect sep)))

;;; :SOURCE s-sql.lisp :WAS reduce-strings
(defun string-reduce (list)
  (let ((accum ())
        (span ""))
    (dolist (part list)
      (cond ((stringp part) (setf span (concatenate 'string span part)))
            (t (when (not (string= "" span))
                 (push span accum)
                 (setf span ""))
               (push part accum))))
    (if (not (string= "" span))
        (push span accum))
    (nreverse accum)))

;;; :SOURCE texinfo-docstrings/colorize.lisp
;; (defun strcat (&rest strings)
;;   (apply #'concatenate 'string strings))
;;
;;; :SOURCE s-sql.lisp :WAS strcat
(defun string-cat (args)
  (let ((result (make-string (reduce #'+ args :initial-value 0 :key 'length))))
    (loop
       :for pos = 0 :then (+ pos (length arg))
       :for arg :in args
       :do (replace result arg :start1 pos))
    result))
;;;
;;; (string-cat '("a" "b" "c"))
;;; (reduce #'+ '("a" "b" "c") :initial-value 0 :key 'length)

;;; :SOURCE xit/cl-utilities/cl-utilities.lisp :WAS `convert-to-readable-string'
(defun string-convert-to-readable (value)
  (if (stringp value)
      value
      (let ((new-string (format nil "~S" value)))
	(string-downcase new-string))))

;;; :SOURCE quicklisp/ql-util.lisp :WAS `split-spaces'
;;; Modified for #\tab, #\newline, etc.
(defun string-split-spaces (line &optional w-last-wspc)
  ;;(declare (optimize (speed 0) (safety 0) (compilation-speed 0) (debug 3)))
  (declare (type string line))
  (let ((words '())
        (mark  0)
        (pos   0))
    (labels ((finish ()
               (setf pos (length line))
               (save)
               (return-from string-split-spaces 
		 (or (and w-last-wspc
			  (nreverse words))
		     (and words (or (pop words) t)
			  (nreverse words)))))
             (save ()
               (when (< mark pos)
                 (push (subseq line mark pos) words)))
             (mark ()
	       (setf mark pos))
             (in-word (char)
	       (or (and (whitespace-char-p char) 
			(progn 
			  (save) 
			  #'in-space))
		   #'in-word))
             (in-space (char)
	       (or (and (whitespace-char-p char) #'in-space) 
		   (progn 
		   (mark) 
		   #'in-word))))
      (let ((state #'in-word))
        (dotimes (i (length line) (finish))
          (setf pos i)
          (setf state (funcall state (char line i))))))))

;;; ==============================
;;; :NOTE Modified version from c.l.l source below.
;;;  - Changed the multiplier semantics. It doesn't IMO make sense to have
;;;    multipliers arguments occur in other than the car position of ARGS.
;;;    e.g the old semantics:
;;;     (make-string* 2 "foo" " " 2 "foo") ;=> "foo foo foo foo"
;;;    new semanitcs:
;;;     (make-string* 2 " foo " 2 " foo") ;=> " foo 2 foo foo 2 foo"
;;;  - Now accepts characters and numbers (integers, floats, etc.)
;;;  - Elides null elements occuring in other than the car position.
;;;  - Defaults to 1x multiplier if car is null 
;;;  - Bails with return value "" when:
;;;    -- arg is null; 
;;;    -- cdr is null
;;;    -- car of args is 0
;;;    -- args is length 2 and: 
;;;    ---  car is null and cadr is null
;;;    ---  cadr is null or empty string
;;;     
;;; :SOURCE (URL `http://groups.google.com/group/comp.lang.lisp/msg/2f64b48bcb0a0519')
;;; :NEWSGROUP comp.lang.lisp
;;; :FROM Szymon "tichy" delme.ssbm2@o2.pl
;;; :DATE 2006-06-05 23:08:50 +0200
;;; :SUBJECT Re: With format, how to print a variable number of spaces
(defun make-string* (&rest args)
  (or 
   (and (or 
	 ;; (make-string*) ;=> ""
	 (null args)
	 ;; On second thought following form should be valid: 
	 ;;  (make-string* nil "x" #\x 3) ;=> "xx3" 
	 ;; (null (car args))
	 ;;
	 ;; (make-string*) ;=> ""
	 (null (cdr args))
	 ;; Macro/backquote can pass either of the following two forms:
	 ;; (make-string* 8 nil) ;=> ""
	 ;; (make-string* nil nil) ;=> ""
	 ;; Also, check for empty string for good measure.
	 ;; (make-string* 8 "") ;=> ""
	 (and (eql (length args) 2)
	      (or 
	       (string-null-or-empty-p (cadr args)) 
	       (every #'null args)))
	 ;; (make-string* 0 "x" "y" "z") ;=> ""
	 (and (typep (car args) '(integer 0 0))))
	"")
   (let ((num (or (and (typep (car args) '(integer 1))
		       (or 
			;; Its irrelevant how many times we multiply
			(and (null (cdr args)) 
			     (return-from make-string* ""))
			(prog1 (car args)
			  (setf args (cdr args)))))
		  1))
	 (stream (make-string-output-stream)))
     (dolist (thing args)
       (etypecase thing
	 (null nil)
	 ;; :WAS ((integer) (setq num (* thing num)))
	 (number  (write thing :stream stream))
	 (string  (write-string thing stream))
	 ;; :NOTE Reading symbols is prob a bad idea.
	 ;; (symbol (write-string (string thing) stream))
	 (character (write-char thing stream))))
     (let* ((string (get-output-stream-string stream))
	    (result-length (* (length string) num))
	    (result (make-string result-length)))
       (loop 
	  :for index :by (length string) :below result-length
	  :do (replace result string :start1 index))
       result))))

;;; :NOTE Has sb-rt test `string-split-on-chars-TEST'
;; string-split-on-chars
(defun string-split-on-chars (string &optional separators white-on-white)
  (declare (string string)
           (optimize (speed 3)))
  (let ((string     (etypecase string
                      (string-empty (return-from string-split-on-chars string))
                      (simple-string string)
                      (string        (copy-seq string))))
        (separators (etypecase separators
                      (null    (if (and (string-all-whitespace-p (the simple-string string))
                                        (null white-on-white))
                                   (return-from string-split-on-chars string)
                                   (map 'string #'identity *whitespace-chars*)))
                      (mon:string-null-or-empty (return-from string-split-on-chars string))
                      (simple-string  separators)
                      (string         (copy-seq separators))
                      ((or character mon:char-code-integer) (char-to-string separators :allow-integers t))
                      (mon:proper-list
                       (etypecase separators
                         (mon:each-a-string-of-length-1
                          (with-standard-io-syntax (format nil "~{~A~}" separators)))
                         (each-a-character-or-char-code-integer (char-list-to-string separators))))))
        (chunks (make-array 0 :adjustable t :fill-pointer 0))
        (position 0)
        (nextpos  0)
        (strlen   (length string)))
    (declare (type simple-string string separators)
             (index-plus-1 position nextpos)
             (array-index strlen)
             (array chunks))
    (loop 
       :while (< position strlen)
       :do (loop 
              :while (and (< nextpos strlen)
                          (not (position (char string nextpos) separators)))
              ;; :NOTE Don't change this to:  :do (incf nextpos))
              :do (setq nextpos (1+ nextpos)))
       (vector-push-extend (subseq string position nextpos) chunks)
       ;; :NOTE Don't change this to:  (incf position (1+ nextpos))
       (setq position (1+ nextpos))
       (setq nextpos  position)
       ;; :finally (return (coerce (the array chunks) 'list))
       :finally (return (map 'list #'identity (the array chunks))))))

;; mon:string-trim-whitespace
;; (map 'list #'identity #(a b c)) 
;;; :SOURCE chunga-1.1.1/read.lisp :WAS `trim-whitespace'
(defun string-trim-whitespace (string &key (start 0) (end (length string)))
  ;; optimized version to replace STRING-TRIM, suggested by Jason Kantz
  ;; #+:lispworks (hcl:fixnum-safety 0)))
  (declare (type string string)
           (inline whitespace-char-p))
  #-sbcl (assert (string-not-null-p arg)
                 (arg)
                 ":FUNCTION `string-trim-whitespace' -- ~
                  arg STRING did not satisfy `string-not-null-p'~% ~
                  got: ~S ~% ~
                  type-of: ~S~%"
                 arg (type-of arg))
  (if (or (string-empty-p string) 
          (and (= (the array-length (length string)) 1)
               (whitespace-char-p (char (the string-not-null-or-empty string) 0))))
      ;; :NOTE Are the consequences/benefits to doing this instead:
      ;; (make-string 0 :element-type 'standard-char)
      (make-string 0 :element-type 'character)
      (locally 
          (declare (type string-not-null-or-empty string)
                   (array-index start)
                   (index-or-minus-1 end)
                   (optimize (speed 3)))
        (let* ((str-ghost (copy-seq string)) ;; copy-seq returns a simple-string.
               (start% (loop 
                          ;; :WAS :with str = (the string string)
                          :with str = (the simple-string str-ghost)
                          :for i :of-type index-or-minus-1 :from start :below end :by 1
                          ;; :WAS :while (whitespace-char-p (char str i))
                          :while  (whitespace-char-p (schar str i))
                          :finally (return i)))
               (end% (loop
                        ;; :WAS :with str = ;; :WAS (the string string)
                        :with str = (the simple-string str-ghost)
                        :for i :of-type index-or-minus-1 :downfrom (1- end) :downto start :by 1
                        ;; :WAS :while (whitespace-char-p (char str i))
                        :while (whitespace-char-p (schar str i))
                        :finally (return (1+ i))))
               )
          (declare (index-or-minus-1 start% end%))
          (cond ((and (zerop start%) (= end% (the array-length (length string))))
                 ;; (break "with string: ~S start was: 0 end% was: ~d" string end%)
                 (make-array end% :element-type (array-element-type string) :initial-contents string))
                ((> start% end%) 
                 (make-string 0 :element-type 'character))
                (t ;; (break "with string: ~S start was: ~d end% was: ~d" string start% end%)
                 (loop 
                    with new-str = (make-array (- end% start%) :element-type (array-element-type string))
                    ;; :WAS with str = (the string string)
                    with str = (the simple-string string)
                    for idx-new upfrom 0 to end% by 1
                    for idx-old from start% below end% by 1
                    ;; :WAS do  (setf (char new-str idx-new) (char str idx-old))
                    do  (setf (char new-str idx-new) (schar str idx-old))
                    finally (return new-str)) ))))))

(defun string-split-newline (string)
  (declare (type string string))
  (loop 
     :for i = 0 :then (1+ j)
     :as j = (position #\newline string :start i)
     :collect (subseq string i j)
     :while j))

;; :COURTESY PJB :SOURCE (URL `http://paste.lisp.org/display/120998')
(defun string-split-on-column (string column)
  ;; (last-elt (string-split-on-column (format nil "Hello World~%How do yo do?~%Comment ça va?~%") 8))
  (declare (type string string)
           (type index column))
  (mapcan (lambda (line)
            (loop
               with i = 0
               while (< (+ i column) (length line))
               collect (subseq line i (min (length line) (incf i column))) into result
               finally (return (nconc result (list (subseq line i))))))
          ;; :WAS (split-sequence:split-sequence  #\newline string)
          (string-split-newline string)))

;; :SOURCE clocc/src/string.lisp
(defun string-split-multi (str chars &rest opts)
  (declare (type string str)
	   (type sequence chars))
  ;; (apply #'mon::split-seq str #'(lambda (ch) 
  (apply #'split-seq str #'(lambda (ch) 
			     (declare (character ch)) 
			     (find ch chars))
         opts))

(defun substring (string-seq from &optional (to (length (the string string-seq))))
  (declare (type string string-seq)
	   (type fixnum from to))
  (subseq string-seq from to))

;;; MY version (URL `http://paste.lisp.org/+2LC4/1')
;; (defun string-insert-char (string char index)
;;   (declare (type string string)
;;            (type character char)
;;            ;; SBCL x86-32
;;            ((mod 536870910) index)
;;            (optimize speed))
;;   (let* ((oldlen (length string))
;;          (newlen (if (zerop oldlen)
;;                      (return-from string-insert-char (string char))
;;                      (1+ oldlen)))
;;          (result (make-array newlen :element-type 'character)))
;;     (declare ((simple-array character (*)) result))
;;     (unless (< index newlen)
;;       (error "arg INDEX must be less than ~D" newlen))
;;     (if (or (zerop index) (= index oldlen))
;;         (loop
;;            initially (setf (char result index) char)
;;            for x across string
;;            for y upfrom (if (zerop index) 1 0) below (if (zerop index) newlen oldlen)
;;            do (setf (char result y) x)
;;            finally (return result))
;;         (loop 
;;            for n from 0 below index
;;            do (setf (char result n) (char string n))
;;            finally (return 
;;                      (loop 
;;                         initially (setf (char result (1- index)) char)
;;                         for o upfrom index below newlen
;;                         do (setf (char result o) (char string (1- o)))
;;                         finally (return result)))))))

(defun string-insert-char (string char index)
  (declare (type string string)
           (type character char)
           ;; SBCL x86-32
           ((mod 536870910) index)
           (optimize speed))
  (let* ((oldlen (length string))
         (newlen (if (zerop oldlen)
                     (return-from string-insert-char (string char))
                     (1+ oldlen)))
         (result (make-array newlen :element-type 'character)))
    (declare ((simple-array character (*)) result))
    (unless (< index newlen)
      (error "arg INDEX must be less than ~D" newlen))
    (flet ((copy (str chr idx old new rslt)
             (if (or (zerop idx) (= idx old))
                 (loop
                    initially (setf (char rslt idx) chr)
                    for x across str
                    for y upfrom (if (zerop idx) 1 0) below (if (zerop idx) new old)
                    do (setf (char rslt y) x)
                    finally (return rslt))
                 (loop 
                    for n from 0 below idx
                    do (setf (char rslt n) (char str n))
                    finally (return 
                              (loop 
                                 initially (setf (char rslt (1- idx)) chr)
                                 for o upfrom idx below new
                                 do (setf (char rslt o) (char string (1- o)))
                                 finally (return rslt)))))))
      (declare (inline copy))
      (typecase string
        ((simple-array character (*))
         (funcall #'copy string char index oldlen newlen result))
        ((simple-base-string)
         (funcall #'copy string char index  oldlen newlen result))
        (t (funcall #'copy string char index  oldlen newlen result))))))
      

;; 3b's version (URL `http://paste.lisp.org/+2LC4/5') :WAS `push-in-place2'
(defun string-insert-char-3b (word letter place)
  (declare (string word) 
           (character letter) 
           (fixnum place)
           (optimize speed))
  (let ((ret (make-string (1+ (length word)))))
    (flet ((copy ()
             (when (plusp place)
               (replace ret word :end1 place))
             (setf (char ret place) letter)
             (when (< place (length word))
               (replace ret word :start1 (1+ place) :start2 place))))
      (declare (inline copy))
      (typecase word
        ((simple-array character (*))
         (copy))
        ((simple-base-string)
         (copy))
        (t (copy)))
      ret)))
;;
;; (string-insert-char "" #\a 0)
;; (string-insert-char "m" #\a 1)
;; (string-insert-char "mb" #\a 0)
;; (string-insert-char "vmamb" #\a 5)

;;; ==============================
;;; :NOTE lice/fns.lisp had this which arnesi says is slow.
;;;  (apply 'concatenate 'string strings))
;;; :COURTESY arnesi/string.lisp
;;; :NOTE Issue 155 REST-LIST-ALLOCATION CLTL2 p 77-78 
;;; :SEE info node (info "(ansicl)Ordinary Lambda Lists")
;;; :SEE (URL `http://www.lispworks.com/documentation/HyperSpec/Issues/iss297_w.htm')
;; ,----
;; | the value of an &REST parameter is permitted, but not required,
;; | to share (top-level) structure with the last argument to APPLY.
;; `----
;; This means we must not: (setf (do thing to strings) strings)
;; without first copying the &rest list
;; :NOTE Has regression test `mon-test:concat-TEST'
(defun concat (&rest strings)
  (declare (each-a-sequence-proper-or-character strings)
           (optimize (speed 3)))
  (let* ((strings-cln   
          (if (or (null strings) (every #'null strings))
              (return-from concat (make-string 0))
              (string-seqs-convert-chars-if
               (if (some #'null strings)
                   (remove-if #'null strings)
                   (copy-seq strings)))))
         (length (reduce #'+ strings-cln :key #'length))
         (result (make-string length)))
    (declare ((integer 0 *) length)
             ((simple-array character (*)) result)
             (each-a-sequence strings-cln))
    (loop
       :for string :in strings-cln
       :for start = 0 :then end
       :for end = (+ start (the array-length (length string)))
       :while string
       :do (replace result string :start1 start :end1 end)
       :finally  (return result))))

;;; ==============================
;; Initial version used a stepping iterator over each elt
;; (defun string-seqs-convert-chars-if (string-seq)
;;   (declare (list string-seq))
;;   (if (notany #'characterp string-seq)
;;       string-seq
;;       (loop 
;;          for char-psn in string-seq
;;          for cnt from 0
;;          when (characterp char-psn) 
;;          do (setf (elt string-seq cnt) (string char-psn))
;;          finally (return string-seq))))
;;
(defun string-seqs-convert-chars-if (string-seq)
  (declare (each-a-sequence-proper-or-character string-seq)
           (optimize (speed 3)))
  (if (notany #'characterp string-seq)
      string-seq
      (loop 
         for char-psn = (position-if #'characterp string-seq)
         then (position-if #'characterp string-seq :start char-psn)
         for char = (and char-psn (string (elt string-seq char-psn)))   
         while char
         do (setf (elt string-seq char-psn) char)
         finally (return string-seq))))

;;; ==============================
;;; :COURTESY PJB 
;;; :SEE (URL `http://groups.google.com/group/comp.lang.lisp/browse_frm/thread/2d71b553b62e20b5#')
;;; :SEE :FILE com/informatigo/tools/script.lisp
;;; :SOURCE comp.lang.lisp :DATE 2009-06-13 :SUBJECT Re: Emacs Lisp's "mapconcat" in Common Lisp?
;; (defun mapconcat-old (map-fun sequence separator)  ;; :WAS (function sequence separator) 
;;   (etypecase sequence
;;     (list 
;;      (if sequence
;; 	 (let* ((items (mapcar (lambda (item)
;; 				 (let ((sitem (funcall map-fun item)))
;;                                    ;; I've found an implementation issue where the function differs from
;;                                    ;; the emacs lisp equivalent: 
;;                                    ;; elisp>   (mapconcat #'identity (list "a" nil "b" nil "c") "") => "abc"
;;                                    ;; cl-user> (mapconcat #'identity (list "a" nil "b" nil "c") "") => "aNILbNILc"
;; 				   ;; Following `and' form fixes a bug in PJB's implementation and omits nulls
;; 				   (and sitem (if (stringp sitem) sitem (princ-to-string sitem)))))
;; 			       sequence))
;; 		(ssepa (if (stringp separator)
;; 			   separator
;; 			   (princ-to-string separator)))
;; 		(size (+ (reduce (function +) items :key (function length))
;; 			 (* (length ssepa) (1- (length items)))))
;; 		(result (make-array size :element-type 'character))
;; 		(start  0))
;; 	   ;; :WAS (replace result  (first items) :start1 start)
;; 	   (replace result  (car items) :start1 start)
;; 	   ;; :WAS (incf start (length (first items)))
;; 	   (incf start (length (car items)))
;; 	   ;; :WAS (dolist (item (rest items))) 
;; 	   (dolist (item (cdr items) result) 
;; 	     (replace result ssepa :start1 start) (incf start (length ssepa))
;; 	     (replace result item  :start1 start) (incf start (length item)))
;; 	   ) ;;result)
;; 	 ""))
;;     (vector
;;      (if (plusp (length sequence))
;; 	 (let* ((items (map 'vector #'(lambda (item)
;; 					(let ((sitem (funcall map-fun item)))
;; 					  (and sitem
;;                                                (if (stringp sitem)
;;                                                    sitem
;;                                                    (princ-to-string sitem)))))
;; 			    sequence))
;; 		(ssepa (if (stringp separator)
;; 			   separator
;; 			   (princ-to-string separator)))
;; 		(size (+ (reduce (function +) items :key (function length))
;; 			 (* (length ssepa) (1- (length items)))))
;; 		(result (make-array size :element-type 'character))
;; 		(start  0))
;; 	   (replace result (aref items 0) :start1 start)
;; 	   (incf start (length (aref items 0)))
;; 	   (loop
;; 	      for i from 1 below (length items)
;; 	      do (replace result ssepa :start1 start)
;; 		 (incf start (length ssepa))
;;  		 (replace result (aref items i) :start1 start) 
;; 		 (incf start (length (aref items i))))
;; 	   result)
;; 	 ""))))
;;
;; (defun test/mapconcat-old ()
;;   (loop :for (expression expected)
;;      :in '(((mapconcat-old (lambda (x) (and x (string-downcase x))) '("one" two three nil "five") "-")
;;             "one-two-three--five")
;;            ((mapconcat-old (lambda (x) (and x (string-downcase x))) '("one") "-")
;;             "one")
;;            ((mapconcat-old (lambda (x) (and x (string-downcase x))) '(nil) "-")
;;             "")
;;            ((mapconcat-old (lambda (x) (and x (string-downcase x))) '() "-")
;;             "")
;;            ((mapconcat-old (lambda (x) (and x (string-downcase x))) #("one" two three nil "five") "-")
;;             "one-two-three--five")
;;            ((mapconcat-old (lambda (x) (and x (string-downcase x))) #("one") "-")
;;             "one")
;;            ((mapconcat-old (lambda (x) (and x (string-downcase x))) #(nil) "-")
;;             "")
;;            ((mapconcat-old (lambda (x) (and x (string-downcase x))) #() "-")
;;             ""))
;;      :do (assert (equal (eval expression) expected)
;;                  ()
;;                  "~%Expression: ~S~%Expected: ~S~%Got: ~S~%"
;;                  expression expected (eval expression)))
;;   :success)
;;
;; (test/mapconcat-old)

;;; ==============================
;;; :COURTESY PJB 
;;; :SEE (URL `http://groups.google.com/group/comp.lang.lisp/browse_frm/thread/2d71b553b62e20b5#')
;;; :SEE :FILE com/informatigo/tools/script.lisp
;;; :SOURCE comp.lang.lisp :DATE 2009-06-13 :SUBJECT Re: Emacs Lisp's "mapconcat" in Common Lisp?
;;;;;;;
;;; New version  <Timestamp: #{2011-01-29T17:46:15-05:00Z}#{11046} - by MON>
;;; :SOURCE (URL `http://paste.lisp.org/display/119172')
;;; 1- mon_key on irc://irc.freenode.org/#lisp signaled that nil are
;;;    substituted by empty strings in emacs mapconcat.
;;; 2- Factorized out the processing for both vectors and lists in JOIN.
(defun mapconcat (function sequence separator)
  (labels ((process-item (item)
             (let ((sitem (funcall function item)))
               (cond
                 ((stringp sitem) sitem)
                 ((null sitem)    "")
                 (t               (princ-to-string sitem)))))
           (join (items)
             (let* ((ssepa (if (stringp separator)
                               separator
                               (princ-to-string separator)))
                    (size   (+ (reduce (function +) items :key (function length))
                               (* (length ssepa) (1- (length items)))))
                    (result (make-string size :element-type 'character))
                    (start  0))
               (replace result  (first items) :start1 start)
               (incf start (length (first items)))
               (dolist (item (rest items))
                 (replace result ssepa :start1 start) (incf start (length ssepa))
                 (replace result item  :start1 start) (incf start (length item)))
               result)))
    (etypecase sequence
      (list
       (if sequence
           (join (mapcar (function process-item) sequence))
           ""))
      (vector
       (if (plusp (length sequence))
           (join (map 'list (function process-item) sequence))
           "")))))

;; :SOURCE cl-data-format-validation-20101006-git/validation.lisp :WAS `join-strings'
(defun string-join-strings (strings  &optional (separator #\space))
  (let ((writer (etypecase separator
                  (character #'write-char)
                  (sequence  #'write-sequence))))
    (with-output-to-string (os)
      (let ((firstp t))
        (map 'nil
             #'(lambda (string)
                 (if firstp
                     (setf firstp nil)
                     (funcall writer separator os))
                 (write-string string os))
             strings)))))

;;; :COURTESY mcclim/Tools/gilbert/clim-doc-convert.lis p:WAS `map-over-string'
;;; :NOTE `+ucs-escape+' (defconstant +ucs-escape+ (code-char 21)) 
(defun string-map (fun str)
  (assert (typep str 'string))
  (locally
      (declare (type string str)
               (type function fun) ;; :ADDED
               (optimize (speed 3) (safety 1)))
    (let ((n (length str)))
      (declare (type fixnum-exclusive n))
      (do ((i 0 (the fixnum-exclusive (+ i 1))))
	  ((>= i n))
	(declare (type fixnum-exclusive i))
        (let ((c (schar str i)))
          (cond ((char= c #\Nak) ;; (char= c +ucs-escape+)
                 (let ((c (parse-integer str 
					 :start (+ i 1) 
					 :end (+ i 5) 
					 :junk-allowed nil 
					 ;; Is this the radix we want? It is if we want HEX.
					 :radix 16))) 
                   (incf i 4)
                   (funcall fun c)))
                (t
                 (funcall fun c))))))))

(defun string-or-char-or-code-point-integer-if (obj)
  (if (typep obj 'string-or-char-or-code-point-integer)
      (typecase obj 
        (string    (copy-seq obj))
        (character obj)
        (char-code-integer   (char-code-integer-to-char obj)))
      (simple-error-mon :w-sym     'string-or-char-or-code-point-integer-if 
                        :w-type    'function 
                        :w-spec    "Arg OBJ not of type `string-or-char-or-code-point-integer'"
                        :w-got     obj
                        :w-type-of t
                        :signal-or-only nil)))

(defun string-symbol-or-char-or-code-point-integer-if (obj)
  (if (typep obj 'string-symbol-or-char-or-code-point-integer)
      (typecase obj 
        (string    (copy-seq obj))
        (symbol    (string obj))
        (character obj)
        (char-code-integer   (char-code-integer-to-char obj)))
      (simple-error-mon :w-sym     'string-symbol-or-char-or-code-point-integer-if 
                        :w-type    'function 
                        :w-spec    "Arg OBJ not of type `mon:string-symbol-or-char-or-code-point-integer'"
                        :w-got     obj
                        :w-type-of t
                        :signal-or-only nil)))

(defun string-symbol-or-char-if (obj)
  (if (typep obj 'string-symbol-or-char)
      ;; :WAS (typecase obj
      (etypecase obj
        (symbol (string obj))
        (string (copy-seq obj))
        (character obj))
      (simple-error-mon :w-sym     'string-symbol-or-char-if
                        :w-type    'function 
                        :w-spec    "Arg OBJ not of type `mon:string-symbol-or-char'"
                        :w-got     obj
                        :w-type-of t
                        :signal-or-only nil)))

;;; ==============================
(defun downcase (obj)
  (let ((chk-dwn (string-symbol-or-char-if obj)))
    (declare (string-symbol-or-char chk-dwn))
    (typecase chk-dwn
      (character (char-downcase chk-dwn))
      (string (string-downcase chk-dwn)))))

(defun upcase (obj)
  (let ((chk-up (string-symbol-or-char-if obj)))
    (declare (string-symbol-or-char chk-up))
    (typecase chk-up
      (character (char-upcase chk-up))
      (string (string-upcase chk-up)))))

(defun capitalize (obj)
  (let ((chk-cap (string-symbol-or-char-if obj)))
    (declare (string-symbol-or-char chk-cap))
    (typecase chk-cap
      (character (if (upper-case-p chk-cap)
                     chk-cap
                     (char-downcase chk-cap)))
      (string (string-capitalize chk-cap)))))

;;; ==============================
(defun capitalize-loosely (obj)
  (let ((cptlz-if (string-symbol-or-char-or-code-point-integer-if obj)))
    (declare (string-or-char-or-code-point-integer cptlz-if))
    (typecase cptlz-if
      (string 
       ;; `nstring-capitalize' is fine b/c `%capitalize-if' `cl:copy-seq'd CPTLZ-IF
       (nstring-capitalize cptlz-if))
      (character 
       (char-code (if (upper-case-p cptlz-if)
                      (char-downcase cptlz-if)
                      (char-upcase cptlz-if)))))))

(defun downcase-loosely (obj)
  (let ((dwncs-if (string-symbol-or-char-or-code-point-integer-if obj)))
    (declare (type string-or-char-or-code-point-integer dwncs-if))
    (typecase dwncs-if
      (string 
       ;; `nstring-capitalize' is fine b/c `string-symbol-or-char-or-code-point-integer-if'
       ;; has `cl:copy-seq'd DWNCS-IF
       (nstring-downcase dwncs-if))
      (character  (char-code (char-downcase dwncs-if))))))

(defun upcase-loosely (obj)
  (let ((upcs-if (string-symbol-or-char-or-code-point-integer-if obj)))
    (declare (string-or-char-or-code-point-integer upcs-if))
    (typecase upcs-if
      (string 
       ;; `nstring-capitalize' is fine b/c `string-symbol-or-char-or-code-point-integer-if'
       ;; has `cl:copy-seq'd UPCS-IF
       (nstring-upcase upcs-if))
      (character  (char-code (char-downcase upcs-if))))))

(defun string-invert-case (string-to-invert &key (case :preserve))
  (declare (type string string-to-invert))
  (unless (memq case '(:preserve :upcase :downcase :invert))
    (setf case :preserve))
  (if (string-empty-p string-to-invert)
      (make-string 0)
      (case case
        (:preserve string-to-invert)
        (:upcase  
         (if (some #'lower-case-p  string-to-invert)
             (string-upcase string-to-invert) 
             string-to-invert))
        (:downcase 
         (if (some #'upper-case-p string-to-invert)
             (string-downcase string-to-invert)
             string-to-invert))
        (:invert 
         (loop 
            :with sym-str = (copy-seq string-to-invert)
            :for idx :upfrom 0 :below (length sym-str)
            :for charat :across sym-str
            :do (if (lower-case-p charat)
                    (setf (char sym-str idx) (char-upcase charat))
                    (setf (char sym-str idx) (char-downcase charat)))
            :finally (return sym-str))))))

(defun string-for-readtable-case (case-frob-string &optional readtable)
  (declare (type string case-frob-string)
           ((or null readtable) readtable))
  (string-invert-case case-frob-string 
                      :case (or 
                             (and readtable 
                                  (or 
                                   (and (readtablep readtable)
                                        (readtable-case readtable))
                                   (simple-error-mon :w-sym  'string-for-readtable-case
                                                     :w-type 'function
                                                     :w-spec "arg READTABLE not `readtablep'"
                                                     :w-got  readtable
                                                     :w-type-of readtable
                                                     :signal-or-only nil)))
                             (readtable-case (or readtable *readtable*)))))

;;; :COURTESY mcclim/ESA/utils.lisp :WAS `no-upper-p'
(defun string-no-upper-p (string)
  (declare (type string string))
  (notany #'upper-case-p string))

;; sbcl/src/code/early-extensions.lisp
#+sbcl
(defun string-remove-backslashes (string &key (start 0) (end (length string)))
  (declare (type string string))
  (etypecase string
    (simple-string (sb-impl::remove-backslashes string start end))
    (string (copy-seq (substring start end)))))

;;; :NOTE Had comment; 
;;;  "FIXME: This is a *very* big kludge, waiting for babel to be fixed."
;; :COURTESY osicat.lisp :WAS `to-simple-string'
;; (defun string-simple-string (thing)
;;   (let ((s (string thing)))
;;     (make-array (length s)
;;                 :element-type 'character
;;                 :initial-contents s)))	 


;;; :SOURCE xit/utilities/file-browser.lisp :WAS `str-upto-char'
(defun string-upto-char (str chr)
  (declare (type character chr)
           (type string str))
  (let ((l (coerce str 'list)))
    (coerce (subseq l 0 (1+ (or (position chr l) (length l))))
	    'string)))

;;; :SOURCE texinfo-docstrings/colorize.lisp
(defun string-starts-with (starts-w str)
  (declare (type string str starts-w)
           (optimize (speed 3)))
  (let ((stw-len (length starts-w)))
    (declare (array-length stw-len))
    (and (>= (the array-length (length str)) stw-len)
         (string-equal starts-w str :end2 stw-len))))

;;; :SOURCE mcclim/Drei/lisp-syntax-swine.lisp
(defun string-longest-common-prefix (strings &optional null-as-nil)
  (declare (each-a-string-or-null strings)
           (optimize (speed 3)))
  (when (null strings) 
    (return-from string-longest-common-prefix 
      (if null-as-nil 
          nil 
          (make-string 0))))  
  (locally (declare (each-a-string strings))
    (flet ((common-prefix (s1 s2)
             (declare (string s1 s2))
             (let ((diff-pos (mismatch s1 s2)))
               (if diff-pos (subseq s1 0 diff-pos) s1))))
      (reduce #'common-prefix strings))))

;;; :SOURCE texinfo-docstrings/docstrings.lisp :WAS `flatten-to-string'
;;; :SOURCE /mcclim/Doc/docstrings.lisp :WAS `flatten-to-string'
(defun flatten-list-to-string (list)
  (declare (type list list))
  (format nil "~{~A~^-~}" (flatten list)))

;;; :SOURCE texinfo-docstrings/docstrings.lisp :WAS `string-lines'
(defun string-lines-to-array (string)
  (declare (type simple-string string))  
  (coerce (with-input-from-string (s string)
            (loop
               :for line = (read-line s nil nil)
               :while line :collect line))
          'vector))

;;; :COURTESY Larry Hunter. Obtained from Google.
(defun string-substitute (string substring replacement-string)
  (declare (type string string substring replacement-string))
  (assert (> (length substring) 0))
  (let ((substring-length (length substring))
    (last-end 0)
    (new-string ""))
    (do ((next-start (search substring string) (search substring string :start2 last-end)))
	((null next-start)
	 (concatenate 'string new-string (subseq string last-end)))
      (setq new-string
	    (concatenate 'string new-string (subseq string last-end next-start) replacement-string))
      (setq last-end (+ next-start substring-length)))))

(defun string-last-word (string)
  (let ((pos (position #\space string :from-end t)))
    (if pos
	(substring string (1+ pos))
	string)))

;;; :SOURCE sbcl/contrib/sb-cover/cover.lisp
(defun string-convert-tabs (tabby-str &optional omit-last-newline)
  (declare (type string-or-null tabby-str))
  (unless (null tabby-str)
    (with-output-to-string (stream)
      (loop 
         :for char :across (the string tabby-str)
	 :for col :from 0
	 :for i :from 0
	 :do (if (eql char #\Tab)
                 (loop 
                    :repeat (- 8 (mod col 8))
                    :do (write-char #\Space stream)
                    :do      (incf (the fixnum-exclusive col))  ;;fixnum col))
                    :finally (decf (the fixnum-exclusive col))) ;; fixnum col)))
                 (progn
                   (when (eql char #\Newline)
                     ;; Filter out empty last line
                     (when (and omit-last-newline 
                                (eql (the index i) (1- (length (the string tabby-str)))))
                       (return))
                     (setf col -1))
                   (write-char char stream)))))))

(defun string-is-nil-like (string)
  (declare (type string-or-null string))
  (or (null string)
      (string-empty-p string)
      (string-equal (string-trim-whitespace (the string string)) "NIL")
      (not (position-if-not #'mon:whitespace-char-p (the string string)))))

(defun string-or-symbol-first-char (string-or-symbol)
  (declare ((or symbol string-or-null) string-or-symbol))
  (unless (null string-or-symbol)
    (typecase  string-or-symbol
      ;; ((simple-array character (*)) (string-first-char string-or-symbol))
      (boolean        string-or-symbol)
      (string  (or (string-equal string-or-symbol "t")
                   (and (string-not-equal string-or-symbol "nil")
                        (string-first-char string-or-symbol))))
      (symbol         (string-first-char (symbol-name string-or-symbol))))))


;;; ==============================
;;; :STRINGS-DOCUMENTATION
;;; ==============================

(fundoc 'string-to-char
  "Return first char of string to a character code value as if by `char-code'.~%~@
When keyword W-CODE-CHAR is supplied convert car.~%~@
:EXAMPLE~%
 \(string-to-char \"bbba\"\)~%
 \(string-to-char \"bbba\" :w-code-char nil\)~%
 \(string-to-char \"bbba\" :w-code-char nil\)~%
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `char-to-string', `string-to-number', `char-code-limit' `char-code',
`code-char', `char-name', `name-char', `char-upcase', `char-downcase',
`char-int', `schar', `digit-char', `character', `base-char', `standard-char',
`extended-char',`standard-char-p', `graphic-char-p',
`alpha-char-p',`digit-char-p', `alphanumericp', `upper-case-p', `lower-case-p',
`both-case-p', `char=', `char/=', `char<', `char>', `char<=', `char>=',
`char-equal', `char-not-equal+'.~%▶▶▶")

(fundoc 'string-cat
 "Concatenate a list of strings into a single one.~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'concat
        "Concatenate all the arguments and make the result a string.~%~@
The result is a string whose elements are the elements of all the arguments.~%~@
Each argument must be a string.~%~@
:EXAMPLE~%
 \(concat \"a\" \"b\" \"c\"\)~%
 \(concat \"~~%\" \"a\"\)~%
 \(concat \"a\" #\(#\\a\) \"b\"\)~%
 \(concat \"a\" '\(#\\a\) \"b\"\)~%
 \(concat #\\a nil #\\b nil #\\c\)~%
 \(concat  nil\)~%
 \(concat \"a\" nil \"b\" nil\)~%
 \(concat \"a\" nil #\(#\\b\) nil\)~%
 \(concat \"a\" nil #\(\) nil\)~%
 \(concat nil #\(\) \(\)\)~%
 \(concat \(format nil \"~~a~~%\" \"a\"\) \"b\" \"c\"\)~%~@
:EMACS-LISP-COMPAT~%~@
:NOTE Unlike Emacs' concat where \(concat \"\\n\" \"a\"\) returns the #\\a char
on the second line, this concat does not interpolate format directives as
chararacter literals e.g.:~%
 `~~%' as \(#\\Newline\)~%~@
:NOTE Unlike Emacs' concat it is permissible to pass chararacter literals, e.g.:~%~@
 \(concat #\\a nil #\\b nil #\\c\)~%~@
:SEE-ALSO `mon:mapconcat', `mon:string-cat'.~%▶▶▶")

(fundoc 'downcase
  "Convert argument to lower case and return that.~%~@
The argument may be a character or string.  The result has the same type.~%~@
The argument object is not altered -- the value is a copy.~%~@
:EXAMPLE~%
 \(downcase \"BUBBA\"\)~%
 \(downcase #\\B\)~%~@
:SEE-ALSO `capitalize', `upcase',`upcase-initials'.~%▶▶▶")

(fundoc 'upcase
"Convert argument to upper case and return that.~%~@
The argument may be a character or string.  The result has the same type.~%~@
The argument object is not altered -- the value is a copy.~%~@
:EXAMPLE~%
 \(upcase \"bubba\"\)~%
 \(upcase #\\b\)~%~@
:SEE-ALSO `mon:upcase', `mon:downcase', `mon:capitalize', `upcase-initials',
`cl:string-capitalize', `cl:nstring-capitalize', `cl:string-upcase',
`cl:string-downcase', `cl:schar', `cl:char-upcase', `cl:char-downcase',
`cl:upper-case-p', `cl:lower-case-p', `cl:character', `cl:base-char',
`cl:standard-char', `cl:extended-char', `cl:standard-char-p',
`cl:characterp'~%▶▶▶")

(fundoc 'capitalize
"Convert argument to capitalized form and return that.~%~@
This means that each word's first character is upper case
and the rest is lower case.~%~@
The argument may be of type `cl:character' `cl:string' 
`mon:code-point', `mon:char-code-integer' \(when not sb-unicode\).~%~@
The argument object is not altered--the value is a copy.~%~@
:EXAMPLE~%
 \(capitalize \"make me more formal\"\)~%
 \(capitalize #\\a\)~%
 \(code-char \(capitalize #\\a\)\)~%
 \(capitalize #\\►\)~%
 \(code-char \(capitalize #\\►\)\)~%
 \(capitalize 8\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:upcase', `mon:downcase', `cl:string-capitalize',
`cl:nstring-capitalize', `cl:string-upcase', `cl:string-downcase', `cl:schar',
`cl:char-upcase', `cl:char-downcase', `cl:upper-case-p', `cl:lower-case-p',
`cl:character', `cl:base-char', `cl:standard-char', `cl:extended-char',
`cl:standard-char-p', `cl:characterp', `sb-impl::**character-database**',
`sb-impl::*base-char-name-alist*',
`sb-impl::*unicode-character-name-database*'.~%▶▶▶")

(fundoc 'downcase-loosely
"Like `mon:downcase' but supports `char-code-integer's like emacs lisp.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'upcase-loosely
"Like `mon:upcase' but supports `char-code-integer's like emacs lisp.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'capitalize-loosely
"Like `mon:capitalize' but supports `char-code-integer's like emacs lisp.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'string-symbol-or-char-if
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `string-symbol-or-char-if', `string-or-char-or-code-point-integer-if',
`string-symbol-or-char-or-code-point-integer-if'.~%▶▶▶")

(fundoc 'string-or-char-or-code-point-integer-if
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `string-symbol-or-char-if', `string-or-char-or-code-point-integer-if',
`string-symbol-or-char-or-code-point-integer-if'.~%▶▶▶")

(fundoc 'string-symbol-or-char-or-code-point-integer-if
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `string-symbol-or-char-if', `string-or-char-or-code-point-integer-if',
`string-symbol-or-char-or-code-point-integer-if'.~%▶▶▶")

;; (setf (documentation 'string-simple-string 'function)
;;       #.(format nil
;; "Convert THING to a string of type `simple-string'~%
;; That is, make an array of element-type `character' with THING as its
;; initial-contents.~%
;; :EXAMPLE~%~%(string-simple-string '(#\s #\i #\m #\p #\l #\e\)\)~% 
;; :SEE-ALSO `<XREF>'.~%▶▶▶"))

(fundoc 'substring
  "Return a new string whose contents are a substring of string.~%~@
The returned string consists of the characters between index FROM
\(inclusive\) and index TO (exclusive) of string.~%~@
FROM and TO are zero-indexed:~%~@
 0 means the first character of string.~%~@
Negative values are counted from the end of string.~%~@
If TO is nil, the substring runs to the end of string.
The string argument may also be a vector.~% 
In that case, the return value contains the elements between index
FROM \(inclusive\) and index TO (exclusive) of that vector argument.~%~@
:EXAMPLE~%
 \(substring \"abcd\" 3\)~%
 \(substring \"abcd\" 1 3\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:substring', `mon:string-remove-backslashes',
`mon:string-trim-whitespace', `mon:string-split-newline',
`mon:string-substitute', `mon:string-split-multi', `mon:string-split-spaces',
`mon:string-upto-char', `mon:string-split-on-chars'.~%▶▶▶")

(fundoc 'string-upto-char
"Return left part of string STR up to CHR `(inclusive\).~%~@
If CHR is not found or is the last character of STR return whole string.~%~@
:EXAMPLE~%
 \(string-upto-char \"bubbam\" #\\a\)~%~@
:SEE-ALSO `substring', `mon:string-remove-backslashes', `mon:string-trim-whitespace',
`mon:string-split-newline', `mon:string-substitute', `mon:string-split-multi',
`mon:string-split-spaces', `mon:string-split-on-chars'.~%▶▶▶")

(fundoc 'mapconcat
 "Apply FUNCTION to each elt of SEQUENCE, and concatenate the results as strings.~%~@
SEPARATOR is a string or char interspersed between values returned by FUNCTION.~%~@
:EXAMPLE~%
 \(values \(mapconcat \(function 1+\) '\(1 2 3 4\) #\\-\)
	 \(mapconcat \(function 1+\) #\(1 2 3 4\) #\\-\)\)~%
 \(mapconcat #'identity 
	    \(lambda \(x\) \(format nil \"~~d~~%\" \(1+ x\)\)\) #\\Newline\)~%
 \(mapconcat #'identity '\(1 2 3 4\) #\\Newline\)~%
 \(mapconcat #'identity '\(1 2 3 4 #\\newline\) \"\"\)
 \(mapconcat #'identity '\(1 2 3 4\) \"-\"\)~%
 \(mapconcat #'identity '\(\"a\" \"b\" \"c\" \"d\"\) #\\-\)~%
 \(mapconcat #'identity '\(#\\a #\\b #\\c #\\d\) #\\-\)~%
 \(mapconcat #'identity '\(#\\a #\\b #\\c #\\d\) \"-\"\)~%
 \(mapconcat #'identity '\(#\\a 8 #\\c 9\) \"-\"\)~%
 \(mapconcat #'identity #\(#\\a 8 #\\c 9\) \"-\"\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `cl:concatenate', `mon:concat'.~%▶▶▶")

(fundoc 'string-join-strings
  "Return a new string by joining together the STRINGS,
separating each string with a SEPARATOR character or string
:EXAMPLE~%
 \(join-strings '\(\"a\" \"b\" \"ab\" \"some string\"\) #\\*\)~%~@
:SEE-ALSO `mon:concat', `mon:mapconcat', `mon:string-map', `cl:concatenate'.~%▶▶▶")

(fundoc 'string-delimited-to-list
  "Split a string with delimiter.~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'string-reduce
  "Join adjacent strings in a list, leave other values intact.~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'string-implode
  "Reduce a string-list of to a single string, inserting separator SEP between them.~%~@
STRING-LIST is a list of strings.
SEP is a string to insert before each strin in STING-LIST.~%@
SEP should satisfy `cl:simple-stringp', signal an errro if not.~%@
:EXAMPLE~%
 \(string-implode \" \" '\(\"\" \"\" \"\" \"string3\"\)\)~%
 \(string-implode \" string\" '\(\"\" \"1\" \"2\" \"3\"\)\)~%
 \(string-implode \", \" '\(\"string\" \"string1\" \"string2\" \"string3\"\)\)~%~@
:SEE-ALSO `mon:string-explode', `mon:symbol-name-explode', `mon:concat',
`mon:string-cat', `mon:mapconcat', `cl:concatenate', `cl:make-string'.~%▶▶▶")

 ;; (string-implode "string1" " string2" " string3" " string4")

(fundoc 'string-explode
  "Return string as a list of strings.~%~@
:EXAMPLE~%
 \(string-explode \"bubba\"\)~%~@
:SEE-ALSO `mon:symbol-name-explode'.~%▶▶▶")

(fundoc 'symbol-name-explode
  "Return EXPLODE-SYM as a list of characters or strings of length 1.~%~@
EXPLODE-SYM is a symbol satisfying `cl:symbolp'.~%~@
When EXPLODE-SYM is `mon:booleanp' return values is T or NIL.~%~@
When optional arg AS-CHARS is non-nil return a list of characters, else return a
list of strings \(the default\).~%~@
:EXAMPLE~%
 \(symbol-name-explode 'bubba\)~%
 \(symbol-name-explode 'bubba t\)~%
 \(symbol-name-explode nil\)~%
 \(symbol-name-explode 'nil t\)~%
 \(symbol-name-explode t\)~%
 \(symbol-name-explode t t\)~%~@
:SEE-ALSO `mon:string-explode'.~%▶▶▶")

(fundoc 'string-lines-to-array
         "Read lines in STRING, return as array \(as if by `cl:vector'\).~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'string-replace-all
  "Return a new string in which all the occurences of the FROM-STRING
in STRING are replaced with TO-STRING.~%~@
Keyword test is a predicate to compare by. Default is `cl:char='.~%~@
:EXAMPLE~%
 \(string-replace-all \"a b B c\" \"b\" \"Q\" :test #'char-equal\)~%
 \(string-replace-all \"a b B c\" \"b\" \"Q\"\)~%~@
:NTOE To replace without case-sensitivity use :test #'char-equal~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'string-substitute
"Substitute in STRING with SUBSTRING using replacement-string~%~@
Signal an error when SUBSTRING is length 0.~%~@
:EXAMPLE~%~@
 \(string-substitute \"bubba\" \"b\" \" gorgon shield \"\)~%~@
:SEE-ALSO `mon:substring', `mon:string-remove-backslashes',
`mon:string-trim-whitespace', `mon:string-split-newline',
`mon:string-substitute', `mon:string-split-multi', `mon:string-split-spaces',
`mon:string-upto-char', `mon:string-split-on-chars'.~%▶▶▶")

(fundoc 'string-split-multi
  "Split the STR string on chars.~%~@
:EXAMPLE~%~@
 { ... EXAMPLE ... }~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `mon:substring', `mon:string-remove-backslashes',
`mon:string-trim-whitespace', `mon:string-split-newline',
`mon:string-substitute', `mon:string-split-multi', `mon:string-split-spaces',
`mon:string-upto-char', `mon:string-split-on-chars'.~%▶▶▶")

#+sbcl 
(fundoc 'string-remove-backslashes
  "Remove any occurrences of #\\ from STRING.~%~@
Keyword START is an index into STRING, default is 0.~%~@
Keyword END is an index into STRING, default is length of STRING.~%~@
:EXAMPLE~%
 \(string-remove-backslashes \"bubba\\bubba\\bubba\\\\\")~%
 \(string-remove-backslashes \"bubba\\bubba\\bubba\" :start 3 :end 8\)~%~@
:SEE-ALSO `sb-impl::remove-backslashes', `substring',
`string-remove-backslashes', `string-trim-whitespace', `string-split-newline',
`string-substitute', `string-split-multi', `string-split-spaces', `string-upto-char',
`string-split-on-chars'.~%▶▶▶")

(fundoc 'string-split-newline
  "Return STRING as a list of substrings of split on #\\newline.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:NOTE Two consecutive spaces will be seen as if there weak-pointer-value an
empty string between them.~%~@
:SEE-ALSO `mon:substring', `mon:string-remove-backslashes',
`mon:string-trim-whitespace', `mon:string-substitute', `mon:string-split-multi',
`mon:string-split-spaces', `mon:string-upto-char',
`mon:string-split-on-chars'.~%▶▶▶")

(fundoc 'string-split-on-chars
"Split STRING into substrings where there are matches for SEPARATORS.~%~@
Return a list of substrings \(sans seaparator\(s\)\).~%~@
SEPARATORS may be any of the following:~%
 - a string satisfying `string-not-empty';~%
 - a list of strings satisfying `each-a-string-of-length-1-p';~%
 - a character;~%
 - a list of characters;~%
 - a list of char-codes;~%~@
Defaults to a string built from characters of `mon:*whitespace-chars*'.~%~@
When SEPARATORS is ommitted and optional arg WHITE-ON-WHITE is non-nil allow
splitting STRING when `string-all-whitespace-p'. Defualt is to return STRING unmodified.~%~@
:EXAMPLE~%
 \(string-split-on-chars \"bub ba	bubba\"\)~%
 \(string-split-on-chars \"bubba	bubba\" \"b\"\)~%
 \(string-split-on-chars \"bubba\" #\\b\)~%
 \(string-split-on-chars \" b u bba \" 32\)~%
 \(string-split-on-chars \(format nil \"~~{~~C~~}\" *whitespace-chars*\)\)~%
 \(string-split-on-chars \(format nil \"~~{~~C~~}\" *whitespace-chars*\) nil t\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'make-string*
"Convert ARGS to string multiple times.~%~@
When first arg in ARGS is an integer create <N> instances of string produced by
coalescing remaining args.~%
:EXAMPLE~%
 \(make-string*\)~% ;=> \"\"~%
 \(make-string* 0 \"x\"\)~% ;=> \"\"~%
 \(make-string* 13 nil\)~% ;=> \"\"~%
 \(make-string* nil nil\)~% ;=> \"\"~%
 \(make-string* 13 \"\"\)~% ;=> \"\"~%
 \(make-string* 1 \"x\"\)~% ;=> \"x\"~%
 \(make-string* nil \"x\" \"8\"\)~% ;=> \"x8\"~%
 \(make-string* 3 \"x\"\)~% ;=> \"xxx\"~%
 \(make-string* \"x\"\)~% ;=> \"x\"~%
 \(make-string* #\\x\)~% ;=> \"x\"~%
 \(make-string* nil nil #\\x nil \"x\" nil\)~% ;=> \"xx\"~%
 \(make-string* 3 \"y\" \"x\"\)~% ;=> \"yxyxyx\"~%
 \(make-string* 3 #\\x \"x\"\)~% ;=> \"xxxxxx\"~%
 \(make-string* 3 nil #\\x nil \"x\" nil\)~% ;=> \"xxxxxx\"~%
 \(make-string* 2 \"foo\" \" \" 2 \" \"\)~%  ;=> \"foo 2 foo 2 \"~%
 \(make-string* 3 #\\Space #\\x \"x \" -3.3 \" \" 3/9\)~% ;=> \"xx -3.3 1/3xx -3.3 1/3xx -3.3 1/3\"~%
 \(make-string* 3.0 #\\Space #\\x #\\Space \"x\" #\\Space -3.3 #\\Space 3/9\)~% ;=> \"3.0 x x -3.3 1/3\"~%~@
Highly modified version of function presented on c.l.l here:~%~@
:SEE (URL `http://groups.google.com/group/comp.lang.lisp/msg/2f64b48bcb0a0519')~%~@
:SEE-ALSO `cl:make-string', `mon:mapconcat'.~%▶▶▶")

(fundoc 'string-trim-whitespace
        #.`(format nil
  "Return a copy of STRING trimmed of chars satisfying `mon:whitespace-char-p' between START and END.~%~@
Keyword START is a index into beginning of STRING. Default is 0.~%~@
Keyword END is a bounding index to trim from STRING. Default to STRING length.~%~@
:EXAMPLE~%
 \(string-trim-whitespace \"   trim around me  \"\)~%
 \(string-trim-whitespace \"   trim around me m \" :start 5 :end 14\)~%
 \(string-trim-whitespace  \"~C~C~C~C6 8 10\" :start 0 :end 8\)~%
 \(string-trim-whitespace  \"~C~C~C~C6 8 \" :start 0 :end 6)~% 
\(string-trim-whitespace \" ~C~C~C~Ctrim around me~C ~C\"\)~%
:SEE-ALSO `cl:string-trim', `cl:string-left-trim', `cl:string-right-trim',
`mon:whitespace-char', `mon:*whitespace-chars*', `mon:substring',
`mon:string-remove-backslashes', `mon:string-split-newline',
`mon:string-substitute', `mon:string-split-multi', `mon:string-split-spaces',
`mon:string-upto-char', `mon:string-split-on-chars'.~%▶▶▶"
  ,@(mapcar #'code-char '(9 12 11 160 9 12 11 160 9 12 11 160 10 13))))

(fundoc 'string-no-upper-p
  "Does STRING contain no uppercase characters.~%~@
:EXAMPLE~%
 \(string-no-upper-p \"abcd\"\)~%
 \(string-no-upper-p \"abcD\"\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'string-longest-common-prefix
        "Return the longest string that is a common prefix of STRINGS.~%~@
STRINGS is a list of strings or nil.~%~@
When STRINGS is null return \"\".
When optional arg NULL-AS-NIL is non-nil and STRINGS is null return nil.~%~@
:EXAMPLE~%
 \(string-longest-common-prefix '\(\"abc\" \"abcd\" \"abcdefg\" \"abcdefgh\"\)\)~%
 \(string-longest-common-prefix '\(\"abqe\" \"abqef\" \"abqcd\"\)\)~%
 \(string-longest-common-prefix nil\)~%
 \(string-longest-common-prefix nil t\)~%~@
;; Following fails succesfully:~%
 \(string-longest-common-prefix '\(\"abqe\" . \"abqef\")\)~%~@
:SEE-ALSO `mon:substring', `mon:string-starts-with', `mon:string-begins',
`mon:string-upto-char', `mon:string-or-symbol-first-char'.~%▶▶▶")

(fundoc 'string-begins
  "Return non-nil if STR begin with PUTATIVE-START.~%~@
:EXAMPLE~%
 \(string-begins \"abcd\" \"ab\"\)~%
 \(string-begins \"abcd\" \"cd\"\)~%
 \(string-begins \"abcd\" \"abcd\"\)~%~@
:SEE-ALSO `mon:substring', `mon:string-longest-common-prefix'.~%▶▶▶")

(fundoc 'string-split-spaces
"Return LINE delimited by chars of type `mon:whitespace-char' as list of strings.~%~@
When optional arg W-LAST-WSPC is non-nil tail of list is the -1 Nth elt without
the trailing whitespace-chars trimmed.~%~@
:EXAMPLE~%
 \(string-split-spaces \" \"\)~%
 \(string-split-spaces \"\"\)~%
 \(string-split-spaces \"\"\ t)~%
 \(string-split-spaces \" 1sisis \"\)~%
 \(string-split-spaces \" 1sisis \" t\)~%
 \(string-split-spaces \"1sisis    2sisis 3sisis    \"\)~%
 \(string-split-spaces \"1sisis    2sisis 3sisis    \" t\)~%~@
:SEE-ALSO `mon:string-split-multi', `mon:string-split-on-chars',
`mon:string-split-newline', `mon:string-trim-whitespace',
`mon:whitespace-char-p', `mon:string-substitute'.~%▶▶▶")

(fundoc 'string-coerce-from
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'string-seqs-convert-chars-if
        "Destrutively modify STRING-SEQ by replacing characterp elts with with string equivalent.~%~@
STRING-SEQ is a proper list satisfying `mon:each-a-sequence-proper-or-character'.~%~@
:EXAMPLE~%
 \(string-seqs-convert-chars-if '\(\"a\" #\\b #\\c #\(#\\d #\\e\) #\\f\)\)~%
 \(string-seqs-convert-chars-if '\(\"a\" nil \"b\" nil \"c\" #\(#\\d #\\e\) \"f\"\)\)~%
 \(string-seqs-convert-chars-if nil\)~%
:SEE-ALSO `mon:concat', `mon:mapconcat', `mon:string-coerce-from',
`mon:string-convert-tabs', `mon:string-substitute', `mon:make-string*',
`mon:make-string-adjustable' .~%▶▶▶")

(fundoc 'string-convert-tabs
"Convert all occurences of #\Tab char in TABBY-STR to their equivalent in #\\Space chars.~%~@
When optional arg OMIT-LAST-NEWLINE is non-nil if last char of TABBY-STR is
#\\Newline it is elided.~%~@
:EXAMPLE~%
 \(string-convert-tabs \"         string bag  \"\)~%
 \(string-convert-tabs \"         string bag 
        	 \" t\)~%~@
:SEE-ALSO `mon:string-split-multi', `mon:string-split-on-chars',
`mon:string-split-newline', `mon:string-underscore-to-dash',
`mon:string-trim-whitespace', `mon:whitespace-char-p',
`mon:string-substitute'.~%▶▶▶")

(fundoc 'string-first-char
 "Return first character in STRING.~%~@
STRING should satisfy `mon:simple-string-or-null-p', signal an error if not.~%~@
When STRING is null or `mon:simple-string-empty-p' return nil.~%~@
:EXAMPLE~%
 \(string-first-char \"bubba\"\)~%
 \(string-first-char \"\"\)~%
 \(string-first-char nil\)~%~@
:SEE-ALSO `mon:string-or-symbol-first-char'.~%▶▶▶")

(fundoc 'string-or-symbol-first-char
 "Return first character in STRING-OR-SYMBOL.~%~@
STRING-OR-SYMBOL should satisfy either `cl:symbolp' or
`mon:simple-string-or-null-p', signal an error if not.~%~@
Return a boolean when value of STRING-OR-SYMBOL is any of the following:~%
 - T \(return T\);
 - null \(return nil\);
 - `cl:stringp' and `mon:simple-string-empty-p' \(return nil\);
 - `cl:stringp' and `cl:string-equal' \"t\" or \"T\", \(return T\);
 - `cl:stringp' and `cl:string-not-equal' \"nil\" or \"NIL\" \(return nil\);
 - `cl:symbolp' with a `cl:symbol-name' which is `mon:simple-string-empty-p';~%~@
:EXAMPLE~%
 \(string-or-symbol-first-char 'bubba\)~%
 \(string-or-symbol-first-char \"bubba\"\)~%
 \(string-or-symbol-first-char \"\"\)~%
 \(string-or-symbol-first-char t\)~%
 \(string-or-symbol-first-char \"T\"\)~%
 \(string-or-symbol-first-char nil\)~%
 \(string-or-symbol-first-char \"nil\"\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'string-is-nil-like
"Whether STRING has nil like qualities.~%~@
STRING is a simple-string or null. Signal an error if not.~%~@
Return non-nil if STRING satisfies any of the following:
 - It is the empty-string;
 - If every char is `mon:whitespace-char-p';
 - If STRING is string-equal \"nil\";
 - If string is null;~%~@
:EXAMPLE~%
 \(string-is-nil-like \"\"\)~%
 \(string-is-nil-like \"   \"\)~%
 \(string-is-nil-like \"NIL\"\)~%
 \(string-is-nil-like \"nil\"\)~%
 \(string-is-nil-like \"nil   \"\)~%
 \(string-is-nil-like nil\)~%
 \(string-is-nil-like \(\)\)~%~@
:SEE-ALSO `mon:string-trim-whitespace'.~%▶▶▶")

(fundoc 'string-invert-case
"Invert the CASE of STRING-TO-INVERT.~%~@
Keyword CASE is as keyword per return value of `cl:readtable-case' e.g. one of:~%
 :upcase :downcase :invert :preserve~%
:EXAMPLE~%
 \(string-invert-case \"bubba\" :case :preserve\)~%
 \(string-invert-case \"BÜbBá\" :case :invert\)~%
 \(string-invert-case \"BuBbá\" :case :downcase\)~%
 \(string-invert-case \"BüBbá\" :case :upcase\)~%~@
:SEE-ALSO `mon:string-for-readtable-case', `mon:char-for-readtable-case',
`mon:char-invert-case-maybe'.~%▶▶▶")

(fundoc 'string-for-readtable-case
        "Return CASE-FROB-STRING according to the `cl:readtable-case' of READTABLE~%~@
When optional arg READTABLE is provided it should satisfy `cl:readtable', signal
an error if not. When ommitted default to value of `cl:*readtable*'.~%~@
:EXAMPLE~%
 \(loop 
   :with case-str  = \"WILL i bE cASe FrobBeD\"
   :with cases     = \(list :upcase :downcase :preserve :invert\)
   :with read-out  = \(make-string-output-stream\)
   :with new-rdtbl = \(copy-readtable *readtable*\)
   :initially \(format read-out \"Initially with: ~~A~~%\" case-str\)
   :for case :in cases
   :do \(setf \(readtable-case new-rdtbl\) case\)
   \(format read-out \"read: ~~A read-table-case ~~S~~%\" 
           \(string-for-readtable-case case-str new-rdtbl\) case\)
   :finally \(unwind-protect
                 \(return \(get-output-stream-string read-out\)\)
              \(close read-out\)\)\)~%~@
:SEE-ALSO `mon:string-invert-case', `mon:read-symbol-name-preserving-case',
`mon:read-symbol-name-preserving-case-if', `mon:char-for-readtable-case',
`mon:char-invert-case-maybe'.~%▶▶▶")

(fundoc 'string-split-on-column
"Split STRING on COLUMN boundaries.~%~@
:EXAMPLE~%
 \(string-split-on-column 
  \(format nil \"Hello World~%How do yo do?~%Comment ça va?~%\"\) 8\)~%~@
:SEE-ALSO `mon:string-split-newline', `mon:string-split-multi'.~%▶▶▶")

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
