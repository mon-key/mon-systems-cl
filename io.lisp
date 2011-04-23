;;; :FILE mon-systems/io.lisp
;;; ==============================

;;; ==============================
;;
;; :TODO asdf:run-shell-command
;;
;;; ==============================


(in-package #:mon)
;; *package*

(defun toggle-print-case  ()
  (case *print-case* 
    (:downcase (setf *print-case* ':upcase)) 
    (:upcase   (setf *print-case* ':downcase))
    (:capitalize 
     (warn ":FUNCTION `toggle-print-case' -- case is :capitalize, won't toggle"))))

(defun eof-p (stream)
  (null (peek-char nil stream nil nil)))

;; :SOURCE xit/cl-utilities/cl-utilities.lisp
(defun query-string (query-string &key special default (test nil) 
		     (trim-chars '(#\Space #\Tab)));; '(#\Space #\Tab #\Newline)))					
  (let ((input nil))
    (loop 
      (setq input
	(progn (if default
		   (format *query-io* "~&~a (~a) " query-string default)
		 (format *query-io* "~&~a " query-string))
	       (force-output *query-io*)
	       (string-trim trim-chars
			    (read-line *query-io*))))
      (when (and default (string= input ""))
	(setq input default))
      (when (or (and test (funcall test input))
		(string/= input ""))
	(when special
	  (set special input))
	(return
	  input)))))

;;; :SOURCE (URL `http://paste.lisp.org/+2K9U') :WAS `read-next-char'
;;; :PASTE-NUMBER 119586: trivial-get-char :PASTED-BY redline6561 :DATE 2011-02-11
;;; :SEE (info "(libc)Terminal Modes")
(defun read-next-term-char (&optional (stream *query-io*))
  #+sbcl
  ;; Thanks to Thomas F. Burdick
  (let ((old (sb-posix:tcgetattr 0))
        (new (sb-posix:tcgetattr 0))
        (bits (logior sb-posix:icanon sb-posix:echo sb-posix:echoe
                      sb-posix:echok sb-posix:echonl)))
    (unwind-protect
         (progn
           (setf (sb-posix:termios-lflag new)
                 (logandc2 (sb-posix:termios-lflag old) bits)
                 (aref (sb-posix:termios-cc new) sb-posix:vmin) 1
                 (aref (sb-posix:termios-cc new) sb-posix:vtime) 0)
           (sb-posix:tcsetattr 0 sb-posix:tcsadrain new)
           (read-char stream))
      (sb-posix:tcsetattr 0 sb-posix:tcsadrain old)))

  #+ccl
  (rlet ((old :termios)
         (new :termios))
        (let ((bits (logior #$ICANON #$ECHO #$ECHOE #$ECHOK #$ECHONL))
              (cc-array (pref new :termios.c_cc)))
          (unwind-protect
               (progn
                 ; Ensure that we actually store the original value.
                 (#_tcgetattr 0 old)
                 (setf (pref new :termios.c_lflag)
                       (logandc2 (pref old :termios.c_lflag) bits)
                       (paref cc-array (:array :char) #$VMIN) 1
                       (paref cc-array (:array :char) #$VTIME) 0)
                 (#_tcsetattr 0 #$TCSADRAIN new)
                 (read-char stream))
            (#_tcsetattr 0 #$TCSADRAIN old))))

  #+ecl
  ()

  #+cmucl
  ;; Thanks to Rob Warnock
  ;; FIXME: Why does it return immediately?
  (alien:with-alien ((old (alien:struct unix:termios))
                     (new (alien:struct unix:termios)))
    (let ((e0 (unix:unix-tcgetattr 0 old))
          (e1 (unix:unix-tcgetattr 0 new))
          (bits (logior unix:tty-icanon unix:tty-echo unix:tty-echoe
                        unix:tty-echok unix:tty-echonl)))
      (declare (ignorable e0 e1))
      (unwind-protect
           (progn
             (setf (alien:slot new 'unix:c-lflag)
                   (logandc2 (alien:slot old 'unix:c-lflag) bits)
                   (alien:deref (alien:slot new 'unix:c-cc) unix:vmin) 1
                   (alien:deref (alien:slot new 'unix:c-cc) unix:vtime) 0)
             (unix:unix-tcsetattr 0 unix:tcsadrain new)
             (read-char stream))
        (unix:unix-tcsetattr 0 unix:tcsadrain old))))

  #+clisp
  ;; Thanks to Pascal Bourguignon
  (ext:with-keyboard
    (system::input-character-char
     (read-char ext:*keyboard-input*))))

(defun read-new-random-state-seed ()
  ;; Read an integer from #P"/dev/urandom" suitable for passing to sb-ext:seed-random-state.
  ;; :EXAMPLE (sb-ext:seed-random-state (read-new-random-state-seed))
  ;;  
  ;; (let ((probe-urandom (and (probe-file #P"/dev/urandom"))))
  ;; (or (and probe-urandom)
  ;;     (error (make-condition 'simple-error
  ;;                            ;;  'file-error :pathname #P"/dev/urandom")
  ;;                            :format-control "did not find file #P\"/dev/urandom\"")))
  (with-open-file (r #P"/dev/urandom" 
                     :element-type '(unsigned-byte 32)
                     :direction :input 
                     :if-does-not-exist :error)
    (let ((a (make-array '(1) :element-type '(unsigned-byte 32))))
      (assert (= 1 (read-sequence a r)))
      (aref a 0))))

;;; ==============================
;; stream-fd
;; :ANNOTATION-NUMBER 1
;; :PASTED-BY stassats 2011-02-12
;; :SEE (URL `http://paste.lisp.org/+2KAO/1')
;; :WAS `stream-fd'
;; :SEE `sb-impl::*available-buffers*' 
;; :SEE :FILE sbcl/src/code/fd-stream.lisp
;; :SEE `sb-sys:fd-stream'/`external-format' structures
;; :SEE `make-fd-stream' `sb-sys:fd-stream-fd', `beep'
;; sb-sys:*stderr*, sb-sys:*stdout* sb-sys:*stdin* , sb-sys:*tty*
;; (find-all-symbols "STREAM-FILE-DESCRIPTOR")
(defun stream-file-descriptor (stream)
  ;; (stream-file-descriptor sb-sys:*stderr*)
  ;; (stream-file-descriptor sb-sys:*stdin*)
  ;; (stream-file-descriptor sb-sys:*stdout*)
  ;; (stream-file-descriptor sb-sys:*tty*)
  (typecase stream
    (file-stream (sb-sys:fd-stream-fd stream))
    (synonym-stream (stream-file-descriptor (symbol-value (synonym-stream-symbol stream))))
    (stream nil)))

;; :SOURCE quicklisp/quicklisp/utils.lisp
(defun press-enter-to-continue ()
  (format *query-io* "~&Press Enter to continue.~%")
  (let ((result (read-line *query-io*)))
    (zerop (length result))))

;;; :SOURCE D. Mcdermott ytools/base.lisp
(defun print-spaces (number stream)
    (dotimes (n number) 
      (write-char #\Space stream)))

;; :SOURCE sbcl/src/compiler/disassem.lisp  :WAS `princ16'
(defun princ-16 (value stream)
  (write value :stream stream :radix t :base 16 :escape nil))

;;; :SOURCE garnet-20030525/kr/kr.lisp
(defun indent-by (indent)
  (dotimes (i indent (* indent 3))
    (write-string "   ")))

;;; ==============================
;;; LiCE handles a few escape codes like GNU Emacs
;;;  (set-macro-character #\" #'read-string-with-escapes)
;;; 
;;; :TODO Needs an alias `read-string-with-escapes' -> `string-read-with-escapes'
;;; :COURTESY lice/src/global.lisp
(defun read-string-with-escapes (stream close)
  (with-output-to-string (out)
    (do ((char (read-char stream nil :eof) (read-char stream nil :eof)))
        ((or (eq char :eof) (char= char close))
         (if (eq char :eof)
             (error 'end-of-file :stream stream)))
      (when (char= char #\\)
        (setq char (read-char stream nil :eof))
        (case char
          (:eof (error 'end-of-file :stream stream))
          (#\n (setq char #\Newline))
          (#\t (setq char #\Tab))
          (#\r (setq char #\Return))
          (#\f (setq char #\Page))
          (#\v (setq char #\Vt))
	  ;; #\NULL ;; ASCII (0, #o0, #x0)
	  ;; (0, #o0, #x0)
	  ;; #\NUL  ;; (char-name #\NULL) (char-code #\NULL) (princ #\NULL) (code-char 0)
	  ))
        (write-char char out))))


;;; ==============================
;; (let ((s (make-string-output-stream)))
;;   (write-string (format nil "~S" (output-stream-p s)) s)
;;   (unwind-protect 
;;        (get-output-stream-string s)
;;     (close s)))
;;
;; (with-output-to-string (s)
;;   (format s "output-stream-p: ~S~%open-stream-p: ~S"
;;           (output-stream-p s) (open-stream-p s)))
;;
;; (let ((out '())) (write-string "writing to \"out\"" out))
;; (let ((out t)) (write-string "writing to \"out\"" out))
;;; ==============================
;; (booleanp nil)

(defun open-stream-output-stream-p (stream &key allow-booleans allow-fill-pointer-strings w-error)
  (if (or (and allow-booleans (booleanp stream))
          (and allow-fill-pointer-strings 
               (string-with-fill-pointer-p stream))
          (and (streamp stream)
               (open-stream-p (the stream stream))
               (output-stream-p (the stream stream))))
      (if w-error 
          (values (cond ((null stream) nil)
                        ;; ((eql stream t) t)
                        ((eq stream t) t)
                        ((stringp stream) (the string stream))
                        (t (the stream stream)))
                  t)
          t)
      (and w-error
           (open-stream-output-stream-error stream
                                            :w-sym 'open-stream-output-stream-p
                                            :w-type 'function
                                            :w-obj-locus "STREAM"
                                            :expected-type ;;(or (and allow-booleans 'stream-or-boolean) 'stream)
                                            (let ((1or 
                                                   (delete-if #'null `(,(or (and allow-booleans 'stream-or-boolean) 'stream) 
                                                                        ,(and allow-fill-pointer-strings 'string-with-fill-pointer)))))
                                              (case (length 1or)
                                                (1 1or)
                                                (2 `(or  ,@1or))))
                                            :signal-or-only nil))))

;; (delete-if #'null
;;            `(or ,(or (and allow-booleans 'stream-or-boolean) 'stream) 
;;                 ,(and allow-fill-pointer-strings 'string-with-fill-pointer)))





;;; ==============================
;;; :IO-DOCUMENTATION
;;; ==============================

(fundoc 'toggle-print-case
 "Toggle `*print-case*'.~% 
When :downcase set it :upcase.~%~@
When :upcase set it :upcase.~%~@
When :capitalize signal a warning and do nothing.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `cl:with-standard-io-syntax'.~%►►►")

(fundoc 'query-string
		"Query user with QUERY-STRING.~%~@
Keyword :SPECIAL when non-nil it is a symbol which is bound to user input.~%~@
Keyword :DEFAULT is the default if user doen't provide a value.~%~@
Keyword :TEST is a predicate.~%~@
Keyword :TRIM-CHARS are removed when read and will reprompt.
:TRIM-CHARS defaults to: (#\Space #\Tab #\Newline).~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:NOTE On SBCL passing the following form will prevent successfull entry of
carriage returns, e.g. character #\\RETURN.~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'print-spaces
"Print NUMBER spaces to STREAM as if by `write-char'~%~@
:EXAMPLE~%~@
  { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'read-string-with-escapes
"Read a string containing C style character escapes and convert to CL style.~%~@
STREAM is a stream to read from.~%~@
CLOSE is a character which when encountered will close STREAM.~%~@
Signal an error when :of is encountered.~%~@
Following table maps the corresponding conversions:~% 
- \\n -> #\\NEWLINE     LINE FEED \(LF\) \(10, #o12, #xa\)
- \\f -> #\\PAGE        FORM FEED \(FF\) \(12, #o14, #xc\)
- \\t -> #\\TAB         CHARACTER TABULATION \(9, #o11, #x9\)
- \\r -> #\\RETURN      CARRIAGE RETURN \(13, #o15, #xd\)
- \\v -> #\\VT          LINE TABULATION \(11, #o13, #xb\)~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `*whitespace-chars*', `whitespace-char-p', `ascii-downcase',
`ascii-downcase', `ascii-char-p', `ascii-string-p', `sb-int:bell-char-code',
`sb-int:backspace-char-code', `sb-int:tab-char-code',
`sb-int:line-feed-char-code', `sb-int:form-feed-char-code',
`sb-int:return-char-code', `sb-int:escape-char-code',
`sb-int:rubout-char-code'.~%►►►")

(fundoc 'eof-p
"Return non-nil if STREAM has no more data left in it to be read.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%►►►")

(fundoc 'read-next-term-char
"Read next char from stream.~%~@
Like `cl:read-char' but for unbuffered termios \(read-char\) calls.%~@
Optional arge STREAM defaults to `*query-io*'.%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE \(info \"\(libc\)Terminal Modes\"\)
:SEE-ALSO `mon:query-string', `cl:read-char', `sb-posix:tcgetattr',
`sb-posix:tcsetattr', `sb-posix:tcsadrain', `sb-posix:termios-lflag',
`sb-posix:termios-cc', `sb-posix:icanon', `sb-posix:echo', `sb-posix:echoe',
`sb-posix:echok', `sb-posix:echonl', `sb-posix:vmin', `sb-posix:vtime'.~%►►►")

(fundoc 'open-stream-output-stream-p
"Return true if STREAM satisfies both `cl:open-stream-p' and `cl:output-stream-p'.~%~@
Keyword ALLOW-BOOLEANS when non-nil indicates that T and NIL are allowed as
valid values for STREAM i.e. for use as arguments to `cl:format'~%~@
Keyword ALLOW-FILL-POINTER-STRINGS when non-nil indicates that strings
satisfying `mon:string-with-fill-pointer-p' are considered streams.~%~@
Keyword W-ERROR when non-nil indicates that the argument STREAM should be
returned when the the above constraints are met and if not signal an error of
type `mon:open-stream-output-stream-error'.~%~@
:EXAMPLE~%
 \(open-stream-output-stream-p *standard-output* :allow-booleans nil :w-error t\)~%
 \(open-stream-output-stream-p t :allow-booleans t :w-error t\)~%
 \(open-stream-output-stream-p nil :allow-booleans t :w-error t\)~%
 \(open-stream-output-stream-p *standard-input* :allow-booleans nil :w-error t\)~%
 \(open-stream-output-stream-p t :allow-booleans nil\)~%
 \(open-stream-output-stream-p \"bubba\" :w-error t\)~%
 \(open-stream-output-stream-p \"bubba\" :allow-booleans t :w-error t\)~%
 \(open-stream-output-stream-p \"string\" :allow-booleans t :allow-fill-pointer-strings t :w-error t\)~%
 \(open-stream-output-stream-p \"string\" :allow-fill-pointer-strings t :w-error t\)~%
 \(let \(\(fp-strm \(make-array 6 
                            :element-type 'character 
                            :initial-contents \"string\" 
                            :fill-pointer 6\)\)\)
   \(open-stream-output-stream-p fp-strm :w-error t\)\)~%
 \(let \(\(fp-strm \(make-array 6 
                            :element-type 'character 
                            :initial-contents \"string\" 
                            :fill-pointer 6\)\)\)
   \(multiple-value-bind \(str-strm fp-strm-p\) 
       \(open-stream-output-stream-p fp-strm 
                                    :allow-fill-pointer-strings t 
                                    :w-error t\)
     \(list str-strm fp-strm-p \(type-of str-strm\)\)\)\)~%~@
:SEE-ALSO `mon:stream-or-boolean', `cl:streamp', `cl:open-stream-p',
`cl:output-stream-p' `mon:open-stream-output-stream-error-report'.~%►►►")


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF
