;;; :FILE-CREATED <Timestamp: #{2011-01-19T12:38:56-05:00Z}#{11033} - by MON>
;;; :FILE file-io.lisp
;;; ==============================

;;; ==============================
;;
;; `alexandria:read-file-into-string',  `alexandria:write-string-into-file',
;; `alexandria:with-open-file*',  `alexandria:copy-stream',
;; `alexandria:with-input-from-file', `alexandria:with-output-to-file',
;;
;;; ==============================


(in-package #:mon)
;; *package*

(defmacro with-opened-file ((stream filespec &rest options) &body body)
  (with-gensyms (abort-on-close?)
    `(let ((,stream (with-error-handling ((open ,filespec ,@options)
                                          (:conditions file-error))))
           (,abort-on-close? 't))
       (unwind-protect
            (multiple-value-prog1 
                (progn ,@body)
              (setq ,abort-on-close? nil))
         (when (streamp ,stream) 
           (close ,stream :abort ,abort-on-close?))))))

(defmacro with-temp-file ((stream-name file-name) &body body)
  `(alexandria:with-output-to-file  (,stream-name ,file-name 
                                                  :if-exists :supersede 
                                                  ;; :ADDED
                                                  :if-does-not-exist :create)
     ,@body))

;;; :SOURCE texinfo-docstrings/colorize.lisp
(defmacro with-each-stream-line ((var stream) &body body)
  (let ((eof (gensym))
        (eof-value (gensym))
        (strm (gensym)))
    `(let ((,strm ,stream)
           (,eof ',eof-value))
       (do ((,var (read-line ,strm nil ,eof) (read-line ,strm nil ,eof)))
           ((eql ,var ,eof))
         ,@body))))

;;; ==============================
;; :SOURCE (URL `git://github.com/Hexstream/com.hexstreamsoft.lib.git')
;; :FILE lib.lisp
;;
;; :NOTE Macro expansion of `alexandria:with-unique-names' vs. `sb-int:with-unique-names'
;; alexandria:with-unique-names
;; (LET ((SHARED (GENSYM "SHARED")))
;;   `(FLET ((,SHARED (,VAR)
;;             ,@BODY))
;;      (LET ((,VAR ,STRING-OR-STREAM))
;;        (IF ,VAR
;;            (,SHARED ,VAR)
;;            (WITH-OUTPUT-TO-STRING (,VAR) (,SHARED ,VAR))))))
;;
;; sb-int:with-unique-names
;; (LET ((SHARED (SB-INT:BLOCK-GENSYM "SHARED")))
;;   `(FLET ((,SHARED (,VAR)
;;             ,@BODY))
;;      (LET ((,VAR ,STRING-OR-STREAM))
;;        (IF ,VAR
;;            (,SHARED ,VAR)
;;            (WITH-OUTPUT-TO-STRING (,VAR) (,SHARED ,VAR))))))
;; 
;; (with-output-to-string-or-stream (i) (princ "bubba" i))
;; (with-output-to-string-or-stream (i) (princ "bubba" i) (princ "bubba2" i))
(defmacro with-output-to-string-or-stream ((var &optional (string-or-stream var)) &body body)
  ;; (macroexpand-1 '(with-output-to-string-or-stream (i) "bubba"))
  (#-sbcl alexandria:with-unique-names
   #+sbcl sb-int:with-unique-names 
   (shared)
   `(flet ((,shared (,var)
             ,@body))
      (let ((,var ,string-or-stream))
        (if ,var
            (,shared ,var)
            (with-output-to-string (,var)
              (,shared ,var)))))))

;;; ==============================
;; :SOURCE (URL `git://github.com/ivan4th/i4-diet-utils.git') 
;; :FILE i4-diet-utils.lisp :WAS `with-input-file'
;; :NOTE Requires flexi-streams `make-flexi-stream'
(defmacro with-input-file ((file-var file &key (external-format :utf-8)) &body body)
  (alexandria:once-only (file external-format) ; keep order of evaluation
    (let ((in (gensym)))
      `(with-open-file (,in ,file 
			    :direction :input
			    :element-type '(unsigned-byte 8))
         (let ((,file-var 
		(flexi-streams:make-flexi-stream ,in
						 :external-format ,external-format)))
           ,@body)))))

;; quickproject/quickproject.lisp
(defmacro with-new-file ((stream file) &body body)
  `(with-open-file (,stream ,file
                            :direction :output
                            :if-exists :error
                            ;; :ADDED
                            :if-does-not-exist :create)
     ;;(let ((*print-case* :downcase))
     ,@body))


(defmacro with-new-file-renaming-old ((stream file) &body body)
  `(with-open-file (,stream ,file
                            :direction :output
                            :if-exists :rename
                            :if-does-not-exist :create)
     ;;(let ((*print-case* :downcase))
     ,@body))

;;
;; :SOURCE (URL `git://github.com/ivan4th/i4-diet-utils.git') 
;; :FILE i4-diet-utils.lisp :WAS `with-overwrite'
;; :NOTE Requires flexi-streams `make-flexi-stream'
(defmacro with-file-overwritten ((file-var file &key (external-format :utf-8)) &body body)
  (alexandria:once-only (file external-format) ; keep order of evaluation
    (let ((out (gensym)))
      `(with-open-file (,out ,file :direction :output
                             :if-does-not-exist :create
                             :if-exists :supersede
                             :element-type '(unsigned-byte 8))
         (let ((,file-var (flexi-streams:make-flexi-stream ,out :external-format ,external-format)))
           ,@body)))))
;;
;; :SOURCE (URL `git://github.com/ivan4th/i4-diet-utils.git') 
;; :FILE i4-diet-utils.lisp :WAS `write-file'
(defun write-file (string file &key (external-format :utf-8))
  (with-file-overwritten (out file :external-format external-format)
    (write-string string out)))
;;
;; :SOURCE (URL `git://github.com/ivan4th/i4-diet-utils.git') 
;; :FILE i4-diet-utils.lisp :WAS `snarf-file'
(defun read-file-to-string (file &key (external-format :utf-8))
  (with-output-to-string (out)
    (with-input-file (in file :external-format external-format)
      (loop
	 :for read = (read-line in nil nil)
	 :while read
	 :do (princ read out)
	 :do (terpri out)))))

;; :SOURCE asdf.lisp :WAS `read-file-forms'
(defun read-file-forms (file)
  (with-open-file (in file)
    (loop 
       :with eof = (list nil)
       :for form = (read in nil eof)
       :until (eq form eof)
       :collect form)))

;;; ==============================
;; :SOURCE restas-wiki/src/ storage.lisp :WAS `write-string-into-gzip-file'
;; :WAS (defun write-string-into-gzip-file (string path)
;;        (with-open-file (ostream
;;                         path
;;                         :element-type '(unsigned-byte 8)
;;                         :direction :output
;;                         :if-exists :supersede)
;;          (salza2:with-compressor (compressor 'salza2:gzip-compressor
;;                                              :callback (salza2:make-stream-output-callback ostream))
;;            (salza2:compress-octet-vector (babel:string-to-octets string :encoding :utf-8)
;;                                          compressor))))
;;; ==============================
(defun write-string-to-file-gzip (string gzip-output-pathname &optional if-exists-rename)
  (declare (type string string))
  ;; (type pathname path))
  (let ((rtn-path gzip-output-pathname) 
        (cnt-byte 'nil))
    (setf cnt-byte
          (with-open-file (ostream
                           rtn-path
                           :element-type '(unsigned-byte 8)
                           :direction :output
                           :if-exists (or (and if-exists-rename :rename) :supersede)
                           :if-does-not-exist :create)
            (salza2:with-compressor (compressor 'salza2:gzip-compressor
                                                :callback (salza2:make-stream-output-callback ostream))
              ;; :WAS (salza2:compress-octet-vector (babel:string-to-octets string :encoding :utf-8) compressor) 
              #+sbcl (salza2:compress-octet-vector (sb-ext:string-to-octets string :external-format :utf-8) compressor)
              #-sbcl (salza2:compress-octet-vector (flex:string-to-octets   string :external-format :utf-8) compressor))))
    (if cnt-byte
        (values rtn-path cnt-byte)
        (values nil 'non-local-exit))))

;; :NOTE :SEE Nathan Froyd's archive for working with cpio/tar data
;; (URL `https://github.com/froydnj/archive.git')
;; (archive::create-tar-file  <PATHNAME> '(<FILELIST>))
;;; ==============================
;; :NOTE Rudiments of a $> tar cvzf file.tgz file
;; (defun gzip-file-tgz (tar-pathname file &key (if-tar-does-not-exist :create)
;;                                              (tarball-extension { tgz | tar.gz }
;; (archive::create-tar-file 
;; (let ((archiving (make-pathname <TAR-PATHNAME> (...) ))
;;       (file-to-compress <FILE>))
;; (progn
;;   (archive::create-tar-file archiving '(<FILE>))
;;   (salza2:gzip-file  archiving   ... 
;;    (merge-pathnames arhiving ... tarball-extension))

(defun gzip-file-and-delete-source (file)
  (let ((file-gz (concatenate 'string (namestring file) ".gz"))
        (delete-rtn '()))
    ;; gzip-file returns a pathaname
    (setf file-gz (salza2:gzip-file file file-gz))
    (setf delete-rtn (and (delete-file file) (pathname file)))
    (values file-gz delete-rtn)))

;;; ==============================
;; :NOTE `mon:pathname-file-list-if' doesn't catch symlinks when non-SBCL!
#+sbcl 
(defun gzip-files-and-delete-source (files-list) 
  (let ((cln-dirs-syms (pathname-file-list-if files-list :as-pathnames nil)))
    (flet  ((gzip-file-and-delete (in-file)
              (declare (string in-file))
              (let ((file-gz (concatenate 'string in-file ".gz"))
                    (delete-rtn '()))
                (setf file-gz (salza2:gzip-file in-file file-gz))
                (setf delete-rtn (and (delete-file in-file) (pathname in-file)))
                (list file-gz delete-rtn))))
      (loop 
         for file in cln-dirs-syms collect (gzip-file-and-delete file)))))

;;; ==============================
;; :SOURCE restas-wiki/src/storage.lisp :WAS `read-gzip-file-into-string'
;; :WAS (defun read-gzip-file-into-string (path)
;;        (babel:octets-to-string (with-open-file (in path :element-type '(unsigned-byte 8))
;;                                  (zip:skip-gzip-header in)
;;                                  (flex:with-output-to-sequence (out)
;;                                    (zip:inflate in out)))
;;                                :encoding :utf-8))
;;; ==============================
(defun read-file-gunzip-to-string (gzip-pathname)
  #-sbcl (flex:octets-to-string
          (with-open-file (in gzip-pathname :element-type '(unsigned-byte 8) :direction :input)
            (flex:with-output-to-sequence (out) (chipz:decompress out 'chipz:gzip in)))
          :external-format :utf-8)
  #+sbcl (sb-ext:octets-to-string
          (with-open-file (in gzip-pathname :element-type '(unsigned-byte 8) :direction :input)
            (flex:with-output-to-sequence (out) (chipz:decompress out 'chipz:gzip in)))
          :external-format :utf-8))

(defun read-file-gzip-to-gunzip-file (gzip-input-pathname gunzip-output-pathname &optional if-exists-rename)
  (with-open-file (gzstream gzip-input-pathname
                            :direction :input
                            :element-type '(unsigned-byte 8))
    (with-open-file (gunz-stream gunzip-output-pathname
                                 :direction :output
                                 :element-type '(unsigned-byte 8)
                                 :if-exists (or (and if-exists-rename :rename) :supersede))
      (chipz:decompress gunz-stream 'chipz:gzip gzstream)
      gunzip-output-pathname)))



;;; ==============================
;;; :FILE-IO-DOCUMENTATION
;;; ==============================

(fundoc 'with-temp-file
  "Create a new stream named STREAM-NAME, evaluate body there, writing stream to file.~%~@
The value returned is the value of the last form in body.~%~@
If FILE-NAME exists its contents are overwritten as if by :if-exists :supersede
:EXAMPLE~%
 \(let \(\(mp \(make-pathname :name \"bubba\" 
			  :defaults *default-pathname-defaults*\)\)\)
   \(with-temp-file \(tf mp\)
     \(princ \"bubba\" tf\)\)\)~%~@
:EMACS-LISP-COMPAT Similiar to `with-temp-buffer'.~%~@
:SEE-ALSO `with-each-stream-line', `with-opened-file', 
`alexandria:write-string-into-file', `alexandria:read-file-into-string',
`alexandria:with-input-from-file', `alexandria:with-output-to-file'.~%▶▶▶")

(fundoc 'with-opened-file
"Like WITH-OPEN-FILE, but set STREAM to nil if file-error is signaled opening FILESPEC.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'with-each-stream-line
"<DOCSTR>~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `mon:with-temp-file', `mon:with-opened-file', 
`alexandria:write-string-into-file', `alexandria:read-file-into-string',
`alexandria:with-input-from-file', `alexandria:with-output-to-file'.~%▶▶▶")
 
(fundoc 'with-output-to-string-or-stream
"Like `cl:with-output-to-string' but optional arg may be a string or stream.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'with-file-overwritten
"Evaluate BODY as if by `cl:with-open-file' with output file sa flexi-stream.~%~@
FILE-VAR is a variable bound to a file output stream, it has dynamic extent which
ends when BODY form is exited.~%~@
FILE is a pathname to write file to. If file exists it is superseded, it it does
not exist it is created.~%~@
:EXTERNAL-FORMAT is a format for writing file it defaluts to :utf-8 as per
`flexi-streams:make-flexi-stream'~%~@
:EXAMPLE~%
 \(let \(\(fl \(merge-pathnames 
	   \(make-pathname :directory '\(:relative \"notes\"\)
			  :name \"test\"\)
	   \(pathname-system \"mon\"\)\)\)\)
      \(with-file-overwritten \(v fl\)
	\(format v \(mapconcat #'identity '\(\"a\" \"b\" \"c\" \"d\" \"e\" \"f\"\) \"~~%\"\)\)
	\(loop
	   for byt in '\(0 174 14 13 255\)
	   do \(write-byte byt v\)\)
	\(format v \(mapconcat #'identity '\(\"~%a\" \"b\" \"c\" \"d\" \"e\" \"f\"\) \"~~%\"\)\)\)\)~%~@
:SEE-ALSO `cl:with-open-file'.~%▶▶▶")

(fundoc 'with-new-file
"Like `cl:with-open-file', but output STREAM to FILE which must not exist.~%~@
If file exists signal an error.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'with-new-file-renaming-old
"Like `mon:with-new-file' but \":if-exists\" file then \":new-version\" ~%~@
STREAM is a stream variable as per `with-open-file'.~%
FILE is a filespec pathname designator as per `with-open-file'.~%
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'read-file-to-string
"Return contents of FILE as if by `with-output-to-string'.~%~@
Snarfage occurs linewise with `cl:read-line'ing.~%~@
FILE is a pathname as per `mon:with-input-file'.~%~@
Keyword EXTERNAL-FORMAT is as per `flexi-streams:make-flexi-stream'. 
Default is :utf-8.~%~@
:EXAMPLE~%
 \(let \(\(fl \(merge-pathnames 
	    \(make-pathname :directory '\(:relative \"notes\"\) 
			   :name \"test\"\)
	    \(pathname-system \"mon\"\)\)\)
       \(snarfed nil\)\)
   \(with-file-overwritten \(v fl\)
     \(format v \(mapconcat #'identity 
			  '\(\"a\" \"b\" \"c\" \"d\" \"e\" \"f\"\) 
			  \"~~%\"\)\)\)
   \(prog1 
       \(setf snarfed \(read-file-to-string fl\)\)
     \(delete-file-if-exists fl\)\)\)~%~@
:SEE-ALSO `mon:delete-file-if-exists', `mon:with-file-overwritten',
`mon:with-each-stream-line', `mon:with-opened-file', `mon:write-file'
`mon:with-temp-file', `alexandria:write-string-into-file',
`alexandria:read-file-into-string', `alexandria:with-input-from-file',
`alexandria:with-output-to-file'.~%▶▶▶")

(fundoc 'write-file
"Write STRING to FILE as if by `cl:write-string'.~%~@
Output is as if by `mon:with-file-overwritten'. 
FILE is overwritten if it exists and created if not.~%~@
Keyword EXTERNAL-FORMAT is as per `flexi-streams:make-flexi-stream'.
Default is :utf-8.~%~@
:EXAMPLE~%
 \(let \(\(fl \(merge-pathnames 
	   \(make-pathname :directory '\(:relative \"notes\"\) 
			  :name \"test\"\)
	   \(pathname-system \"mon\"\)\)\)
      \(snarfed 
       \(format nil \(mapconcat #'identity '\(\"a\" \"b\" \"c\" \"d~~%\"\) \"~~%\"\)\)\)\)
  \(setf snarfed \(write-file snarfed fl\)\)
  \(prog1 
      \(setf snarfed \(concat snarfed \(read-file-to-string fl\)\)\)
    \(delete-file-if-exists fl\)\)\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'read-file-forms
"Read forms in FILE as if by `cl:read' with EOF marker as the empty list.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'write-string-to-file-gzip
"Write compressing STRING to GZIP-OUTPUT-PATHNAME.
Return as if by `cl:values':
 - GZIP-OUTPUT-PATHNAME
 - number of bytes written
If a non-local exit \(sans non-recoverable error\) should occur when writing
returned values are: NIL,'NON-LOCAL-EXIT~%~@
STRING is a string it compress to GZIP-OUTPUT-PATHNAME.
STRING is assumed to be in UTF-8 format and it is written as such.~%~@
GZIP-OUTPUT-PATHNAME is a pathname designator to write the compressed string to.~%~@
When optional arg IF-EXISTS-RENAME is non-nil and GZIP-OUTPUT-PATHNAME
already exists the existent file will be rewritten.~%~@
:EXAMPLE~%
 \(write-string-to-file-gzip 
  \(mon-test:make-random-string 300\) 
  \(merge-pathnames \(make-pathname :directory '\(:relative \"tests\"\) 
                                  :name \"test\" 
                                  :type \"gz\"\)\)\)~%~@
 \(write-string-to-file-gzip 
  \(mon-test:make-random-string 300\) 
  \(merge-pathnames \(make-pathname :directory '\(:relative \"tests\"\) 
                                  :name \"test\" 
                                  :type \"gz\"\)\) t\)~%~@
:NOTE SBCL renames this file <FILE>.gz to <FILE>.gz.bak such the following will 
will error when attempt to read backup of existing GZIP-OUTPUT-PATHNAME:
 shell> gunzip
If this is a problem kludgy workarounds include the following:
shell> gunzip -S \"bak\" foo.gz.bak~%
shell> zcat foo.gz.bak~%~@
:SEE-ALSO `write-string-to-file-gzip', `read-file-gunzip-to-string',
`read-file-gzip-to-gunzip-file', `salza2:with-compressor',
`salza2:compress-octet-vector', `salza2:make-stream-output-callback',
`flex:with-output-to-sequence', `chipz:decompress', `sb-ext:octets-to-string',
`sb-ext:string-to-octets', `flex:octets-to-string', `flex:string-to-octets'.~%▶▶▶")

(fundoc 'read-file-gunzip-to-string
"Read the decompress contents of GZIP-PATHNAME to a string.~%~@
Returns a string formatted as UTF-8.~%~@
GZIP-PATHNAME is a pathname-designator for a gzip'd file to decompress to string.~%~@
:EXAMPLE~%
 \(read-file-gunzip-to-string
  \(write-string-to-file-gzip  
   \(mon-test:make-random-string 300\)
   \(merge-pathnames \(make-pathname :directory '\(:relative \"tests\"\) 
                                   :name \"test\" 
                                   :type \"gz\"\)\)\)\)~%~@
:SEE-ALSO `write-string-to-file-gzip', `read-file-gunzip-to-string',
`read-file-gzip-to-gunzip-file', `salza2:with-compressor',
`salza2:compress-octet-vector', `salza2:make-stream-output-callback',
`flex:with-output-to-sequence', `chipz:decompress', `sb-ext:octets-to-string',
`sb-ext:string-to-octets', `flex:octets-to-string', `flex:string-to-octets'.~%▶▶▶")

(fundoc 'read-file-gzip-to-gunzip-file
"Decompress contents of GZIP-INPUT-PATHNAME to GZIP-INPUT-PATHNAME.~%~@
GZIP-INPUT-PATHNAME is a pathname-designator for a gzip'd file to gunzip.
GZIP-OUTPUT-PATHNAME is a pathname-designator to output the decompressed file contents to.
:EXAMPLE
 \(read-file-gzip-to-gunzip-to-file 
  \(merge-pathnames \(make-pathname :directory '\(:relative \"tests\"\) :name \"test\" :type \"gz\"\)\)
   \(merge-pathnames \(make-pathname :directory '\(:relative \"tests\"\) :name \"test-unzip\"\)\)\)~%
\(read-file-gzip-to-gunzip-file 
 \(write-string-to-file-gzip 
  \(mon-test:make-random-string 300\) 
  \(merge-pathnames \(make-pathname :directory '\(:relative \"tests\"\) :name \"gz-rand\" :type \"gz\"\) t\)\)
 \(merge-pathnames \(make-pathname :directory '\(:relative \"tests\"\) :name \"gz-rand-unzip\"\)\)\)~%~@
:SEE-ALSO `write-string-to-file-gzip', `read-file-gunzip-to-string',
`read-file-gzip-to-gunzip-file', `salza2:with-compressor',
`salza2:compress-octet-vector', `salza2:make-stream-output-callback',
`flex:with-output-to-sequence', `chipz:decompress', `sb-ext:octets-to-string',
`sb-ext:string-to-octets', `flex:octets-to-string', `flex:string-to-octets'.~%▶▶▶")

(fundoc 'gzip-files-and-delete-source
        "For each file in FILES-LIST, compress it as if by `salza2:gzip-file' and
`cl:delete-file' the source.~%~@
For each file in FILES-LIST, return a list of the form:~%
 \(#P<FILE>.gz #P<FILE>\)~%~@
The car of each list is pathname of the gzipped file.
The cadr of each list is pathname of the deleted file.~%~@
It is assumed each file in FILES-LIST would satisfy a test with `cl:probe-file'.~%~@
Compressed files are given a \".gz\" extension.
For example, if a file name has `cl:file-namestring' \"file.bmp\", its
compressed `cl:file-namestring' is \"file.bmp.gz\".~%~@
If an existing file with a \".gz\" extension exists for a given
`cl:file-namestring' the existing file superseded.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
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
