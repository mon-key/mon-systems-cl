;;; :FILE-CREATED <Timestamp: #{2011-08-02T19:25:49-04:00Z}#{11312} - by MON>
;;; :FILE mon-systems/image-rotate.lisp
;;; ==============================

(in-package #:mon)
;; *package*


(defparameter *rotate-images-thread* '())

(defparameter *read-image-file-list* '())

(defparameter *valid-image-types* (list "jpg" "jpeg" "tiff" "tif" "bmp" "png"))

(defparameter *image-output-default-thumb-type* "jpg")


(defun verify-image-file-output-type (maybe-valid-output-extension)
  (declare (string maybe-valid-output-extension))
  (unless (member maybe-valid-output-extension mon::*valid-image-types* :test #'string-equal)
    (mon:simple-error-mon  :w-sym "verify-image-file-output-type"
                           :w-type 'function
                           :w-spec "arg MAYBE-VALID-OUTPUT-EXTENSION not string-equal any of:~% ~S"
                           :w-args  (list mon::*valid-image-types*)
                           :w-got   maybe-valid-output-extension
                           :w-type-of t
                           :signal-or-only nil))
  (string-downcase maybe-valid-output-extension))

(defun verify-image-file-file-kind (maybe-image-file-file &optional (error-on-wild-empty-dotted t))
  (declare (mon:pathname-or-namestring maybe-image-file-file))
  (unless (mon:pathname-or-namestring-not-empty-dotted-or-wild-p maybe-image-file-file)
    (if error-on-wild-empty-dotted
        (mon:simple-error-mon  :w-sym "verify-image-file-file-kind"
                               :w-type 'function
                               :w-spec "arg MAYBE-IMAGE-FILE-FILE not `mon:pathname-or-namestring-not-empty-dotted-or-wild-p'"
                               :w-got   maybe-image-file-file
                               :w-type-of t
                               :signal-or-only nil)
        (return-from verify-image-file-file-kind nil)))
  (case (osicat:file-kind maybe-image-file-file)
    (:regular-file (pathname maybe-image-file-file))
    (t nil)))

(defun unset-special-param-read-image-file-list (special-param) 
  (declare (special special-param))
  (when 
      (and (boundp special-param)
           (symbol-value special-param))
    (set special-param nil)))

(defun read-image-file-list-from-file (pathname-or-namestring &key (special-param '*read-image-file-list*)
                                       ;;(element-type 'character))
                                       (external-format :default))

  (declare (mon:pathname-or-namestring pathname-or-namestring)
           (special special-param))
  (with-open-file (img-files  pathname-or-namestring 
                              :direction         :input 
                              :if-does-not-exist :error
                              :external-format   external-format
                              :element-type      'character)
    ;; Make sure that :if-does-not-exist has a chance to run
    (unset-special-param-read-image-file-list special-param)
    (set special-param (read  img-files))))

(defun read-image-file-list-from-fprint0-file (pathname-or-namestring &key (special-param 'mon::*read-image-file-list*)
                                               ;;(element-type 'character))
                                               (external-format :default))
  (declare (mon:pathname-or-namestring pathname-or-namestring)
           (special special-param))
  (mon::unset-special-param-read-image-file-list special-param)
  (set special-param
       (read-file-list-from-fprint0-file pathname-or-namestring :external-format external-format)))

(defun make-target-pathname-for-image-resize (source-pathname &key target-directory target-type
                                                               (prefix-name-with "") 
                                                               (suffix-name-with ""))
  (declare (mon:pathname-or-namestring target-directory target-type)
           (string prefix-name-with suffix-name-with))
  (let ((dest-dir  (pathname-directory target-directory))
        (dest-name (concatenate 'string prefix-name-with (pathname-name source-pathname) suffix-name-with))
        (dest-type (mon::verify-image-file-output-type target-type)))
    (cons  source-pathname
           (make-pathname :directory dest-dir
                          :name      dest-name
                          :type      dest-type))))

(defun make-pathname-source-destination-resize-pairs (read-source-files-from &key target-directory 
                                                                                  target-type
                                                                                  (prefix-name-with "")
                                                                                  (suffix-name-with ""))
  (declare (pathname-or-namestring read-source-files-from target-directory)
           (string  target-type prefix-name-with suffix-name-with))
  (flet ((mk-rsz-path (source-image)
           (declare (pathname-or-namestring source-image))
           (make-target-pathname-for-image-resize
            source-image
            :target-directory target-directory 
            :prefix-name-with prefix-name-with
            :suffix-name-with suffix-name-with
            :target-type      target-type)))
    (loop 
       for file in (read-image-file-list-from-fprint0-file read-source-files-from)
       collecting (mk-rsz-path (pathname file)))))

;; 


(defun resize-image-files-in-fprint0-file (fprint0-file &key target-directory 
                                                             target-type
                                                             (prefix-name-with "")
                                                             (suffix-name-with "" suffix-supplied)
                                                             resize-x)
  (declare (pathname-or-namestring  fprint0-file target-directory target-type)
           (string prefix-name-with suffix-name-with)
           ((unsigned-byte 32) resize-x))
  (let* ((resize-arg            (format nil "~D" resize-x))
         (suffix-with           (or (and suffix-supplied suffix-name-with)
                                    (concatenate 'string '(#\-) (the string resize-arg))))
         (base-resize-arg-list (list "-resize" resize-arg))
         (resize-pairs         (make-pathname-source-destination-resize-pairs fprint0-file 
                                                                              :target-directory target-directory
                                                                              :target-type      target-type
                                                                              :prefix-name-with prefix-name-with
                                                                              :suffix-name-with suffix-with))
         (convert-path   "/usr/bin/convert")
         (proc-stack (make-array (length resize-pairs) :fill-pointer 0)))
    (declare (string convert-path))
    (labels ((convert-resize (pathname-pairs)
               (declare (cons pathname-pairs))
               (let* ((source-dest (list (namestring (car pathname-pairs))
                                         (namestring (cdr pathname-pairs))))
                      (arglist (append base-resize-arg-list source-dest))
                      (proc-stat '()))
                 (setf proc-stat
                       `(,(sb-ext:process-exit-code (sb-ext:run-program convert-path arglist))
                          ,@source-dest))))
             (all-resized ()
               (dolist (rsz resize-pairs
                        (setf proc-stack (coerce proc-stack 'list)))
                 (vector-push (convert-resize rsz) proc-stack)))
             (all-resized-in-thread ()
               (sb-thread:make-thread #'all-resized :name "resize-image-files-in-fprint0-file")))
    (all-resized-in-thread))))

;; :NOTE This should really be installed to Clime...
(defun rotate-image-files-in-dir-list (dir-list &key image-type degrees positive-or-negative 
                                       (special-thread-param '*rotate-images-thread*)
                                       (report-stream *standard-output*))
  (declare ((integer 1 359) degrees)
           (string image-type)
           ((or symbol keyword) positive-or-negative))
  (unless (member positive-or-negative (list :positive :negative :clockwise :counter-clockwise))
    (error "keyword POSITIVE-OR-NEGATIVE not one of:~%~T~
                      :positive :negative :clockwise :counter-clockwise~%"))
  (unless (member image-type *valid-image-types* :test #'string=)
    (error "keyword type not member of:~%~T~S~%" *valid-image-types*))
  (let ((rotation-string (format nil "~C~D" (ecase positive-or-negative
                                              ((:positive :clockwise) #\+)
                                              ((:negative :counter-clockwise) #\-))
                                 degrees))
        (wild-file-type (make-pathname :name :wild :type image-type))
        (proc-stack '()))
    (declare (string rotation-string)
             (pathname wild-file-type))
    (labels ((merge-wild-ftype (merge-dir)
               (merge-pathnames wild-file-type merge-dir))
             (make-rotation-list ()
               (loop 
                  for dir in dir-list
                  for merge-dir = (merge-wild-ftype dir)
                  nconcing (directory merge-dir :resolve-symlinks nil) into rtn
                  finally (return (loop for pths in rtn collect (namestring pths)))))
             (process-rotation (pathname-native)
               (let ((proc-stat 
                      (sb-ext:process-exit-code 
                       (sb-ext:run-program "/usr/bin/convert" (list "-rotate" rotation-string pathname-native pathname-native) ))))
                 ;; not following could prob. be accomplished with an :if-error-exists arg.
                 (if (zerop proc-stat)
                     (format report-stream "~&~%successful rotation ~A of image at pathname: ~A" rotation-string pathname-native)
                     (format report-stream "~&~%failed to rotate image at pathname: ~A~&~%process exited with code ~D"
                             pathname-native proc-stat))
                 (push (cons pathname-native proc-stat)  proc-stack)))
             (all-rotations ()
               ;; (make-rotation-list)
               (dolist (dr (make-rotation-list)
                        ;;(print (setf proc-stack (nreverse proc-stack)) report-stream))
                        (setf proc-stack (nreverse proc-stack)))
                 (process-rotation dr)))
             (all-rotations-in-thread  ()
               (sb-thread:make-thread #'all-rotations  
                                      :name "rotate-image-files-in-dir-list")))
      (set special-thread-param
           (all-rotations-in-thread)))))


;;; ==============================
;;; :VARIABLES-DOCUMENTATION
;;; ==============================

(vardoc '*read-image-file-list*
        "Variable holding a list of pathnames.
Variable is set by `read-image-file-list-from-file'.
It is unset with `unset-special-param-read-image-file-list'.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(vardoc '*valid-image-types*
"List of strings designating valid pathname-types.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(vardoc '*image-output-default-thumb-type*
"String designating a defualt `cl:pathname-type' to use when outputing an image type.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

;;; ==============================
;;; :FUNCTIONS-DOCUMENTATION
;;; ==============================

(fundoc 'verify-image-file-file-kind
" Whether MAYBE-IMAGE-FILE-FILE satisfies `mon:pathname-or-namestring-not-empty-dotted-or-wild-p'.~%~@
If so, return its `cl:pathname' representation.
If not when ERROR-ON-WILD-EMPTY-DOTTED is null return nil, else signal an error.
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'verify-image-file-output-type
"Whether MAYBE-VALID-OUTPUT-EXTENSION is cl:string-equal an element in `mon:*valid-image-types*'.~%~@
If so, return its `cl:string-downcase'd representation else signal an error.~%~@
:EXAMPLE~%
 \(mapcar #'verify-image-file-output-type 
         \(mapcar #'string-upcase mon::*valid-image-types*\)\)~%
 \(verify-image-file-output-type \"bubba\"\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'unset-special-param-read-file-list
"Unset the value of SPECIAL-PARAM.~%~@
:EXAMPLE~%
 \(progn
   \(setf *read-image-file-list* \(list \"bubba\" \"BUBBA\" \"Bubba\"\)\)
   \(unset-special-param-read-file-list '*read-image-file-list*\)\) 
:SEE-ALSO `<XREF>'.~%▶▶▶"))

(read-image-file-list-from-file
"Read the list of pathnames stored in PATHNAME-OR-NAMESTRING set the list read as value of SPECIAL-PARAM.~%~@
Keyword SPECIAL-PARAM is a special parameter to use when holding a list of
image file pathnames. Default is `mon:*read-image-file-list*'.~%~@
Keyword EXTERNAL-FORMAT is as if by `cl:open'. Default value is :default.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'make-target-pathname-for-image-resize
        "Return a pathname for use in image resizing operations~%~@
Return value is a cons of the form:~%
 \( <SOURCE-PATHAME> . <GENERATED-DESTINATION-PATHNAME>\)~%~@
SOURCE-PATHNAME is an object of type `mon:pathname-or-namestring' designating an
existing image file. Its `cl:pathname-name' is used as the template when
generating the returned pathname.~%~@
TARGET-DIRECTORY is an object of type `mon:pathname-or-namestring' designating a
directory in which the resized image will be located.~%
TARGET-TYPE is a string designating a `cl:pathname-type' image file extension.
It should satisfy `mon:verify-image-file-output-type'.~%~@
Keyword PREFIX-NAME-WITH is a string to prepend to SOURCE-PATHNAME's `cl:pathname-name'.~%~@
Keyword SUFFIX-NAME-WITH is a string to append to SOURCE-PATHNAME's `cl:pathname-name'.~%~@
:EXAMPLE~%
 \(make-target-pathname-for-image-resize
  #P\"/some/source/path/to/existing/image-of-type-bitmpap.bmp\" 
  :target-directory #P\"/some/destination/path/for/resized/image/\" 
  :target-type \"jpg\"
  :prefix-name-with \"prepended-\" 
  :suffix-name-with \"-appended\"\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'make-pathname-source-destination-resize-pairs
"Return a list of consed source/target image pairs.~%~@
READ-SOURCE-FILES-FROM is an object of type `mon:pathname-or-namestring' its
contents are processed with `mon:read-image-file-list-from-fprint0-file'.
Keywords TARGET-DIRECTORY TARGET-TYPE PREFIX-NAME-WITH SUFFIX-NAME-WITH are as
per `mon:make-target-pathname-for-image-resize'
:EXAMPLE~%~@                          
 \(make-pathname-source-destination-resize-pairs
  #P\"/some/file/with/null-byte/delimited/image-file-names\"
  #P\"/some/source/path/to/existing/image-of-type-bitmpap.bmp\" 
  :target-directory #P\"/some/destination/path/for/resized/image/\" 
  :target-type \"jpg\"
  :prefix-name-with \"prepended-\" 
  :suffix-name-with \"-appended\"\)
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'read-image-file-list-from-fprint0-file
        "Read the #\\Nul character terminated pathnames contained of PATHNAME-OR-NAMESTRING.~%~@
Return a list of strings with each null terminated pathname split on the
terminating #\\Nul character with #\\Nul char removed.~%~@
Occurences of #\\Newline and #\\Return are elided from results.~%~@
:NOTE A #\\Nul character terminated pathname is the default output for the unix
command `find` when it used invoked the -frint0 arg.~%~@
Keyword SPECIAL-PARAM is a special variable to bind results to. Default is `mon::*read-image-file-list*'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'resize-image-files-in-fprint0-file
"Resize each null-terminated pathname in FPRINT0-FILE.~%~@
Keyword resize-x is an unsigned integer value.
Keywords TARGET-DIRECTORY, TARGET-TYPE, PREFIX-NAME-WITH, and SUFFIX-NAME-WITH are as per 
`mon:make-pathname-source-destination-resize-pairs'.
When SUFFIX-NAME-WITH is not explicitly provided the value of RESIZE-X is
appended to the resized imaged saved to TARGET-DIRECTORY.~%~@
:EXAMPLE~%
 \(let* \(\(null-list-directory #P\"/some/directory/with/bitmaps/\" \)
       \(null-list-pathname  \(merge-pathnames 
                             \(make-pathname :name \"null-terminated-file-list\"\)
                             null-terminated-file-list\)\)
       \(sb-ext:run-program \"/usr/bin/find\" \(list \(namestring null-list-directory\)
                                                 \"-type\" \"f\" \"-name\" \"*.bmp\" \"-fprint0\"
                                                 \(namestring null-list-pathname\)\)\)
       \(mon::resize-image-files-in-fprint0-file null-list-pathname 
                                                :target-directory null-list-directory 
                                                :target-type \"jpg\" 
                                                :resize-x 1000\)\)\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'rotate-image-files-in-dir-list
"Rotate each image found in the directories of DIR-LIST by DEGREES POSITIVE-OR-NEGATIVE.~%~@
Rotation is performed with external command `convert` from the ImageMagick suite.~%~@
DIR-LIST is a list of pathnames designating existing directories. No symlink detection is performed.
Keyword IMAGE-TYPE names and image file extension.
It is a string with the same format as the :type argument to `cl:make-pathname'.
Valid values are limited to the following case sensitve image type extensions:~%
 \"jpg\" \"jpeg\" \"tiff\" \"tif\" \"bmp\" \"png\"~%~@
Keyword DEGREES is an integer in the range [1,359] designating a degree of
rotation to apply to images.~%~@
Keyword POSITIVE-OR-NEGATIVE is a keyword designating whether rotation is
positive or negative. Valid values are:~%
 :POSITIVE :NEGATIVE :CLOCKWISE :COUNTER-CLOCKWISE~%~@
SPECIAL-THREAD-PARAM is a symbol naming a special variable which holds the
thread object this function exececutes in. Default is mon::*rotate-images-thread*.~%
:USAGE~%~@
 \(rotate-image-files-in-dir-list
 \(list #P\"/mnt/some/path/to/goofy/1351/\"
        #P\"/mnt/some/path/to/goofy/1353/\"
        #P\"/mnt/some/path/to/goofy/1515/\"
        #P\"/mnt/some/path/to/goofy/1535/\"\)
 :image-type \"jpeg\" :degrees 90 :positive-or-negative :counter-clockwise\)~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")



;;; ==============================
;;; EOF
