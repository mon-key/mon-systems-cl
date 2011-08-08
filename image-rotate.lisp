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
    (:regualr-file (pathname maybe-image-file-file))
    (t nil)))

(defun unset-special-param-read-image-file-list (special-param) 
  (declare (special special-param))
  (when 
      (and (boundp special-param)
           (symbol-value special-param))
    (set special-param nil)))

;; :TODO add a per line variant of this that checks for null-byte at EOL as delimiter.
(defun read-image-file-list-from-file (pathname-or-namestring &optional (special-param '*read-image-file-list*))
  (declare (mon:pathname-or-namestring pathname-or-namestring)
           (special special-param))
  (with-open-file (img-files  pathname-or-namestring 
                              :direction :input 
                              :if-does-not-exist :error)
    ;; Make sure that :if-does-not-exist has a chance to run
    (unset-special-param-read-image-file-list special-param)
    (set special-param (read  img-files))))

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
SPECIAL-PARAM is a special parameter to use when holding a list of
image file pathnames. Default is `mon:*read-image-file-list*'
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
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
