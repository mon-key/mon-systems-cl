;;; :FILE-CREATED <Timestamp: #{2011-08-02T19:25:49-04:00Z}#{11312} - by MON>
;;; :FILE mon-systems/image-rotate.lisp
;;; ==============================

(in-package #:mon)
;; *package*


(defparameter *rotate-images-thread* '())

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
  (unless (member image-type (list "jpg" "jpeg" "tiff" "tif" "bmp" "png") :test #'string=)
    (error "keyword type not one of:~%~T~
                      \"jpg\" \"jpeg\" \"tiff\" \"tif\" \"bmp\" \"png\"~%"))
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
;;; :DOCUMENTATION
;;; ==============================

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
