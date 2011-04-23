;;; :FILE-CREATED <Timestamp: #{2011-03-15T18:58:34-04:00Z}#{11112} - by MON>
;;; :FILE mon-systems/tests/usec-tests.lisp
;;; ==============================


(in-package #:mon-test)
;; *package*

(loop
   repeat 10000
   for usec = (nth-value 1 (sb-ext:get-time-of-day))
   collect usec into allb
   collect (logand usec #xff) into 8b
   collect (logand usec #xffff) into 16b
   finally (return (map 'list #'(lambda (x) 
                                  (list 1000 (length (delete-duplicates x))))
                        (list 8b 16b allb))))

;; (loop repeat 10000 collect (nth-value 1 sb-ext:get-time-of-day))

(let ((gthr '())
      (pthnm 
       #-IS-MON (merge-pathnames (enough-namestring (make-pathname :directory '(:relative "notes") :name "1000-usecs")
                                           *default-pathname-defaults*) *default-pathname-defaults*)
       #+IS-MON (translate-logical-pathname "MON:MON-SYSTEMS;notes;1000-usecs")))
  (setf gthr 
        (loop
           repeat 10000 
           collect (nth-value 1 (sb-ext:get-time-of-day))))
  (setf gthr (sort gthr #'>))
  (mon:with-file-overwritten (s pthnm)
    (loop 
       for num in gthr
       do (print num s))))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon-test
;; End:

;;; ==============================
;;; EOF
