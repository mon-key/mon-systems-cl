;;; :FILE-CREATED <Timestamp: #{2011-03-29T11:57:14-04:00Z}#{11132} - by MON>
;;; :FILE mon-systems/chronos.lisp
;;; ==============================

;;; ==============================
;; :NOTES
;; Other CL time related functions:
;; 
;; get-internal-real-time
;; get-universal-time
;; get-decoded-time
;; get-internal-real-time
;; get-internal-run-time
;; decode-universal-time
;; encode-universal-time
;; internal-time-units-per-second
;;
;; :SEE (info "(ansicl)Time")
;;
;; Other SBCL time related functions:
;; sb-ext:get-time-of-day
;;
;; sb-sys:system-internal-run-time
;;
;; sb-unix:unix-setitimer
;; sb-unix:unix-getitimer
;; sb-unix::micro-seconds-per-internal-time-unit
;; sb-unix::system-real-time-values
;; sb-unix::unix-get-seconds-west
;; sb-unix:nanosleep 
;; sb-unix::get-timezone 
;;
;; sb-impl::real-time->sec-and-usec
;;
;; sb-posix:time
;; sb-posix:utime
;; sb-posix:utimes
;; sb-posix:timeval ;; CLASS
;; sb-posix:timeval-sec sb-posix:timeval-usec ;; READERS
;; 
;;
;; (sb-unix::system-real-time-values) => 1301412244, 298
;;
;; (where-is "stat")
;;
;; (sb-posix:stat-ctime (sb-posix:stat (translate-logical-pathname "MON:MON-SYSTEMS;chronos.lisp")))
;; 
;;; ==============================


(in-package #:mon)
;; *package*

(defun current-time ()  
  (multiple-value-bind (secs usec)
      (sb-ext:get-time-of-day)
    (list (ldb (byte 16 16) secs) ;; hi-bit 
	  (ldb (byte 16 0)  secs) ;; lo-bit
	  usec)))

;; :SOURCE CLOCC-cllib/port/sys.lisp :WAS `tz->string'
(defun time-zone-to-string (tz dst &optional (long t))
  (declare (type rational tz))
  (multiple-value-bind (hr mm) (floor (abs (- (if dst 1 0) tz)))
    (let ((mi (floor (* 60 mm)))
          (zo (assoc tz (the list *time-zones*))) )
      (format nil "~:[+~;-~]~2,'0d~:[:~;~]~2,'0d~@[ (~a)~]"
              ;;     tz +/-       
              ;; not sure what/where the prolemb is but the original is missing on DST
              ;; :WAS (minusp tz)
              (or (minusp tz) dst)
              hr long mi (and long (if dst (cadr zo) (cddr zo)))))))

;;; :SOURCE Zach Beane's use-net-legend/utils.lisp :WAS `pretty-time-string'
(defun time-string-yyyy-mm-dd (&optional universal-time stream)
  (declare (stream-or-boolean-or-string-with-fill-pointer stream))
  (unless universal-time (setf universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time)
    (declare (ignore second minute hour))
    (format stream "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun time-string-right-now (&optional (fmt-out nil))
  (declare (stream-or-boolean-or-string-with-fill-pointer fmt-out))
  (multiple-value-bind (se mi ho da mo ye dw dst tz) (get-decoded-time)
    (declare (ignore dw)
	     (type fixnum-exclusive se mi ho da mo ye) ;dw)
	     (type rational tz))
    (format fmt-out 
	    ;; :WAS "~4d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d ~a"
	    ;; ye mo da (aref (the (simple-array simple-string (7)) *week-days*) dw) ho mi se
	    "~4d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~aZ"
	    ye mo da  ho mi se (time-zone-to-string tz dst nil))))

(defun timestamp (&optional fmt-out)
  (declare (stream-or-boolean-or-string-with-fill-pointer fmt-out))
  (let ((chk-usr (or (and (consp *user-name*) 
                          (cdr *user-name*))
                     "")))
    (format fmt-out "<Timestamp: #{~A} - by ~A>" (time-string-right-now) chk-usr)))

;; `get-decoded-time'
;; Return nine values specifying the current time as follows:
;; second, minute, hour, date, month, year, day of week (0 = Monday), T
;; (daylight savings times) or NIL (standard time), and timezone.
(defun timestamp-for-file ()
  (multiple-value-bind (sec min hr day mon yr wd dp zn) (get-decoded-time)
    (declare (ignore wd dp zn))
    (format nil "~4,'0d-~2,'0d-~2,'0d-T~2,'0d~2,'0d~2,'0d" 
            ;;       yr     mon    day     hr    min   sec
            yr mon day hr min sec 
            ;; Consider appending the bottom bytes of the microsec return value
            ;; of `sb-ext:get-time-of-day'???  No, the upper bounds is still way to small:
            ;; (loop
            ;;    repeat 1000 
            ;;    for usec = (nth-value 1 (sb-ext:get-time-of-day))
            ;;    collect usec into allb
            ;;    collect (logand usec #xff) into 8b
            ;;    collect (logand usec #xffff) into 16b
            ;;    finally (return (map 'list #'(lambda (x) 
            ;;                                   (list 1000 (length (delete-duplicates x))))
            ;;                         (list 8b 16b allb))))
            )))

;;; ==============================


;;; ==============================
;;; :CHRONOS-DOCUMENTATION
;;; ==============================

(fundoc 'current-time
"Return the current time as number of seconds since 1970-01-01 00:00:00.~%~@
Return value is a list of three integers wit the format:~%~@
 \( MSB LSB USEC \)~%~@
The first has the most significant 16 bits of the seconds, 
The second has the least significant 16 bits.~%~@
The third integer gives the microsecond count.~%~@
:EXAMPLE~%
 \(current-time\)~%~@
:EMACS-LISP-COMPAT~%~@
:SEE-ALSO `sb-ext:get-time-of-day', `mon:time-string-right-now'
`cl:get-internal-real-time', `cl:get-decoded-time', `cl:decode-universal-time',
`cl:encode-universal-time'.~%▶▶▶")

(fundoc 'time-string-right-now
        "Print the current time to destination stream out \(defaults to t\).~%~@
:EXAMPLE~%~@
 \(time-string-right-now\)~%
 \(let \(\(get-tm \(make-string-output-stream\)\)\)
   \(format get-tm \"Hey, bubba! Got the time?~~%\"\)
   \(time-string-right-now get-tm\) 
   \(format get-tm \"~~&Thanks, bubba!\"\)
   \(get-output-stream-string get-tm\)\)~%~@
:SEE-ALSO `mon:time-string-yyyy-mm-dd', `mon:current-time',
`cl:get-internal-real-time', `cl:get-decoded-time', `cl:decode-universal-time',
`cl:encode-universal-time'.~%▶▶▶")

(fundoc 'time-string-yyyy-mm-dd
"Return UNIVERSAL-TIME formatted as an ISO-8601-ish string.~%~@
Return value has the format:~%
 YYYY-MM-DD~%~@
Optional arg STREAM when non-nil should be of type
`mon:stream-or-boolean-or-string-with-fill-pointer'. 
When provided return value is output to STREAM.~%~@
:EXAMPLE~%
 \(time-string-yyyy-mm-dd\)~%
 \(time-string-yyyy-mm-dd \(get-universal-time\)\)~%
 \(time-string-yyyy-mm-dd nil t\)~%
 \(let \(\(str \(make-array 6 
                        :element-type 'base-char 
                        :fill-pointer 6 
                        :initial-contents \":date \"\)\)\)
   \(time-string-yyyy-mm-dd \(get-universal-time\) str\)
   str\)
 \(with-output-to-string \(s\) \(time-string-yyyy-mm-dd nil s\)\)~%
:SEE-ALSO `mon:time-string-right-now', `mon:current-time',
`cl:get-internal-real-time', `cl:get-decoded-time', `cl:decode-universal-time',
`cl:encode-universal-time'.~%▶▶▶")

(fundoc 'time-zone-to-string
"Convert the CL timezone \(rational [-24;24], multiple of 3600\) to a string.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'timestamp-for-file
  "Timestamp string for use when generating unique file names.~%~@
Return value has the format:~%
 2011-03-15-T131131~%~@
:EXAMPLE~%
 \(timestamp-for-file\)~%~@
:NOTE Ommision of puctuation chars #\\: and #\\. is intentional!
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
