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
;; 
;; we want -0500 (EST) and -0400 (EDT) and aren't getting them.
;; (time-zone-to-string 5 t)   (cond ((and (plusp tz) dst) ; -0400
;; (time-zone-to-string 5 nil)        (and (plusp tz) (null dst)) ; -0500
;; (time-zone-to-string -5 nil)       (and (minusp tz) (null dst)) ; -0500
;; (time-zone-to-string -5 t)
;; 
;; :NOTE BROKEN!
(defun time-zone-to-string (tz dst &optional (long t))
  (declare (type rational tz))
  (multiple-value-bind (hr mm) (floor (abs (- (if dst 1 0) tz)))
    (let ((mi (floor (* 60 mm)))
          (zo (assoc tz (the list *time-zones*))) )
      (format nil "~:[+~;-~]~2,'0d~:[:~;~]~2,'0d~@[ (~a)~]"
              ;;     tz +/-       
              ;; not sure what/where the problem is but the original is missing on DST
              ;; :WAS (minusp tz)
              (or (minusp tz) dst)
              hr long mi (and long (if dst (cadr zo) (cddr zo)))))))

;; during standard time
;; (eastern-time-zone-to-string nil) ;=> "-0500"
;; (eastern-time-zone-to-string t) "-0500 (EST)"
;; during daylight-savings time
;; (eastern-time-zone-to-string t) "-0400"
;; (eastern-time-zone-to-string t) "-0400 (EDT)"
(defun eastern-time-zone-to-string (&optional (long t))
  (multiple-value-bind (sec min hr date month year dow dst tz) (get-decoded-time)
    (declare (ignore sec min hr date month year dow))
    (if (eql tz 5)
        (if dst
            (format nil "-0400~A" (or (and long " (EDT)") ""))
            (format nil "-0500~A" (or (and long " (EST)") "")))
    (time-zone-to-string tz dst long))))

(defun time-string-yyyy-mm-dd (&optional universal-time stream)
  (declare (stream-or-boolean-or-string-with-fill-pointer stream))
  (format-timestring stream
                     (if universal-time
                         (local-time:universal-to-timestamp universal-time)
                         (local-time:now))
                     :format *timestamp-yyyy-mm-dd-format*))

(defun time-string-get-universal-time ()
  (declare (optimize (speed 3)))
  (princ-to-string (get-universal-time)))

(defun get-universal-time-string ()
  (declare (optimize (speed 3))
           (inline get-universal-time-string))
  (time-string-get-universal-time))

;; (local-time:parse-timestring (substring  (time-string-right-now) 0 24))
;; (local-time:parse-timestring "2012-03-03T16:03:47-04:00")
(defun time-string-right-now (&optional (stream nil))
  (declare (stream-or-boolean-or-string-with-fill-pointer fmt-out))
  (multiple-value-bind (se mi ho da mo ye dow dst tz) (get-decoded-time)
    (declare (ignore dow dst tz)
	     (type fixnum-exclusive se mi ho da mo ye) ;dw)
	     (type rational tz))
    (format stream
	    ;; :WAS "~4d-~2,'0d-~2,'0d ~a ~2,'0d:~2,'0d:~2,'0d ~a"
	    ;; ye mo da (aref (the (simple-array simple-string (7)) *week-days*) dw) ho mi se
	    "~4d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d~aZ"
	    ye mo da  ho mi se 
            (eastern-time-zone-to-string nil)
            )))

(defun timestamp (&optional stream)
  (declare (stream-or-boolean-or-string-with-fill-pointer stream))
  ;; (let ((chk-usr (or (and (consp *user-name*) 
  ;;                         (cdr *user-name*))
  ;;                    "")))
  ;;   (format stream "<Timestamp: #{~A} - by ~A>" (time-string-right-now) chk-usr))
  (format-timestring stream (local-time:now) :format *timestamp-for-file-header-format*))

;; (format-timestring nil (local-time:now) :format )

;; "<Timestamp: #{2012-03-03T18:32:03-0500Z} - by MON>"
;; (decode-universal-time (get-universal-time))
;; (timestamp-for-file)
;; (timestamp-for-file :universal-time t)
;; `get-decoded-time'
;; Return nine values specifying the current time as follows:
;; second, minute, hour, date, month, year, day of week (0 = Monday), T
;; (daylight savings times) or NIL (standard time), and timezone.
;;
;; :NOTE The UNIVERSAL-TIME keyword is poorly named but changing it to something
;; like UNIVERSAL-TIME-AS-STRING will require adjusting callers in external systems...
(defun timestamp-for-file (&key (universal-time nil)
                                (with-utc-offset nil))
  (declare (boolean universal-time
                    with-utc-offset))
  (if universal-time
      (time-string-get-universal-time)
      ;; (multiple-value-bind (sec min hr day mon yr wd dp zn) (get-decoded-time)
      ;;   (declare (ignore wd dp zn))
      ;;   (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d~2,'0d~2,'0d" 
      ;;           ;;       yr     mon    day     hr    min   sec
      ;;           yr mon day hr min sec))
      (format-timestring nil (local-time:now)
                         :format (if with-utc-offset 
                                     *timestamp-for-file-gmt-no-colon-offset-format*
                                     *timestamp-for-file-format*)
                         :timezone local-time:*default-timezone*)))
;; (timestamp-for-file :with-utc-offset t)
;; "2012-03-03T181325-0500"

(defun %lt-construct-timestring (timestamp format timezone)
  (declare (type local-time:timestamp timestamp)
           (optimize (speed 3)))
  (multiple-value-bind (nsec sec minute hour day month year weekday daylight-p offset abbrev)
      (local-time:decode-timestamp timestamp :timezone timezone)
    (declare (ignore daylight-p))
    (let ((*print-pretty* nil)
          (*print-circle* nil))
      (with-output-to-string (result nil :element-type 'base-char)
        (dolist (fmt format)
          (case (or (and (stringp fmt) :string) 
                    (and (characterp fmt) :character)
                    fmt)
            ((or :gmt-offset :gmt-offset-or-z :gmt-offset-no-colon)
             (multiple-value-bind (offset-hours offset-secs)
                 (floor offset local-time:+seconds-per-hour+)
               (declare (fixnum offset-hours offset-secs))
               (if (and (eql fmt :gmt-offset-or-z) 
                        (zerop offset))
                   (princ #\Z result)
                   (format result (if (eql fmt :gmt-offset-no-colon)
                                      "~C~2,'0D~2,'0D"
                                      "~C~2,'0D:~2,'0D")
                           (if (minusp offset-hours) #\- #\+)
                           (abs offset-hours)
                           (truncate (abs offset-secs)
                                     local-time:+seconds-per-minute+)))))
            (:short-year
             (princ (mod year 100) result))
            (:long-month
             (princ (aref local-time:+month-names+ month) result))
            (:short-month
             (princ (aref local-time:+short-month-names+ month) result))
            (:long-weekday
             (princ (aref local-time:+day-names+ weekday) result))
            (:short-weekday
             (princ (aref local-time:+short-day-names+ weekday) result))
            (:timezone
             (princ abbrev result))
            (:hour12
             (princ (1+ (mod (1- hour) 12)) result))
            (:ampm
             (princ (if (< hour 12) "am" "pm") result))
            (:ordinal-day
             (princ (local-time::ordinalize day) result))
            ((:character :string)
             (princ fmt result))
            (t
             (let ((val (ecase (if (consp fmt) (car fmt) fmt)
                          (:nsec nsec)
                          (:usec (floor nsec 1000))
                          (:msec (floor nsec 1000000))
                          (:sec sec)
                          (:min minute)
                          (:hour hour)
                          (:day day)
                          (:weekday weekday)
                          (:month month)
                          (:year year))))
               (cond
                 ((atom fmt)
                  (princ val result))
                 ((minusp val)
                  (format result "-~V,VD"
                          (second fmt)
                          (or (third fmt) #\0)
                          (abs val)))
                 (t
                  (format result "~V,VD"
                          (second fmt)
                          (or (third fmt) #\0)
                          val)))))))))))

(defun format-timestring (stream timestamp-object &key (format *iso-8601-format*)
                                                       (timezone local-time:*default-timezone*))
  (declare (type local-time:timestamp timestamp-object)
           (type (or boolean stream) stream))
  (let ((result (%lt-construct-timestring timestamp-object format timezone)))
    (when stream
      (write-string result stream))
    result))

;; :SOURCE dhs-db/dhs-db-api/timestamp.lisp
;; :WAS `make-database-timestamp'
(defun timestamp-from-database-convert (v)
  (multiple-value-bind (matched values) (cl-ppcre:scan-to-strings "^([0-9]{4})-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2}):([0-9]{2})(\\.[0-9]+)?$" v)
    (unless matched (error "Can't parse ~S as a timestamp" v))
    ;;(make-instance 'database-timestamp :value 
    (encode-universal-time  (parse-integer (aref values 5))
                           (parse-integer (aref values 4))
                           (parse-integer (aref values 3))
                           (parse-integer (aref values 2))
                           (parse-integer (aref values 1))
                           (parse-integer (aref values 0)));)
    ))
;; :SOURCE dhs-db/dhs-db-api/timestamp.lisp
;; :WAS `make-database-date'
(defun date-from-database-convert (v)
  (multiple-value-bind (matched values) (cl-ppcre:scan-to-strings "^([0-9]{4})-([0-9]{2})-([0-9]{2})$" v)
    (unless matched
      (error "Can't parse ~S as a date" v))
    ;;(make-instance 'database-date :value 
    (encode-universal-time 0 0 0
                           (parse-integer (aref values 2))
                           (parse-integer (aref values 1))
                           (parse-integer (aref values 0))) ;)
    ))

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
"Convert a timezone as per output of `cl:get-decoded-time' `cl:decode-universal-time' to a string.~%~@
Arg TZ is a timezone \(rational [-24;24], multiple of 3600\).~%~@
Arg DST is a boolean indicating whether timezone is in daylight savings time.~%
Optional arg LONG is a boolean indicating whether output string should be in
long or short format:~%
When LONG is T (the default) output has the format:~%
  \"[+-]<NNNN> \(<DST>\)\"~%~@
When LONG is NIL output has the format:~%
  \\\"[+-]<NN:NN>\"~%~@
:EXAMPLE~%
 \(time-zone-to-string 5 nil\)~%
 \(time-zone-to-string 5 t\)~%
 \(time-zone-to-string 5 nil nil\)~%
 \(time-zone-to-string 5 t nil\)~%
 \(apply #'time-zone-to-string \(nreverse \(last \(multiple-value-list \(get-decoded-time\)\) 2\)\)\)~%~@
:SEE-ALSO `mon:time-string-right-now', `mon:current-time',
`cl:get-internal-real-time', `cl:get-decoded-time', `cl:decode-universal-time',
`cl:encode-universal-time'.~%▶▶▶")

(fundoc 'timestamp-for-file
  "Timestamp string for use when generating unique file names.~%~@
When keyword UNIVERSAL-TIME \(a boolean\) is non-nil return value of
`cl:get-universal-time' as a string
When keyword WITH-UTC-OFFSET \(a boolean\) is null return value has the format:~%
  2011-03-15-T131131~%~@
When keyword WITH-UTC-OFFSET is T return value has one of the formats:~%
  2011-03-15-T131131+NNNN
  2011-03-15-T131131-NNNN~%~@
depending on whether daylight savings time is active.
:EXAMPLE~%
 \(timestamp-for-file\)~%
 \(timestamp-for-file :universal-time t\)
 \(timestamp-for-file :with-utc-offset t\)
:NOTE Ommision of puctuation chars #\\: and #\\. is intentional!
:SEE-ALSO `<XREF>'.~%▶▶▶")

(fundoc 'time-string-get-universal-time
        "Return current value of `cl:get-universal-time' as a string.~%~@
:EXAMPLE~%
 \(time-string-get-universal-time\)~%~@
:SEE-ALSO `mon:time-string-right-now', `mon:current-time',
`cl:get-internal-real-time', `cl:get-decoded-time', `cl:decode-universal-time',
`cl:encode-universal-time'.~%▶▶▶")

(fundoc 'get-universal-time-string
        "Alias for `mon:time-string-get-universal-time'.~%~@
Return current value of `cl:get-universal-time' as a string.~%~@
:EXAMPLE~%
 \(get-universal-time-string\)~%~@
:SEE-ALSO `mon:time-string-right-now', `mon:current-time',
`cl:get-internal-real-time', `cl:get-decoded-time', `cl:decode-universal-time',
`cl:encode-universal-time'.~%▶▶▶")

(fundoc '%lt-construct-timestring
 "Like `local-time::%construct-timestring' but allows FORMAT to contain the
element :GMT-OFFSET-NO-COLON.~%~@
If present it indicates to print the offset from UTC in an alternative format
separate from that perscribed by ISO-8601/RFC 3339.~%~@
This means that the time-numoffset form described in RFC 3339 section entitled
\"5.6 - Internet Date/Time Format\" can be returned without containing a colon
separating the time-hour and time-minute portions.~%~@
So, instead of either +NN:NN or -NN:NN we can now get +NNNN or -NNNN.~%
 time-numoffset = \(\"+\" / \"-\"\) time-hour \":\" time-minute~%
 time-numoffset = \(\"+\" / \"-\"\) time-hour time-minute~%~@
:EXAMPLE~%
 \(%lt-construct-timestring \(local-time:now\)
                           '\(\(:year 4\) #\\- \(:month 2\) #\\- \(:day 2\) 
                             #\\T \(:hour 2\) \(:min 2\) \(:sec 2\) 
                             :gmt-offset-no-colon\)
                           local-time:*default-timezone*\)~%
 \(%lt-construct-timestring \(local-time:now\)
                           '\(\(:year 4\) #\\- \(:month 2\) #\\- \(:day 2\) #\\_ \"FOO\"\)
                           local-time:*default-timezone*\)~%
 \(%lt-construct-timestring \(local-time:now\)
                           '\(\(:year 4\) #\\- \(:month 2\) #\\- \(:day 2\) 
                             #\\T \(:hour 2\) #\\_ \(:min 2\) #\\_ \(:sec 2\) 
                             :gmt-offset #\\_ #\\\( :timezone #\\\)\)
                           local-time:*default-timezone*\)~%
 \(%lt-construct-timestring \(local-time:now\)
                           '\(\(:year 4\) #\\- \(:month 2\) #\\- \(:day 2\) 
                             #\\T \(:hour 2\) #\\_ \(:min 2\) #\\_ \(:sec 2\) 
                             :gmt-offset-or-z\) 
                           local-time:+utc-zone+\)~%
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
