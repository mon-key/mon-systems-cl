;;; :FILE-CREATED <Timestamp: #{2011-03-21T19:22:54-04:00Z}#{11121} - by MON>
;;; :FILE mon-systems/tests/timing.lisp
;;; ==============================


;;; ==============================
;; :COURTESY Peter Seibel's source code for _Practical Common Lisp_
;; :SOURCE practicals-1.0.3/Chapter32/profiler.lisp
;; :SEE (URL `http://www.gigamonkeys.com/book/conclusion-whats-next.html')
;;
;;; ==============================
;; Copyright (c) 2005, Peter Seibel All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     * Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;     * Redistributions in binary form must reproduce the above
;;       copyright notice, this list of conditions and the following
;;       disclaimer in the documentation and/or other materials provided
;;       with the distribution.
;;
;;     * Neither the name of the Peter Seibel nor the names of its
;;       contributors may be used to endorse or promote products derived
;;       from this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;; ==============================


;;; ==============================
;; :RENAMED 
;; `show-timing-data'    -> `timing-data-show'
;; `clear-timing-data'   -> `timing-data-clear'
;; `compile-timing-data' -> `timing-data-compile'
;; `with-timing'         -> `with-timing-collected'
;;; ==============================

;; (where-is "call-with-timing")

;; (sb-ext:call-with-timing  "call-with-timing")


(in-package #:mon-test)
;; *package*

(defparameter *timing-data* '())

;; :SOURCE Paul Khuong's string-case-20110219-http/string-case.lisp
(defmacro with-timing ((total-iters subiters) &body forms)
  (let ((_thunk (gensym "THUNK"))
        (iters  (ceiling total-iters subiters)))
    `(flet ((,_thunk ()
              ,@forms))
       (let ((min sb-ext:double-float-positive-infinity)
             (sum 0d0)
             (max 0d0))
         (declare (type double-float min sum max))
         (loop repeat ,iters
            do (multiple-value-bind (_ begin/sec begin/us)
                   (sb-unix:unix-fast-getrusage sb-unix:rusage_self)
                 (declare (ignore _))
                 (loop repeat ,subiters
                    do (,_thunk))
                 (multiple-value-bind (_ end/sec end/us)
                     (sb-unix:unix-fast-getrusage sb-unix:rusage_self)
                   (declare (ignore _))
                   (let ((time (+ (float  (- end/sec begin/sec) 0d0)
                                  (* 1d-6 (- end/us begin/us)))))
                     (setf min (min time min)
                           sum (+   time sum)
                           max (max time max))
                     (values))))
            finally (return (values min
                                    (/ sum ,iters)
                                    max)))))))
;;
;; (defun test-string-case (test-str)
;;   (string-case:string-case (test-str)
;;     ("" 'empty)
;;     ("foo" 'foo)
;;     ("fob" 'fob)
;;     ("foobar" 'hit)
;;     (t     'default)))
;;
;; (with-timing (800000 8000) (test-string-case "foobar"))
;; => 0.0d0, 8.999400000000003d-4, 0.0033339999999999997d0

(defmacro with-timing-collected (label &body body)
  (mon:with-gensyms (start)
    `(let ((,start (get-internal-run-time)))
      (unwind-protect (progn ,@body)
        (push (list ',label ,start (get-internal-run-time)) *timing-data*)))))

(defun timing-data-clear ()
  (declare (special *timing-data*))
  (setf *timing-data* ()))

(defun timing-data-show ()
  (loop 
     for (label time count time-per %-of-total) in (timing-data-compile) 
     do (format t "~3d% ~a: ~d ticks over ~d calls for ~d per.~%" 
                %-of-total label time count time-per)))

(defun timing-data-compile () 
  ;; (declare (special *timing-data*))
  (loop 
     with timing-table = (make-hash-table)
     with count-table  = (make-hash-table)
     for (label start end) in *timing-data*
     for time = (- end start)
     summing time into total
     do
     (incf (gethash label timing-table 0) time)
     (incf (gethash label count-table 0))
     finally (return
               (sort (loop 
                        for label being the hash-keys in timing-table 
                        collect (let  ((time (gethash label timing-table))
                                       (count (gethash label count-table)))
                                  (list label
                                        time 
                                        count 
                                        (round (/ time count))
                                        (round (* 100 (/ time total))))))
                     #'> :key #'fifth))))


;;; ==============================
;;; :TIMING-DOCUMENTATION
;;; ==============================

(mon:fundoc 'with-timing-collected
" <DOCSTR> ~%~@
To profile evaluation of a form wrap it in `with-timing-collected'.
Each time BODY form is executed, its start time and end time are recorded,
LABEL is a symbol to associate with form.
Access teh collected timing data with `timing-data-show'.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `with-timing-collected', `timing-data-show', `timing-data-clear',
`timing-data-compile', `*timing-data*'.~%►►►")

(mon:fundoc 'timing-data-compile
" <DOCSTR> ~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `with-timing-collected', `timing-data-show', `timing-data-clear',
`timing-data-compile', `*timing-data*'.~%►►►")

(mon:fundoc 'timing-data-show
"Dump a table showing how much time was spent in different labeled sections of
code evaluated insed the `with-timing-collected' macro.
:EXAMPLE~%
 \(show-timing-data\)
 => 84% BAR: 650 ticks over 2 calls for 325 per.
    16% FOO: 120 ticks over 5 calls for 24 per.
    NIL~%~@
:SEE-ALSO `with-timing-collected', `timing-data-show', `timing-data-clear',
`timing-data-compile', `*timing-data*'.~%►►►")

(mon:fundoc 'timing-data-clear
"Set `mon-tests::*timing-data*' nil.~%~@
:EXAMPLE~%~@
 { ... <EXAMPLE> ... } ~%~@
:SEE-ALSO `with-timing-collected', `timing-data-show', `timing-data-clear',
`timing-data-compile', `*timing-data*'.~%►►►")


;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon-test
;; End:


;;; ==============================
;;; EOF
