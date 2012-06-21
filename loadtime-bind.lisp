;;; :FILE-CREATED <Timestamp: #{2011-03-16T18:10:01-04:00Z}#{11113} - by MON>
;;; :FILE mon-systems/loadtime-bind.lisp
;;; ==============================


;;; ==============================
;;; 
;;; :NOTE Put a consed pair in a file (i.e. mon-systems/loadtime-bind)
;;; with the format:
;;;  ("<CHECK-NAME>" . "<BIND-NAME>")
;;; "<CHECK-NAME>" is a string matching both `sb-posix:passwd-name' and
;;; the final directory component of `cl:user-homedir-pathname'
;;; "<BIND-NAME>" is a string identifying the current-user in an alternative or
;;; more verbose form than that specified to passwd e.g. "MY-MONIKER/NICKNAME"
;;;
;;; The consed pair in that file can be read in at system loadtime and used as
;;; the VERIFY-WITH arg to `mon:username-for-system-var-p' which will examine
;;; its car to verify that  if it does the consed
;;; pair is then passed on to `mon:username-for-system-var-bind' which will set
;;; the value of `mon:*user-name*' to its cdr.
;;; 
;;; :FILE mon-systems/mon.asd 
;;; :FILE mon-systems/specials.lisp
;;; :FILE mon-systems/file-io.lisp 
;;; :FILE mon-systems/loadtime-bind.lisp
;;;
;;; ==============================


(in-package #:mon)
;; *package*

(setq *user-name*
      #-IS-MON (probe-file (merge-pathnames (make-pathname :name "loadtime-bind") 
                                            (load-time-value *default-pathname-defaults*)))
      #+IS-MON (probe-file (translate-logical-pathname "MON:MON-SYSTEMS;loadtime-bind")))

(username-for-system-var-bind 'mon:*user-name*)

#+IS-MON
(rplacd (last *timestamp-for-file-header-format*) (list (cdr *user-name*) #\>))
#-IS-MON
(rplacd (last *timestamp-for-file-header-format*) (if (cdr *user-name*) 
                                                      (list (cdr *user-name*) #\>)
                                                      (list #\>)))
;; (setq *timestamp-for-file-header-format*
;;       `("<Timestamp: #{" 
;;         (:year 4) #\- (:month 2) #\- (:day 2) #\T (:hour 2) #\: (:min 2) #\: (:sec 2) :gmt-offset 
;;         "} - by " ,(cdr *user-name*)))

;;; ==============================


;; Local Variables:
;; indent-tabs-mode: nil
;; show-trailing-whitespace: t
;; mode: lisp-interaction
;; package: mon
;; End:

;;; ==============================
;;; EOF

