;;; :FILE-CREATED <Timestamp: #{2011-03-23T14:53:49-04:00Z}#{11123} - by MON>
;;; :FILE mon-systems/tests/timings.lisp
;;; ==============================

(in-package #:mon-test)
;; *package*

    
(defun timing-number-to-byte-array (iterations)
  (declare ((integer 1 *) iterations))
  (loop 
     repeat iterations
     for rnd across (mon-test::make-random-inverted-number-array)
     do (mon:number-to-byte-array rnd)))

(defun timing-number-to-bits (iterations)
  (declare ((integer 1 *) iterations))
  (loop 
     repeat iterations
     for rnd across (make-random-inverted-number-array)
     do (mon:number-to-bits rnd)))

(defun timing-octet-to-bit-vector (times)
  (dotimes (i times)
    (dotimes (i 256)
      (mon::octet-to-bit-vector i))))

(defun timing-tt--octet-to-bit-vector (times)
  (dotimes (i times)
    (dotimes (i 256)
      (mon::tt--octet-bit-vector (random 256)))))

(defun timing-tt--octet-to-bit-vector-3 (times)
  (dotimes (i times)
    (dotimes (i 256)
      (mon::tt--octet-to-bit-vector-3 (random 256)))))

(with-timing-collected 'timing-tt--octet-to-bit-vector-3
  (timing-tt--octet-to-bit-vector-3 1000000))

(with-timing-collected 'timing-tt--octet-to-bit-vector
  (timing-tt--octet-to-bit-vector 1000000))

(with-timing-collected 'timing-tt--octet-to-bit-vector
  (timing-octet-to-bit-vector 1000000))


;; Neither is worth testing at 10 million iterations...
(with-timing-collected 'timing-number-to-bits 
    (timing-number-to-bits 10000000))

(with-timing-collected 'timing-number-to-byte-array
  (timing-number-to-bits 10000000))

;;; ==============================
;; (time 
;;   (loop 
;;      repeat 1000 
;;      for rnd across (mon-test::make-random-inverted-number-array)
;;        do (tt--number-to-byte-array rnd)))
       
;;(mon-test:with-timing-collected 


;;; ==============================
;;; EOF
