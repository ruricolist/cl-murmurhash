(defpackage :cl-murmurhash/test
  (:use :cl :murmurhash :fiveam)
  (:export :run-tests))
(in-package :cl-murmurhash/test)

(deftype octet () '(unsigned-byte 8))

(defun hash-symbols (&key ((:hash-size *hash-size*) 32))
  "Sanity check: hash all symbols and see how many collisions result."
  (let ((ht (make-hash-table :test 'equal))
        (collisions 0)
        (syms 0))
    (do-all-symbols (sym)
      (incf syms)
      (let* ((str (string sym))
             (hash (murmurhash str)))
        (if (gethash hash ht)
            (unless (string= (gethash hash ht) str)
              (incf collisions))
            (setf (gethash hash ht) str))))
    (format *trace-output* "Hashed ~D symbols with ~D collision~:P"
            syms collisions)))

;;; http://code.google.com/p/smhasher/source/browse/trunk/KeysetTest.cpp

(defconstant +32-bit-verification+ #xb0f57ee3) ;2968878819

(defconstant +128-bit-verification+ #xb3ece62a) ;3018647082

(defun verification-value (hashbits)
  (let ((hash (ecase hashbits
                (32 #'murmurhash::hash32-octets)
                (128 #'murmurhash::hash128-octets))))
    (flet ((hash (&rest args) (apply hash args)))
      (let* ((*hash-size* hashbits)
             (hashbytes (ceiling hashbits 8))
             (key (make-array 256 :element-type 'octet :initial-element 0))
             (hashes (make-array (* hashbytes 256) :element-type 'octet :initial-element 0)))
        (loop for i from 0 below 256
              for j from 0 by hashbytes
              do (setf (aref key i) i)
                 (let ((hash (hash (subseq key 0 (1+ i))
                                   (- 256 i)
                                   i    ;sic
                                   )))
                   (loop for k from 0 below hashbytes
                         do (setf (aref hashes (+ j k))
                                  (ldb (byte 8 (* 8 k)) hash)))))
        (let ((final (hash hashes 0 (* hashbytes 256))))
          (ldb (byte 32 0) final))))))

(def-suite murmurhash)
(in-suite murmurhash)

(test verify-32
  (is (= +32-bit-verification+ (verification-value 32))))

(test verify-128
  (is (= +128-bit-verification+ (verification-value 128))))

(defun run-tests ()
  (hash-symbols)
  (5am:run! 'murmurhash))
