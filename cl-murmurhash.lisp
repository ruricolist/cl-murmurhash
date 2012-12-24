;;;; cl-murmurhash.lisp

(in-package #:cl-murmurhash)

(deftype octet () '(unsigned-byte 8))

(deftype word () '(unsigned-byte 32))

(deftype hash () 'word)

(defparameter *default-seed* #xdeadbeef) ;It had to be something.

(defparameter *hash-size* 32)

;; Utilities

;; N.B. * and + are shadowed.

(declaim (inline + * << ^))

(defun + (a b)
  (declare (type word a b))
  (ldb (byte 32 0) (cl:+ a b)))

(define-modify-macro += (addend) +)

(defun * (a b)
  (declare (type word a b))
  (ldb (byte 32 0) (cl:* a b)))

(define-modify-macro *= (multiplicand) *)

#-sbcl
(defun << (a s)
  (declare (type word a s))
  (ldb (byte 32 0) (logior (ash a s) (ash a (- s 32)))))

#+sbcl
(progn
  (require :sb-rotate-byte)
  (defun << (a s)
    (declare (word a s))
    (sb-rotate-byte:rotate-byte s (byte 32 0) a)))

(define-modify-macro <<= (rotation) <<)

(declaim (ftype (function (word word) word) ^))

(defun ^ (a b)
  (declare (type word a b))
  (logxor a b))

(define-modify-macro ^= (int) ^)

;; The mixer and finalizer.

(declaim (inline seed seeds mix avalanche finalize
                 hash-integer hash-octets))

(defun mix (hash-state word)
  (ecase *hash-size*
    (32 (mix/32 hash-state word))
    (128 (mix/128 hash-state word))))

(define-modify-macro mixf (word) mix)

(defun mix/32 (hash-state word)
  "Mix WORD and HASH-STATE into a new hash state, then mix that hash
state again."
  (declare (type hash hash-state)
           (type word word))
  (let* ((w1 (* word #xcc9e2d51))
         (w2 (<< w1 15))
         (w3 (* w2 #x1b873593))
         (h1 (logxor hash-state w3))
         (h2 (<< h1 13))
         (h3 (* h2 5))
         (h4 (+ h3 #xe6546b64)))
    (declare (type word w1 w2 w3)
             (type hash h1 h2 h3 h4))
    h4))

(defun seed (h1 h2 h3 h4)
  (list h1 h2 h3 h4))

(defun seeds (seed)
  (if (numberp seed)
      (values seed seed seed seed)
      (values-list seed)))

(let ((c1 #x239b961b)
      (c2 #xab0e9789)
      (c3 #x38b34ae5)
      (c4 #xa1e38b93))
  (declare (word c1 c2 c3 c4))
  (defun mix/128 (seed word)
    (multiple-value-bind (h1 h2 h3 h4)
        (seeds seed)
      (multiple-value-bind (k1 k2 k3 k4)
          (seeds word)
        (declare (hash h1 h2 h3 h4)
                 (word k1 k2 k3 k4))
        (*= k1 c1)  (<<= k1 15) (*= k1 c2) (^= h1 k1)
        (<<= h1 19) (+= h1 h2)  (*= h1 (+ 5 #x561ccd1b))
        (*= k2 c2)  (<<= k2 16) (*= k2 c3) (^= h2 k2)
        (<<= h2 17) (+= h2 h3)  (*= h2 (+ 5 #x0bcaa747))
        (*= k3 c3)  (<<= k3 17) (*= k3 c4) (^= h3 k3)
        (<<= h3 15) (+= h3 h4)  (*= h3 (+ 5 #x96cd1c35))
        (*= k4 c4)  (<<= k4 18) (*= k4 c1) (^= h4 k4)
        (<<= h4 13) (+= h1 h1)  (*= h4 (+ 5 #x32ac3b17))
        (seed h1 h2 h3 h4)))))

(declaim (ftype (function (word) word) avalanche))

(defun avalanche (hash-state)
  "Force bits of HASH-STATE to avalanche."
  (declare (type hash hash-state))
  (let* ((h0 hash-state)
         (h1 (logxor h0 (ldb (byte 16 16) h0)))
         (h2 (* h1 #x85ebca6b))
         (h3 (logxor h2 (ldb (byte 19 13) h2)))
         (h4 (* h3 #xc2b2ae35))
         (h5 (logxor h4 (ldb (byte 16 16) h4))))
    (declare (type hash h0 h1 h2 h3 h4 h5))
    h5))

(defun finalize (hash-state length)
  (ecase *hash-size*
    (32 (finalize/32 hash-state length))
    (128 (finalize/128 hash-state length))))

(defun finalize/32 (hash-state length)
  (let ((hash hash-state))
    (declare (type hash hash))
    (setf hash (avalanche (logxor hash length)))
    hash))

(defun finalize/128 (seed length)
  (multiple-value-bind (h1 h2 h3 h4)
      (seeds seed)
    (declare (hash h1 h2 h3 h4))
    (macrolet ((inc ()
                 `(progn
                    (+= h1 h2) (+= h1 h3) (+= h1 h4)
                    (+= h2 h1) (+= h3 h1) (+= h4 h1))))
      (^= h1 length) (^= h2 length)
      (^= h3 length) (^= h4 length)
      (inc)
      (setf h1 (avalanche h1)
            h2 (avalanche h2)
            h3 (avalanche h3)
            h4 (avalanche h4))
      (inc))
    (let ((out 0))
      (setf (ldb (byte 32 0) out) h1
            (ldb (byte 32 32) out) h2
            (ldb (byte 32 64) out) h3
            (ldb (byte 32 96) out) h4)
      out)))

(defun hash-integer (int seed)
  (ecase *hash-size*
    (32 (hash-integer/32 int seed))
    (128 (hash-integer/128 int seed))))

(defun hash-octets (vec seed)
  (ecase *hash-size*
    (32 (hash-octets/32 vec seed))
    (128 (hash-octets/128 vec seed))))

(defun hash-integer/32 (integer seed)
  (let ((hash seed))
    (dotimes (i (ceiling (integer-length integer) 32))
      (mixf hash (ldb (byte 32 (* i 32)) integer)))
    hash))

(defun hash-octets/32 (vector seed)
  (let ((hash seed) (seq (make-array 4 :element-type 'octet)))
    (fast-io:with-fast-input (v vector)
      (do ((octets (fast-io:fast-read-sequence seq v)
                   (fast-io:fast-read-sequence seq v))
           (word 0))
          ((zerop octets) nil)
        (declare (dynamic-extent word)
                 (type word word)
                 (type (integer 0 4) octets)
                 (optimize speed))
        (dotimes (i octets)
          (setf (ldb (byte 8 (* 8 i)) word)
                (aref seq i)))
        (mixf hash word)))
    hash))

(defmacro while (test &body body)
  `(loop (unless ,test (return))
         ,@body))

(defun hash-integer/128 (integer seed)
  (multiple-value-bind (h1 h2 h3 h4)
      (seeds seed)
    (declare (hash h1 h2 h3 h4))
    (let ((blocks (ceiling (integer-length integer) 32))
          (i 0))
      (labels ((extract (position)
                 (ldb (byte 32 position) integer)))
        (declare (inline extract))
        (let ((k1 0) (k2 0) (k3 0) (k4 0))
          (declare (word k1 k2 k3 k4)
                   (dynamic-extent k1 k2 k3 k4)
                   (optimize speed))
          (while (< i blocks)
            (setf k1 (extract 0)
                  k2 (extract 32)
                  k3 (extract 64)
                  k4 (extract 96))
            (setf (values h1 h2 h3 h4)
                  (seeds (mix/128 (seed h1 h2 h3 h4)
                                  (seed k1 k2 k3 k4))))
            (incf i 4)))))
    (seed h1 h2 h3 h4)))

(defun snarf-word (stream seq)
  (let ((k 0))
    (declare (word k))
    (dotimes (i (fast-io:fast-read-sequence seq stream))
      (declare ((integer 0 4) i))
      (setf (ldb (byte 8 (* 8 i)) k)
            (aref seq i)))
    k))

(defun hash-octets/128 (vector seed)
  (multiple-value-bind (h1 h2 h3 h4)
      (seeds seed)
    (let ((seq (make-array 4 :element-type 'octet)))
      (declare (hash h1 h2 h3 h4))
      (declare (inline snarf-word))
      (fast-io:with-fast-input (v vector)
        (let ((k1 0) (k2 0) (k3 0) (k4 0))
          (declare (dynamic-extent k1 k2 k3 k4)
                   (word k1 k2 k3 k4)
                   (optimize speed))
          (while (fast-io:fast-peek-byte v nil nil nil)
            (setf k1 (snarf-word v seq)
                  k2 (snarf-word v seq)
                  k3 (snarf-word v seq)
                  k4 (snarf-word v seq))
            (setf (values h1 h2 h3 h4)
                  (seeds (mix/128 (seed h1 h2 h3 h4)
                                  (seed k1 k2 k3 k4)))))))
      (seed h1 h2 h3 h4))))

(define-condition unhashable-object-error (error)
  ((object :initarg :object))
  (:report (lambda (condition stream)
             (format stream "Don't know how to hash ~S"
                     (slot-value condition 'object)))))

(defgeneric murmurhash (object &key &allow-other-keys)
  (:documentation "Hash OBJECT using the 32-bit MurmurHash3 algorithm.")
  (:method ((object t) &key)
    (error 'unhashable-object-error :object object)))

(defmethod murmurhash ((i integer) &key (seed *default-seed*) mix-only)
  (let ((hash (hash-integer i seed)))
    (if mix-only
        hash
        (finalize hash (integer-length i)))))

;; HACK http://stackoverflow.com/a/6083441
(defmethod murmurhash ((ov #.(class-of (make-array 0 :element-type 'octet)))
                       &key (seed *default-seed*) mix-only)
  (let ((hash (hash-octets ov seed)))
    (if mix-only
        hash
        (finalize hash (length ov)))))

;; Other methods are special cases of integers or octets.

(defmethod murmurhash ((s string) &key (seed *default-seed*) mix-only)
  (murmurhash
   (babel:string-to-octets s)
   :seed seed :mix-only mix-only))

(defmethod murmurhash ((c character) &key (seed *default-seed*) mix-only)
  (murmurhash (char-code c) :seed seed :mix-only mix-only))

(defmethod murmurhash ((p package) &key (seed *default-seed*) mix-only)
  (murmurhash (package-name p) :seed seed :mix-only mix-only))

(defmethod murmurhash ((s symbol) &key (seed *default-seed*) mix-only)
  (let ((*package* (find-package :keyword)))
    (murmurhash (prin1-to-string s) :seed seed :mix-only mix-only)))

(defmethod murmurhash ((n ratio) &key (seed *default-seed*) mix-only)
  (let ((hash seed))
    (mixf hash (hash-integer (numerator n) hash))
    (mixf hash (hash-integer (denominator n) hash))
    (if mix-only hash
        (finalize hash (cl:+ (integer-length (numerator n))
                             (integer-length (denominator n)))))))

(defmethod murmurhash ((n float) &key (seed *default-seed*) mix-only)
  (murmurhash (rational n) :seed seed :mix-only mix-only))

(defmethod murmurhash ((n complex) &key (seed *default-seed*) mix-only)
  (let ((hash seed))
    (declare (type hash hash))
    (mixf hash (hash-integer (realpart n) hash))
    (mixf hash (hash-integer (imagpart n) hash))
    (if mix-only
        hash
        (finalize hash (cl:+ (integer-length (realpart n))
                             (integer-length (imagpart n)))))))

(defmethod murmurhash ((bv bit-vector) &key (seed *default-seed*) mix-only)
  (let ((int 0))
    ;; Presume little-endian.
    (dotimes (i (array-total-size bv))
      (setf (ldb (byte 1 i) int) (row-major-aref bv i)))
    (murmurhash int :seed seed :mix-only mix-only)))

(defmethod murmurhash ((cons cons) &key (seed *default-seed*) mix-only)
  (let ((hash seed)
        (len 0))
    (declare (fixnum len))
    (loop (cond ((null cons) (return))
                ((atom cons)
                 (incf len)
                 (mixf hash (murmurhash cons :seed hash :mix-only t))
                 (return))
                (t (incf len)
                   (mixf hash (murmurhash (car cons) :seed hash :mix-only t))
                   (setf cons (cdr cons)))))
    (if mix-only hash (finalize hash len))))

(defmethod murmurhash ((array array) &key (seed *default-seed*) mix-only)
  (let ((hash seed))
    (mixf hash (hash-integer (array-rank array) seed))
    (mixf hash (murmurhash (array-dimensions array) :seed seed :mix-only t))
    (mixf hash (murmurhash (array-element-type array) :seed seed :mix-only t))
    (loop for elt across array
          do (mixf hash (murmurhash elt :seed hash :mix-only t)))
    (if mix-only hash (finalize hash (array-total-size array)))))

(defmethod murmurhash ((ht hash-table) &key (seed *default-seed*) mix-only)
  (let ((hash seed))
    (mixf hash (murmurhash (hash-table-test ht) :seed hash :mix-only t))
    (maphash
     (lambda (k v)
       (mixf hash (murmurhash k :seed hash :mix-only t))
       (mixf hash (murmurhash v :seed hash :mix-only t)))
     ht)
    (if mix-only hash (finalize hash (hash-table-count ht)))))

(defmethod murmurhash ((p pathname) &key (seed *default-seed*) mix-only)
  (murmurhash (namestring p) :seed seed :mix-only mix-only))

;; Cf. http://bitsquid.blogspot.com/2009/09/simple-perfect-murmur-hashing.html
(defun make-perfect-seed (values)
  "Find a seed that ensures every value in VALUES can be hashed
without any collisions. Useful when you know a set of values in
advance so the hashes can stand for the values.

Return NIL if no perfect seed was found."
  (let ((max (load-time-value (1- (expt 2 32)) t))
        (values (remove-duplicates values)))
    (do ((seed 0 (1+ seed))
         (ht (make-hash-table) (make-hash-table)))
        ((= seed max) nil)
      (dolist (value values)
        (let ((hash (murmurhash value :seed seed)))
          (multiple-value-bind (stored hit) (gethash hash ht)
            (if hit (unless (eql stored value) (return nil))
                (setf (gethash hash ht) value))))
        (return-from make-perfect-seed seed)))))

(defun make-perfect-hash-function (values)
  "Find a perfect seed for VALUES and return a partial application of
MURMURHASH to that seed.

Return NIL if no perfect seed was found."
  (let ((seed (make-perfect-seed values)))
    (when seed
      (lambda (key) (murmurhash key :seed seed)))))

(defun test (&key ((:hash-size *hash-size*) 32))
  (time
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
             syms collisions))))
