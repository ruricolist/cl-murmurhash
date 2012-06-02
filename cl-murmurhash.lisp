;;;; cl-murmurhash.lisp

(in-package #:cl-murmurhash)

(deftype octet () '(unsigned-byte 8))

(deftype word () '(unsigned-byte 32))

(deftype hash () 'word)

(defparameter *default-seed* #xdeadbeef) ;It had to be something.

;; Utilities

(declaim (inline word+ word* rotl))

(declaim (ftype (function (word word) word) word+ word* rotl))

(defun word+ (a b)
  (declare (type word a b))
  (ldb (byte 32 0) (+ a b)))

(defun word* (a b)
  (declare (type word a b))
  (ldb (byte 32 0) (* a b)))

(defun rotl (a s)
  (declare (type word a s))
  (ldb (byte 32 0) (logior (ash a s) (ash a (- s 32)))))

;; The mixer and finalizer.

(declaim (inline mix avalanche finalize))

(declaim (ftype (function (word word) word) mix))

(defun mix (hash-state word)
  "Mix WORD and HASH-STATE into a new hash state, then mix that hash
state again."
  (declare (type hash hash-state)
           (type word word))
  (let* ((w1 (word* word #xcc9e2d51))
         (w2 (rotl w1 15))
         (w3 (word* w2 #x1b873593))
         (h1 (logxor hash-state w3))
         (h2 (rotl h1 13))
         (h3 (word* h2 5))
         (h4 (word+ h3 #xe6546b64)))
    (declare (type word w1 w2 w3)
             (type hash h1 h2 h3 h4))
    h4))

(define-modify-macro mixf (word) mix)

(declaim (ftype (function (word) word) avalanche))

(defun avalanche (hash-state)
  "Force bits of HASH-STATE to avalanche."
  (declare (type hash hash-state))
  (let* ((h0 hash-state)
         (h1 (logxor h0 (ldb (byte 16 16) h0)))
         (h2 (word* h1 #x85ebca6b))
         (h3 (logxor h2 (ldb (byte 19 13) h2)))
         (h4 (word* h3 #xc2b2ae35))
         (h5 (logxor h4 (ldb (byte 16 16) h4))))
    (declare (type hash h0 h1 h2 h3 h4 h5))
    h5))

(defun finalize (hash-state length)
  (let ((hash hash-state))
    (declare (type hash hash))
    (setf hash (avalanche (logxor hash length)))
    hash))

(defun hash-integer (integer seed)
  (let ((hash seed))
    (declare (type hash hash))
    (dotimes (i (ceiling (integer-length integer) 32))
      (mixf hash (ldb (byte 32 (* i 32)) integer)))
    hash))

(defun hash-string (string seed)
  (let ((hash seed) (seq (make-string 4)))
    (declare (type hash hash))
    (with-input-from-string (s string)
      (do ((chars (read-sequence seq s)
                  (read-sequence seq s))
           (word 0))
          ((zerop chars) nil)
        (declare (dynamic-extent word)
                 (type word word)
                 (type (integer 0 4) chars)
                 (optimize speed))
        (dotimes (i chars)
          (setf (ldb (byte 8 (* 8 i)) word)
                (ldb (byte 8 0) (char-code (schar seq i)))))
        (mixf hash word)))
    hash))

(defun hash-octets (vector seed)
  (let ((hash seed) (seq (make-array 4 :element-type 'octet)))
    (declare (type hash hash))
    (flexi-streams:with-input-from-sequence (v vector)
      (do ((octets (read-sequence seq v)
                   (read-sequence seq v))
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

(define-condition unhashable-object-error (error)
  ((object :initarg :object))
  (:report (lambda (condition stream)
             (format stream "Don't know how to hash ~S"
                     (slot-value condition 'object)))))

(defgeneric murmurhash (object &key)
  (:documentation "Hash OBJECT using the 32-bit MurmurHash3 algorithm.")
  (:method ((object t) &key)
    (error 'unhashable-object-error :object object)))

(defmethod murmurhash ((i integer) &key (seed *default-seed*) mix-only)
  (let ((hash (hash-integer i seed)))
    (declare (type hash hash))
    (if mix-only
        hash
        (finalize hash (integer-length i)))))

(defmethod murmurhash ((s string) &key (seed *default-seed*) mix-only)
  (let ((hash (hash-string s seed)))
    (declare (type hash hash))
    (if mix-only
        hash
        (finalize hash (length s)))))

;; Other methods are special cases of integer or string.

(defmethod murmurhash ((c character) &key (seed *default-seed*) mix-only)
  (murmurhash (char-code c) :seed seed :mix-only mix-only))

(defmethod murmurhash ((p package) &key (seed *default-seed*) mix-only)
  (murmurhash (package-name p) :seed seed :mix-only mix-only))

(defmethod murmurhash ((s symbol) &key (seed *default-seed*) mix-only)
  (let ((*package* (find-package :keyword)))
    (murmurhash (prin1-to-string s) :seed see :mix-only mix-only)))

(defmethod murmurhash ((n ratio) &key (seed *default-seed*) mix-only)
  (let ((hash seed))
    (declare (type hash hash))
    (mixf hash (hash-integer (numerator n) hash))
    (mixf hash (hash-integer (denominator n) hash))
    (if mix-only hash
        (finalize hash (+ (integer-length (numerator n))
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
        (finalize hash (+ (integer-length (realpart n))
                          (integer-length (imagpart n)))))))

(defmethod murmurhash ((bv bit-vector) &key (seed *default-seed*) mix-only)
  (let ((int 0))
    ;; Presume little-endian.
    (dotimes (i (array-total-size bv))
      (setf (ldb (byte 1 i) int) (row-major-aref bv i)))
    (murmurhash int :seed seed :mix-only mix-only)))

;; HACK http://stackoverflow.com/a/6083441
(defmethod murmurhash ((ov #.(class-of (make-array 0 :element-type 'octet)))
                       &key (seed *default-seed*) mix-only)
  (let ((hash (hash-octets ov seed)))
    (declare (type hash hash))
    (if mix-only
        hash
        (finalize hash (length ov)))))

(defmethod murmurhash ((cons cons) &key (seed *default-seed*) mix-only)
  (let ((hash seed))
    (declare (type hash hash))
    (dolist (elt cons)
      (mixf hash (murmurhash elt :seed hash :mix-only t)))
    (if mix-only hash (finalize hash (length cons)))))

(defmethod murmurhash ((array array) &key (seed *default-seed*) mix-only)
  (let ((hash seed))
    (declare (type hash hash))
    (mixf hash (hash-integer (array-rank array) seed))
    (mixf hash (hash-integer (array-dimensions array) seed))
    (mixf hash (murmurhash (array-element-type array) :seed seed :mix-only t))
    (loop for elt across array
          do (mixf hash (murmurhash elt :seed hash :mix-only t)))
    (if mix-only hash (finalize hash (array-total-size array)))))

(defmethod murmurhash ((ht hash-table) &key (seed *default-seed*) mix-only)
  (let ((hash seed))
    (declare (type hash hash))
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
