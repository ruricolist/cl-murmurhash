;;;; cl-murmurhash.lisp

(in-package #:cl-murmurhash)

(eval-when (:compile-toplevel :load-toplevel)

  (deftype octet () '(unsigned-byte 8))

  (deftype u32 () '(unsigned-byte 32))

  (deftype octet-vector (&optional n)
    `(simple-array (unsigned-byte 8) (,n))))

(deftype -> (args result) `(function ,args ,result))

(deftype index () '(integer 0 #.array-dimension-limit))

(deftype 8-bit-string ()
  '(and simple-base-string (satisfies 8-bit-string?)))

(defun 8-bit-string? (s)
  (and (typep s 'simple-base-string)
       (every (lambda (c) (< (char-code c) 256)) s)))

(defparameter *default-seed* #xdeadbeef) ;It had to be something.

(defparameter *hash-size* 32)

(declaim (type (member 32 128) *hash-size*))


;;;; Utilities

;; N.B. * and + are shadowed.

(deftype mod32 ()
  '(-> (u32 u32) u32))

(defmacro mod32 (x)
  `(logand ,x #xffffffff))

(declaim (inline + * << >> rotl ^))

(declaim (ftype mod32 + * rotl << >> ^))

(defun + (a b)
  (declare (type u32 a b))
  (ldb (byte 32 0) (cl:+ a b)))

(define-modify-macro += (addend) +)

(defun * (a b)
  (declare (type u32 a b))
  (mod32 (cl:* a b)))

(define-modify-macro *= (multiplicand) *)

#-sbcl
(defun rotl (a s)
  (declare (type u32 a s))
  (logior (ldb (byte 32 0) (ash a s))
          (ldb (byte 32 0)
               (ash a (- 0 (- 32 s))))))

#+sbcl
(defun rotl (a s)
  (declare (u32 a s))
  (mod32 (sb-rotate-byte:rotate-byte s (byte 32 0) a)))

(define-modify-macro rotlf (s) rotl)

(defun << (a s)
  (mod32 (ash a s)))

(defun >> (a s)
  (mod32 (ash a (- s))))

(defun ^ (x y)
  (mod32 (logxor x y)))

(define-modify-macro ^= (int) ^)

(defmacro ~> (needle &rest holes)
  (flet ((enlist (x)
           (if (listp x) x (list x))))
    (if (not holes)
        needle
        `(~> ,(let ((hole (if (listp (first holes))
                              (first holes)
                              (list (first holes)))))
                `(,(car hole) ,needle ,@(cdr hole)))
             ,@(rest holes)))))


;;;; The actual implementation.

(declaim (ftype (-> (u32) u32) fmix32)
         (inline fmix32))

(defun fmix32 (h)
  (declare (u32 h))
  (^= h (ash h -16))
  (*= h #x85ebca6b)
  (^= h (ash h -13))
  (*= h #xc2b2ae35)
  (^= h (ash h -16)))

(define-modify-macro avalanche () fmix32)

(declaim (ftype (-> (integer (integer 0 cl:*)) (unsigned-byte 8)) byte-ref))

(defun byte-ref (int pos)
  (ldb (byte 8 (* pos 8)) int))

(defun byte-length (int)
  (ceiling (integer-length int) 8))

(declaim (ftype (-> (simple-string index) (unsigned-byte 8)) char-ref))

(defun char-ref (string pos)
  (declare (optimize speed) (8-bit-string string) (index pos))
  (char-code (schar string pos)))

;; http://www.foldr.org/~michaelw/log/programming/lisp/icfp-contest-2006-vm
(defmacro tree-case (keyform &body cases)
  (let* ((default (find t cases :key #'car))
         (cases (remove default cases)))
    (labels ((case-tree (keyform cases)
               (if (<= (length cases) 4)
                   (if default
                       `(case ,keyform
                          ,@cases
                          ,default)
                       `(ecase ,keyform
                          ,@cases))
                   (let* ((rest-cases (nthcdr (truncate (length cases) 2) cases))
                          (first-half (ldiff cases rest-cases)))
                     `(if (< ,keyform ,(caar rest-cases))
                          ,(case-tree keyform first-half)
                          ,(case-tree keyform rest-cases))))))
      (alexandria:with-gensyms (kf)
        `(let ((,kf ,keyform))
           ,(case-tree kf (sort (copy-list cases) #'< :key #'first)))))))

(defmacro switch (value &body clauses)
  (let ((break (gensym)))
    `(macrolet ((break ()
                  `(go ,',break)))
       (tagbody
          (tree-case ,value
            ,@(loop for (tag . body) in clauses
                    collect `(,tag (go ,tag)))
            (t (break)))
          ,@(apply #'append clauses)
          ,break))))

(defmacro hash32-body (ref)
  (let ((c1 #xcc9e2d51)
        (c2 #x1b873593))
    `(let* ((end (logand #xfffffffc len))
            (h1 seed))
       (declare (index end) (u32 h1))
       (loop for i of-type index below end by 4 do
         (setf h1
               (let* ((k1 (~> (mod32 (logior (,ref vec i)
                                             (ash (,ref vec (+ i 1)) 8)
                                             (ash (,ref vec (+ i 2)) 16)
                                             (ash (,ref vec (+ i 3)) 24)))
                              (* ,c1)
                              (rotl 15)
                              (* ,c2)))
                      (h1 (~> h1
                              (^ k1)
                              (rotl 13)
                              (* 5)
                              (+ #xe6546b64))))
                 (declare (u32 k1 h1))
                 h1)))
       (let ((k1 0))
         (declare (u32 k1))
         (switch (logand len 3)
           (3 (^= k1 (ash (,ref vec (+ end 2)) 16)))
           (2 (^= k1 (ash (,ref vec (+ end 1)) 8)))
           (1 (^= k1 (,ref vec end))
              (*= k1 ,c1)
              (rotlf k1 15)
              (*= k1 ,c2)
              (^= h1 k1))))
       (unless mix-only
         (^= h1 len)
         (avalanche h1))
       h1)))

(locally (declare (optimize (speed 3) (safety 0) (debug 0)))

  (defun hash32-octets (vec seed &optional (len (length vec)) mix-only)
    (declare (index len) (octet-vector vec))
    (hash32-body aref))

  (defun hash32-integer (vec seed &optional (len (byte-length vec)) mix-only)
    (declare (integer vec) ((integer 0 cl:*) len))
    (if (typep vec 'u32)
        (locally (declare (type (integer 0 4) len))
          (hash32-body byte-ref))
        (hash32-body byte-ref)))

  (defun hash32-8-bit-string (vec seed &optional (len (length vec)) mix-only)
    (declare (index len) (simple-string vec))
    (hash32-body char-ref)))

(defmacro hash128-body (ref)
  (let ((c1 #x239b961b)
        (c2 #xab0e9789)
        (c3 #x38b34ae5)
        (c4 #xa1e38b93))
    `(multiple-value-bind (h1 h2 h3 h4)
         (if (<= (integer-length seed) 32)
             (values seed seed seed seed)
             (values
              (ldb (byte 32 0) seed)
              (ldb (byte 32 32) seed)
              (ldb (byte 32 64) seed)
              (ldb (byte 32 96) seed)))
       (declare (u32 h1 h2 h3 h4))
       (let ((end (logand #xfffffff0 len)))
         (macrolet ((getblock (i)
                      `(mod32 (logior (,',ref vec ,i)
                                      (ash (,',ref vec (+ ,i 1)) 8)
                                      (ash (,',ref vec (+ ,i 2)) 16)
                                      (ash (,',ref vec (+ ,i 3)) 24)))))
           (loop for i from 0 below end by 16 do
             (setf (values h1 h2 h3 h4)
                   (let* ((k1 (~> (getblock i)
                                  (* ,c1)
                                  (rotl 15)
                                  (* ,c2)))
                          (h1 (~> h1
                                  (^ k1)
                                  (rotl 19)
                                  (+ h2)
                                  (* 5)
                                  (+ #x561ccd1b)))
                          (k2 (~> (getblock (+ i 4))
                                  (* ,c2)
                                  (rotl 16)
                                  (* ,c3)))
                          (h2 (~> h2
                                  (^ k2)
                                  (rotl 17)
                                  (+ h3)
                                  (* 5)
                                  (+ #x0bcaa747)))
                          (k3 (~> (getblock (+ i 8))
                                  (* ,c3)
                                  (rotl 17)
                                  (* ,c4)))
                          (h3 (~> h3
                                  (^ k3)
                                  (rotl 15)
                                  (+ h4)
                                  (* 5)
                                  (+ #x96cd1c35)))
                          (k4 (~> (getblock (+ i 12))
                                  (* ,c4)
                                  (rotl 18)
                                  (* ,c1)))
                          (h4 (~> h4
                                  (^ k4)
                                  (rotl 13)
                                  (+ h1)
                                  (* 5)
                                  (+ #x32ac3b17))))
                     (declare (u32 k1 k2 k3 k4 h1 h2 h3 h4))
                     (values h1 h2 h3 h4))))
           (let ((k4 0) (k3 0) (k2 0) (k1 0))
             (declare (u32 k4 k3 k2 k1)
                      (dynamic-extent k4 k3 k2 k1))
             (macrolet ((tail (n)
                          `(,',ref vec (+ ,n end))))
               (switch (logand len 15)
                 (15 (^= k4 (ash (tail 14) 16)))
                 (14 (^= k4 (ash (tail 13) 8)))
                 (13 (^= k4 (tail 12))
                     (*= k4 ,c4) (rotlf k4 18) (*= k4 ,c1) (^= h4 k4))

                 (12 (^= k3 (ash (tail 11) 24)))
                 (11 (^= k3 (ash (tail 10) 16)))
                 (10 (^= k3 (ash (tail 9)   8)))
                 (9  (^= k3 (tail 8))
                     (*= k3 ,c3) (rotlf k3 17) (*= k3 ,c4) (^= h3 k3))

                 (8 (^= k2 (ash (tail 7) 24)))
                 (7 (^= k2 (ash (tail 6) 16)))
                 (6 (^= k2 (ash (tail 5)  8)))
                 (5 (^= k2 (tail 4))
                    (*= k2 ,c2) (rotlf k2 16) (*= k2 ,c3) (^= h2 k2))

                 (4 (^= k1 (ash (tail 3) 24)))
                 (3 (^= k1 (ash (tail 2) 16)))
                 (2 (^= k1 (ash (tail 1)  8)))
                 (1 (^= k1 (tail 0))
                    (*= k1 ,c1) (rotlf k1 15) (*= k1 ,c2) (^= h1 k1)))))

           (unless mix-only

             (^= h1 len)
             (^= h2 len)
             (^= h3 len)
             (^= h4 len)

             (macrolet ((adds ()
                          `(progn
                             (+= h1 h2) (+= h1 h3) (+= h1 h4)
                             (+= h2 h1) (+= h3 h1) (+= h4 h1))))
               (adds)

               (avalanche h1)
               (avalanche h2)
               (avalanche h3)
               (avalanche h4)

               (adds)))

           (logior h1 (ash h2 32) (ash h3 64) (ash h4 96)))))))

(locally (declare (optimize (speed 3) (safety 0) (debug 0)))

  (defun hash128-octets (vec seed &optional (len (length vec)) mix-only)
    (declare (index len)
             (octet-vector vec))
    (hash128-body aref))

  (defun hash128-integer (vec seed &optional (len (byte-length vec)) mix-only)
    (declare (integer vec))
    (hash128-body byte-ref))

  (defun hash128-8-bit-string (vec seed &optional (len (length vec)) mix-only)
    (declare (index len) (simple-string vec))
    (hash128-body char-ref)))

(declaim (inline hash-string mix32 finalize32 mix128 finalize128))

(defun hash-string (s seed mix-only)
  (let ((size *hash-size*))
    (flet ((hash-string (s)
             (ecase size
               (32 (hash32-8-bit-string s seed (length s) mix-only))
               (128 (hash128-8-bit-string s seed (length s) mix-only))))
           (hash-octets (octets)
             (ecase size
               (32 (hash32-octets octets seed (length octets) mix-only))
               (128 (hash128-octets octets seed (length octets) mix-only)))))
      (declare (dynamic-extent (function hash-string) (function hash-octets)))
      (if (typep s '8-bit-string)
          (hash-string s)
          (hash-octets (babel:string-to-octets s))))))

(defun hash-integer (x seed &optional mix-only)
  (ecase *hash-size*
    (32 (hash32-integer x seed (byte-length x) mix-only))
    (128 (hash128-integer x seed (byte-length x) mix-only))))

(defun hash-octets (x seed &optional mix-only)
  (ecase *hash-size*
    (32 (hash32-octets x seed (length x) mix-only))
    (128 (hash128-octets x seed (length x) mix-only))))

(defun mix (seed x)
  (let ((size *hash-size*))
    (etypecase x
      (octet-vector
       (ecase size
         (32 (hash32-octets x seed (length x) t))
         (128 (hash128-octets x seed (length x) t))))
      (integer
       (ecase size
         (32 (hash32-integer x seed (byte-length x) t))
         (128 (hash128-integer x seed (byte-length x) t))))
      (string
       (hash-string x seed t)))))

(define-modify-macro mixf (x) mix)

(defun finalize (seed x)
  (let ((size *hash-size*))
    (etypecase x
      (octet-vector
       (ecase size
         (32 (hash32-octets x seed (length x)))
         (128 (hash128-octets x seed (length x)))))
      (integer
       (ecase size
         (32 (hash32-integer x seed (byte-length x)))
         (128 (hash128-integer x seed (byte-length x)))))
      (string
       (hash-string x seed nil)))))


;;;; Methods to hash literal objects.

(define-condition unhashable-object-error (error)
  ((object :initarg :object))
  (:report (lambda (condition stream)
             (format stream "Don't know how to hash ~S"
                     (slot-value condition 'object)))))

(defgeneric murmurhash (object &key)
  (:documentation "Hash OBJECT using the 32-bit MurmurHash3 algorithm.")
  (:method ((object t) &key &allow-other-keys)
    (error 'unhashable-object-error :object object)))

(defmethod murmurhash ((i integer) &key (seed *default-seed*) mix-only)
  (hash-integer i seed mix-only))

;; HACK http://stackoverflow.com/a/6083441
(defmethod murmurhash ((ov #.(class-of (make-array 0 :element-type 'octet)))
                       &key (seed *default-seed*) mix-only)
  (hash-octets ov seed mix-only))

(defmethod murmurhash ((s string) &key (seed *default-seed*) mix-only)
  (hash-string s seed mix-only))

;; Other methods are special cases of integers, strings, or octets.

(defmethod murmurhash ((c character) &key (seed *default-seed*) mix-only)
  (hash-integer (char-code c) seed mix-only))

(defmethod murmurhash ((p package) &key (seed *default-seed*) mix-only)
  (hash-string (package-name p) seed mix-only))

(defmethod murmurhash ((s symbol) &key (seed *default-seed*) mix-only)
  (let ((hash seed)
        (pkg (package-name (symbol-package s)))
        (sym (symbol-name s)))
    (mixf hash pkg)
    (mixf hash sym)
    (if mix-only
        hash
        (finalize hash (+ (length pkg) (length sym))))))

(defmethod murmurhash ((p pathname) &key (seed *default-seed*) mix-only)
  (let ((s (namestring p)))
    (hash-string s seed mix-only)))

(defmethod murmurhash ((n ratio) &key (seed *default-seed*) mix-only)
  (let ((n (numerator n))
        (d (denominator n))
        (hash seed))
    (declare (integer n d))
    (mixf hash n)
    (mixf hash d)
    (if mix-only
        hash
        (finalize hash
                  (cl:+ (integer-length (numerator n))
                        (integer-length (denominator n)))))))

(defmethod murmurhash ((n float) &key (seed *default-seed*) mix-only)
  (let ((hash seed))
    (multiple-value-bind (signif expon sign)
        (integer-decode-float n)
      (mixf hash signif)
      (mixf hash expon)
      (mixf hash sign)
      (if mix-only
          hash
          (finalize hash (float-digits n))))))

(defmethod murmurhash ((n complex) &key (seed *default-seed*) mix-only)
  (let ((real (realpart n))
        (imag (imagpart n))
        (hash seed))
    (mixf hash real)
    (mixf hash imag)
    (if mix-only
        hash
        (finalize hash
                  (cl:+ (integer-length real)
                        (integer-length imag))))))

(defmethod murmurhash ((bv bit-vector) &key (seed *default-seed*) mix-only)
  (let ((int 0))
    ;; Presume little-endian.
    (dotimes (i (array-total-size bv))
      (setf (ldb (byte 1 i) int) (row-major-aref bv i)))
    (hash-integer int seed mix-only)))

(defmethod murmurhash ((cons cons) &key (seed *default-seed*) mix-only)
  (let ((hash seed)
        (len 0))
    (loop (cond ((null cons)
                 (return))
                ;; Improper list.
                ((atom cons)
                 (incf len)
                 (mixf hash (murmurhash cons :seed hash :mix-only t))
                 (return))
                (t (incf len)
                   (mixf hash (murmurhash (car cons) :seed hash :mix-only t))
                   (setf cons (cdr cons)))))
    (if mix-only
        hash
        (finalize hash len))))

(defmethod murmurhash ((array array) &key (seed *default-seed*) mix-only)
  (let ((hash seed))
    (mixf hash (array-rank array))
    (loop for dimension in (array-dimensions array)
          do (mixf hash dimension))
    (mixf hash (murmurhash (array-element-type array)
                           :seed seed :mix-only t))
    (loop for elt across array
          do (mixf hash (murmurhash elt :seed hash :mix-only t)))
    (if mix-only
        hash
        (finalize hash (array-total-size array)))))

(defmethod murmurhash ((ht hash-table) &key (seed *default-seed*) mix-only)
  (let ((hash seed))
    (mixf hash (string (hash-table-test ht)))
    (maphash
     (lambda (k v)
       (mixf hash (murmurhash k :seed hash :mix-only t))
       (mixf hash (murmurhash v :seed hash :mix-only t)))
     ht)
    (if mix-only
        hash
        (finalize hash (hash-table-count ht)))))


;;;; Derived utilities.

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



;;; http://code.google.com/p/smhasher/source/browse/trunk/KeysetTest.cpp

(defconstant +32-bit-verification+ #xb0f57ee3) ;2968878819

(defconstant +128-bit-verification+ #xb3ece62a) ;3018647082

(defun verification-value (hashbits)
  (let ((hash (ecase hashbits
                (32 #'hash32-octets)
                (128 #'hash128-octets))))
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

(defun verification-test (hashbits expected)
  (let ((value (verification-value hashbits)))
    (values
     (= value expected)
     value expected)))

(defun verify ()
  (and (verification-test 32 +32-bit-verification+)
       (verification-test 128 +128-bit-verification+)))
