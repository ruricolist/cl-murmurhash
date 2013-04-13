;;;; package.lisp

(defpackage #:cl-murmurhash
  (:use #:cl)
  (:nicknames :murmurhash)
  (:export
   :*default-seed* :*hash-size*
   :murmurhash
   :make-perfect-seed :make-perfect-hash-function
   :unhashable-object-error
   :verify)
  (:shadow :+ :* :break))
