;;;; cl-murmurhash.asd

(defsystem "cl-murmurhash"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "32-bit version of MurmurHash3."
  :license "MIT"
  :serial t
  :depends-on ("babel"
               #+sbcl "sb-rotate-byte")
  :components ((:file "package")
               (:file "cl-murmurhash"))
  :in-order-to ((test-op (test-op "cl-murmurhash/test"))))

(defsystem "cl-murmurhash/test"
  :depends-on ("cl-murmurhash" "fiveam")
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :cl-murmurhash/test :run-tests)))
