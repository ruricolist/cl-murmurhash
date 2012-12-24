;;;; cl-murmurhash.asd

(asdf:defsystem #:cl-murmurhash
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "32-bit version of MurmurHash3."
  :license "MIT"
  :serial t
  :depends-on (#:babel #:fast-io)
  :components ((:file "package")
               (:file "cl-murmurhash")))
