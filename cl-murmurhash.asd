;;;; cl-murmurhash.asd

(asdf:defsystem #:cl-murmurhash
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :description "32-bit version of MurmurHash3."
  :license "MIT"
  :serial t
  :depends-on (#:flexi-streams #:babel)
  :components ((:file "package")
               (:file "cl-murmurhash")))
