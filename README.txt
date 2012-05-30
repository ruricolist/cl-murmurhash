Common Lisp implementation of the 32-bit variant of MurmurHash3
<http://code.google.com/p/smhasher/>, "a fast non-crytographic hashing
algorithm."

Use MURMURHASH to get the hash of an object:

     (murmurhash:murmurhash "Hash me")
     => 696528482

If you want to use a different seed, you can bind the parameter
*DEFAULT-SEED*, or pass a keyword argument:

     (murmurhash:murmurhash "Hash me" :seed *new-seed*)

Strings are converted to octets using FLEXI-STREAMS:STRING-TO-OCTETS
and the external format specified in *EXTERNAL-FORMAT*. The default
encoding is UTF-8.

There are two utilities for perfect hashing. MAKE-PERFECT-SEED takes a
list of objects and tries to find a seed that hashes them all without
collisions. MAKE-PERFECT-HASH-FUNCTION does the same, but returns a
partial application of MURMURHASH instead of the seed itself.

Note: finding a perfect seed is usually fast but nothing guarantees
success, or success in a reasonable amount of time.
