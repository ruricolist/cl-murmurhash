Common Lisp implementation of the 32-bit variant of MurmurHash3
<http://code.google.com/p/smhasher/>, "a fast non-crytographic hashing
algorithm."

Use MURMURHASH to get the hash of an object:

     (murmurhash:murmurhash "Hash me")
     => 841354010

(You may want to use MURMURHASH:VERIFY to determine that the algorithm
is implemented correctly for your platform.)

If you want to use a different seed, you can bind the parameter
*DEFAULT-SEED*, or pass a keyword argument:

     (murmurhash:murmurhash "Hash me" :seed *new-seed*)

The size of the hash is controlled by *HASH-SIZE*, which can be bound
to 32 (the default) or 128.

There are two utilities for perfect hashing. MAKE-PERFECT-SEED takes a
list of objects and tries to find a seed that hashes them all without
collisions. MAKE-PERFECT-HASH-FUNCTION does the same, but returns a
partial application of MURMURHASH instead of the seed itself.

Note: finding a perfect seed is usually fast but nothing guarantees
success, or success in a reasonable amount of time.
