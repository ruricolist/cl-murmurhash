Common Lisp implementation of the 32-bit variant of [MurmurHash3](https://github.com/aappleby/smhasher), "a fast non-crytographic hashing algorithm."

Use `murmurhash` to get the hash of an object:

     (murmurhash:murmurhash "Hash me")
     => 841354010

You may want to test the system to determine that the algorithm is implemented correctly for your platform:

    (asdf:test-system "cl-murmurhash")

If you want to use a different seed, you can bind the special variable `*default-seed*`, or pass a keyword argument:

     (murmurhash:murmurhash "Hash me" :seed *new-seed*)

The size of the hash is controlled by `*hash-size*`, which can be bound to 32 (the default) or 128.

There are two utilities for perfect hashing. `make-perfect-seed` takes a list of objects and tries to find a seed that hashes them all without collisions. `make-perfect-hash-function` does the same, but returns a partial application of `murmurhash` instead of the seed itself.

Note: finding a perfect seed is usually fast but nothing guarantees success, or success in a reasonable amount of time.
