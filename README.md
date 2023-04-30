# Record-storable

[![CI](https://github.com/vagarenko/record-storable/actions/workflows/main.yml/badge.svg)](https://github.com/vagarenko/record-storable/actions)

Anonymous storable-based records.

This library is similar to `superrecord` library (http://hackage.haskell.org/package/superrecord) but uses foreign memory to store fields instead of boxed arrays. Also, it provides both mutable and immutable APIs.

Unfortunately, both libraries suffer from the same problem: compilation time seems to grow exponentially in the number of fields of a record. (See also https://github.com/agrafix/superrecord/issues/12)

This library is not being actively developed.
