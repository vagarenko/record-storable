# Record-storable

[![CI](https://github.com/vagarenko/record-storable/actions/workflows/main.yml/badge.svg)](https://github.com/vagarenko/record-storable/actions)

Anonymous storable-based records.

This library is similar to `superrecord` library (http://hackage.haskell.org/package/superrecord) but uses foreign memory to store fields instead of boxed arrays. Also, it provides both mutable and immutable APIs.

Unfortunately, both libraries suffer from the same problem: compilation time seems to grow exponentially in the number of fields of a record. (See also https://github.com/agrafix/superrecord/issues/12)

## Examples

### Create record from HList of fields

```haskell
r :: Rec '[ "a" := Char, "b" := Bool, "c" := Int8, "d" := Double, "e" := Float]
r = record (#a := 'a'
        :&  #b := False
        :&  #c := 0
        :&  #d := 1
        :&  #e := 2
        :& Nil)
```
### Getting & setting fields

```haskell
> getField #a r
'a'
> setField #a r 'b'
Rec [ #a := 'b' , #b := False , #c := 0 , #d := 1.0 , #e := 2.0 ]
> modifyField #b r not
Rec [ #a := 'a' , #b := True , #c := 0 , #d := 1.0 , #e := 2.0 ]
```
