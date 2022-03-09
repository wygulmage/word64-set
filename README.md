# word64-set
## sets of 64 bit non-negative integers (`Word64`) or integers (`Int64`), inspired by [containers](https://github.com/haskell/containers) and [constrained-monads](https://github.com/oisdk/constrained-monads)

Like Data.IntSet, these are big-endian prefix trees. `int64-set` provides sets of `Int64`s in `Data.Set.Int64`, and `word64-set` provides sets of `Word64`s in `Data.Set.Word64` along with an internal module `Data.Set.Word64.Internal`. The internal API is subject to breaking changes at any time for any reason, while the external modules' APIs will follow Haskell's Package Versioning Policy for release numbers greater than 0.0.0.0.
