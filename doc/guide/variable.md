Variable and Pattern Matching
==========

TODO add more content

### Binding and Pattern matching

Like Clojure, the special form to define local bindings in Kapok is `let`.

TODO add examples for local bindings

And `let` supports destructing like Clojure. In Erlang, there is a similar concept of destructing, called pattern matching. There are two kinds of destructing

1. Sequential destructing

  Sequential destructing works with the below types: lists, tuples, bitstring, binary, list string, binary string.

2. Map destructing

  Map destructing is conceptully identical to sequential destructing, except that it works only for maps.

TODO add more details for destructing.

Please notice that set is not supported in destructing.

