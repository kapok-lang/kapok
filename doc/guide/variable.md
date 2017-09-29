Variable and Pattern Matching
==========

Immutable data plays a important role in functional programming language. Just like what's in Erlang, variables in Kapok are immutable. It means that once a variable is bound, it's illegal to be rebound. 

### Identifier

Identifiers are the names which refer to namespaces, functions, variables, etc. They're called symbols in Lisp. There are two kinds of identifiers in Kapok: identifier, and dot identifier.

Identifier must begin with a non-numeric character, and in addition to any alphanumeric characters, it could contain these characters:

```text
! $ % * + - / < = > ? @ _ | ~ & # ^
```

like symbols in other Lisp dialects, the valid characters for identifiers are far more than non-Lisp language. For example, valid characters for identifiers in Python could only contain alphanumeric characters and underscore. Notice that the last a few characters are preserved for some other keywords or literal types, as listed below:

```clojure
~ ~@                    ;; is macro unquote, unquote splicing keyword
&optional &rest &key    ;; function argument specification
#""                     ;; list string
^                       ;; atom
```

It would not work using `~ & # ^` as the start of an identifier. But you could use them in any position after the first char in an identifier, as long as it would not cause confusion with the preserved literal types.

Also notice that identifiers in other Erlang VM based programming languages have fewer valid characters. For example, identifiers in Elixir contain only alphanumeric characters and underscore. If you need to write a Kapok module for Elixir code to call, please make sure the identifier name to be compactible.

If an identifier that start with underscore(_) and with tailing other characters, the Kapok compiler would not report warning if the identifier is not used. If an identifier is a single underscore, it acts like a placeholder. For example, if we didn’t need to capture a value during the pattern matching, we could specify the special variable _ (an underscore). This acts like a variable but immediately discards any value given to it—in a pattern match, it is like a wildcard saying, “I’ll accept any value here.”

The dot character(.) is not a valid character of an identifier. If the dot character occurs between two identifiers, it represents a namespaced-identifier, which is called dot-identifier in whole. A dot-identifier evaluates to the named value in the specified namespace. For example, we could specify a embedded namespace in the ns special form, as

```clojure
(ns some-namespace
  ;; ...
  )

(defn f []
  ;; ...
  )
```

Or call a function of this namespace like this

```clojure
(some-namespace.f)
```

Multiple dot characters could occur in an dot-identifier, which is a way to specify multiple levels of namespaces and their hierarchy. For example

```clojure
(ns some-namespace.some-inner-namspace.some-innermost-namespace
  ;; ...
  )

(defn f []
  ;; ...
  )
```

the above codes are valid, the `f` function could be called in this way:

```
(some-namespace.some-inner-namespace.some-innermost-namespace.f)
```

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

