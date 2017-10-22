Variable and Pattern Matching
==========

Immutable data plays a important role in functional programming language. Just like what's in Erlang, variables in Kapok are immutable. It means that once a variable is bound, it's illegal to be rebound. The idea behind keeping this variable immutability in Kapok is that it helps us to program better, and it's easy for Kapok to utilize the immutability of Erlang and map the Kapok code to Erlang code.

Just like what's in Lisp, there is a keyword `let` used to bind variables. But there is no `setq` or any similar operation to perform imperative style assignment/re-assignment. And it supports pattern matching. Yeah, this good feature from Erlang is kept. In summury, it looks closer to Clojure other than any other older Lisp dialet, when talking about variable binding and pattern matching in Kapok.

To declare, bind and use a variable, first we need to took a look at identifiers.

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

The dot character(.) is not a valid character of an identifier. If the dot character occurs between two identifiers, it represents a namespaced-identifier, which is called dot-identifier in whole. A dot-identifier evaluates to the named value in the specified namespace. For example, we could specify a simple namespace in the ns special form, as

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

Similarly, we could specify a embedded namespace in the ns form using dot-identifier. Multiple dot characters could occur in an dot-identifier, which is a way to specify multiple levels of namespaces and their hierarchy. For example

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

The dot-identifier could be used in namespace name, struct name and protocol name only. It's not allowed to be used as a variable or function because there is no hierarchy needed for them.

### Binding and Pattern Matching

The `let` form is a special form to define local bindings in Lisp. In kapok, it also supports pattern matching like Clojure.

A local binding is a named reference which is lexically scoped to the extent of the let expression. It's also called local variable or locals. For example, this is a function in Python:

```python
def f(x, y):
    l = x * x + 1;
    m = y * 2 + x;
    return l * m + m
```

is equivalent to this Kapok function:

```clojure
(defn f [x y]
  (let [l (+ (* x x) 1)
        m (+ (* y 2) x)]
    (+ (* l m) m)))
```

the `l` and `m` locals in the respective function bodies both refer to an intermediate value. They're declared in the list as the first argument of the let form, which is called binding list. Inside the binding list, every two expressions match in a pair to declare a local: a pattern and a value. We will talk about them later in pattern matching. Following the binding list, it's is the body of let form, which could contain one or more than one expression. Locals are usual in common programming language. In Kapok, all locals are immutable. you can override a local binding within a nested `let` form, but there is no way to change its value within the scope of a single let form. 

Occasionally, you will want to evaluate an expression in the binding list without a local refered to its result. In these cases, an underscore could be used as the local name, or as the prefix of local name, so that the compiler know that this value is going to be unused intentionally. For example,

```clojure
(let [x (get-value)
      _ (print "value: ~p" x)]
  (+ 1 x)
  )
```

The `print` expression will be evaluated and the result will be marked as unused. Otherwise if a local without underscore prefix is unused within tho context of a `let`'s body, the compiler will trigger a warning for this unused variable.

In the binding list, it supports destructuring like Clojure. In Erlang, a similar concept of destructuring is called pattern matching. Destructuring is somewhat different from pattern matching. In Kapok, the semantics follows closer to Erlang, so we stick to the name pattern matching. The pattern matching works for two kinds of structure:

#### 1. sequential collection

  Sequential collections include the types of lists, tuples, bitstring(and binary), list strings, binary strings.
  
  For every element in the collection, a local must be declared in the pattern part. For example, we could write this code to do pattern matching for a list:

```
(let [[x _y z] [42 "foo" 99.2]]
  (+ x z))
```

  No ommit is allowed in pattern, which is different from Clojure. For example, it's legal to do destructuring like

```clojure
(def v [42 "foo" 99.2 [5 12]])
;= #'user/v 
(let [[x y z] v]
  (+ x z))
;= 141.2
```

But it's illegal to omit `[5 12]` in `v` in the pattern part in Kapok.

It shares the same syntax to pattern match a sequential collection, and declare a sequential collection. The syntax is consistent in this way. For example, the following code show how to pattern match each type of sequential collections:

```
(let [;; list
      [a _ _] [1 2 3]
      ;; tuple
      {b _} {4 5}
      ;; bitstring
      <<(c (:size 5)) (_ (:size 15))>> <<(6 (:size 5)) (7 (:size 3)) (8 (:size 12))>>
      ;; binary
      <<(d), (_), (_)>> <<(9), (10), (11)>>
      ;; list string
      [e & _] #"hello"
      ;; binary string
      <<(f), (_) (_) (_) (_)>> "hello"
      ]
  ;; body
  ...
)
```

#### 2. map collection

  We could do pattern matching for maps. Conceptually it's identical to pattern matching for sequential collection. The difference is that we only have to write the key value pair in the pattern part for what we want to match. For example

```
(let  [;; map
       #{^k1 value} #{^k1 100 
                      ^k2 200}]
  ;; body
  (+ 1 value)
)
```

We omit `^k2` in the pattern and fetch whatever value of `^k1`, refer it as the local `value` and access it in the body of the let form.

Please notice that set collection is not supported in pattern matching.

