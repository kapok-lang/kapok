Expressions
==========

Kapok is a functional programming language. Codes in Kapok are constructed in functions, and then functions are packed into namespaces(or called modules sometimes). And namespaces are organized into files. This way of code organization is just like Erlang.

Functions are constructed by expressions, each expression returns a value, as each function returns a value. In Lisp, an expression is usually called s-expression or s-expr(for "symbolic expression"). A s-expression could be either an "atom" or a "list". 

An "atom" is an atomic symbol that eval to a value. Please notice that, it's not equal to the atom data type in the previous chapter. 

A list is a parenthesized sequenece of s-expressions. Inside the parentheses, the first element is taken as a function or a macro, the left are taken as its arguments. It has the form `(function argument1 argument2 ...)`. Some forms are called special forms because they are the basic primitives to the language and implemented as the foundation.

Let's take a look at those special forms of expressions.

### Special Forms

#### let

We have seen `let` form when we discuss pattern matching previously. A `let` form has two parts: local bindings and body. The local binding part is a literal list of "pattern, value" pair, where "pattern" and "value" are matched by position, and local names are usually bound in "pattern", "value" could be any expression. The body part consists of arbitrary number of expressions. For example:

```
(let ;; the binding part
     [str "a binary string"
      {family _} (#os.#family)
      #{#a arg} (maps.from_list [{#b 2} {#a 1}])]
  ;; the body part
  (io.format "os family: ~p" [family])
  (inc value)
 )
```

In this `let` form, a local `str` is bound to a literal string `"a binary string"`, a local `family` is bound to the first element of the tuple returned by calling `(#os.#family)` function, and a local `arg` is bound to the value of the map constructed by the given associative list. Notice how pattern matching works for the later two locals `family` and `arg`. They are both bound to a part of a bigger data structure.

In the following body part, there are two expressions, the first one just output some message to stdout, and its return value is ignored. The second add 1 to `arg` and get a value `2` . Since it's the last expression of the whole `let` form, its value `2` will become the value of the whole `let` form.

Aliases are also supported to retain a destructed collection in pattern matching of the `let` form. For example:

```

(defn f []
  {2 3.14 $x})

(let [{a _ _} &as v (f)]
  {(inc a) v})
```

In the second pair of pattern value, we bind the first element of the tuple returned from function `f` to local `a`, meanwhile we bind the whole tuple to an alias local `v`, since we need to use `v` as a part of the return value of this `let` form. Without this alias feature, doing so would requires something like this:

```
(let [v (f)
      {a _ _} v]
  {(inc a) v})
```

It's verbose and sometimes inconvenient if we need to do pattern matching for function argument and use the bound locals in function guards.

#### case

#### try

#### fn

#### do

#### send, receive

#### Operators

op-not
op-or, op-and, op-xor
op-andalse, op-orelse

### attribute, behaviour
