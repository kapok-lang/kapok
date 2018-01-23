Function
==========

Kapok is a functional programming language. As in any other functional programming language, function plays a important role in code. We have discussed how to define anonymous functions in expressions using the `fn` form. Here we will talk about how to define named function in the top level of a namespace. 

#### Public, Private

There are two types of functions/macros accessibility, public and private. A private function/macro is accessable only inside the same namespace, while a publice function/macro could also be called in any other module. A public function is equal to an exported function in Erlang, while a private function is not exported.

#### Define a function

The special form `defn` is used to define public functions, and `defn-` is to define private functions. Both share the same syntax:

```clojure
;; define a public function with a single clause
(defn function [arguments] guard
  body)

;; define a public function with multiple clauses
(defn function
  ([arguments1] guard1
   body1)
  ([arguments2] guard2
   body2)
  ...)
```

"function" should be an identifier. "arguments" is a literal list which is parenthesized in square brackets. "guard" is optional and "body" could contains multiple expressions. The value evaluated by last expression of "body" is the value of whole function clause. For example:

```clojure
(defn sum [l] (&when (list? l))
  (seq.reduce l (fn [x acc] (+ x acc))))
```

This example defines a function `sum` to sum a list of numbers. It accepts a single argument `l`. In the function clause guard, it checks whether the argument `l` is of type list. If `l` is a list, it uses the protocol function `seq.reduce` to traverse the list and calculate the sum. Otherwise an exception is raised due to no function clause matches the given argument.

If we want `sum` to accept a tuple of numbers as the argument, we could write a new clause for the argument being a tuple:

```clojure
(defn sum [l] (&when (list? l))
  (seq.reduce l (fn [x acc] (+ x acc))))

(defn sum [t] (&when (tuple? t))
  (sum (erlang.tuple_to_list t)))
```

Just like what's in Erlang, it's ok for the function `sum` to have two clauses, where each clause handles a different type of argument. In the clause body for the tuple argument, first the tuple argument is converted to a list, then `sum` is called using the list to run code of the previous clause. It works because the Erlang VM supports tail recursion. The definition code could also be merged into a single `defn` form:

```clojure
(defn sum
  ([l] (&when (list? l))
   (seq.reduce l (fn [x acc] (+ x acc))))

  ([t] (&when (tuple? t))
   (sum (elang.tuple_to_list t))))
```

In the above examples, if the keyword `defn` is replaced by `defn-`, then the code still works except that we get a private function instead of a public function.

#### Function arguments

Kapok supports four types of arguments: normal, optional, rest, keyword, just like what's in Common Lisp. A normal argument is the arguments which are listed one by one in the argument list of function definition, each of them represents a standalone actual argument. For example:

```clojure
(defn add [x y]
  (+ x y))
;; (add 1 2) ;=> 3
```

Function `add` takes two arguments `x`, `y` and return their sum as result. And then we call `add` with two actual arguments `1`, `2`, so that in the definition, argument `x` get the value `1` and argument `y` get the value `2`. The format of calling `add` is similar to its definition.

An optional argument is the argument after keyword `&optional` in a function definition, which could be omitted when calling the function. For example:

```clojure
(defn f [&optional (a 1) (b 0)]
  (+ a b))
;; (f )     ;=> 1
;; (f 2)    ;=> 2
;; (f 2 3)  ;=> 5
```

Here we define a function `f` with two optional argument `a` and `b`, `a` has the default value 1 and `b` has the default value 0. If no actual argument is provided for an argument when the function is called, it would take the default value as its value. In this example, if we call `f` without any argument, local `a` would be 1 and local `b` would be 2. The optional argument could only be omitted from right to left, which means if an argument, e.g. `a` needs to be omitted, then all arguments right to `a`, in this case only `b`, need to be omitted. So if we call `f` with only one actual argument, the argument would to be bound to `a`.

A rest argument is the last argument after keyword `&rest` in a function definition, which bundles all the tailing actual arguments. For example:

```clojure
(defn add [&rest l]
  (seq.reduce l 0 (fn [x acc] (+ a acc))))
;; (add )          ;=> 0
;; (add 1)         ;=> 1
;; (add 1 2)       ;=> 3
;; (add 1 2 3 4 5) ;=> 15
```

In this example, all the actual arguments are packed into `l`, just like we write:

```clojure
(defn add [l]
  ;; body
  ...)
;; (add [1 2 3 4 5]) ;=> 15
```

A keyword argument is the last argument in a function definition, which bundles all the tailing actual arguments in key-value pairs. For example:

```clojure
(defn sum [&key a b]
  (+ a b))
;; (sum :b 2 :a 1)    ;=> 3
```

In this example, all the actual arguments are packed into key-value pairs, and then inside the function body, the corresponding values of keys are mapped to locals `a`, `b`. You may notice that when `sum` is called, the order of key `a`, `b` is not the same as they're in the definition. It's equivalent to:

```clojure
(defn sum [map]
  (+ (maps.get #a map)
     (maps.get #b map)))
;; (sum #{:b 2
          :a 1})    ;=> 3
```

As these four types of arguments could be combined in a function, there are serverl combinations of function arguments:

1. normal + optional
1. normal + rest
1. normal + key
1. optional normal + optional + rest

Only these combinations are valid, others would cause compile errors. The first three combination is plain simple. Let's take a look at an example for the last one:

```clojure
;; define a function with normal + optional + rest arguments
(defn f1 [a &optional b &rest c]
  ...)

(f1 1)       ;; a == 1, b == :nil, c == []
(f1 1 2)     ;; a == 1, b == 2, c == []
(f1 1 2 3)   ;; a == 1, b == 2, c == [3]
(f1 1 2 3 4) ;; a == 1, b == 2, c == [3 4]
```

The argument binding from left to right. If the previous one is bound, it will try to bind the tailing to left arguments.

#### Macro

Macros are a special functions that when they are called, they are expanded in compile time insteed of runtime. Macros use keywords `defmacro`, `defmacro-` rather than `defn`, `defn-`. Similarly, `defmacro` is for public macros, while `defmacro-` is for private macros. 

There are three macro special forms to make our life easier when defining macros: `\`` (backquote), `~` (unquote), `~@` (unquote splicing). They works like what's in Clojure or any other Lisp dialet.

Take the `if` macro in standard library for example:

```clojure
(defmacro if [test then &optional else]
  `(case (kapok.core.#true? ~test)
     (:true ~then)
     (:false ~else)))
```

When macro `if` is called in the code, the calling code ast is passed as the arguments "test", "then", "else". And then this macro expands its result and insert it right the place. Assume we call this macro in a context:

```clojure
(defn call-if []
  (if (check-stat)
      (do-something)
    (do-something-else)))
```

When the `if` macro is expanded, this definition of `call-if` would become:

```clojure
(defn call-if []
  (case (kapok.core.#true (check-stat))
    (:true (do-something))
    (:false (do-something-else))))
```

Like any other Lisp dialet, the macro expansion is done at the early phase of the compilation before the code is compiled to Erlang VM binary code.
