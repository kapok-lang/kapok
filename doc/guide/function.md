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

```
(defn sum [l] (&when (list? l))
  (seq.reduce l (fn [x acc] (+ x acc))))

(defn sum [t] (&when (tuple? t))
  (sum (erlang.tuple_to_list t)))
```

Just like what's in Erlang, it's ok for the function `sum` to have two clauses, where each clause handles a different type of argument. In the clause body for the tuple argument, first the tuple argument is converted to a list, then `sum` is called using the list to run code of the previous clause. It works because the Erlang VM supports tail recursion. The definition code could also be merged into a single `defn` form:

```
(defn sum
  ([l] (&when (list? l))
   (seq.reduce l (fn [x acc] (+ x acc))))

  ([t] (&when (tuple? t))
   (sum (elang.tuple_to_list t))))
```

In the above examples, if the keyword `defn` is replaced by `defn-`, then the code still works except that we get a private function instead of a public function.

#### Function arguments

#### Guards

#### Macro
