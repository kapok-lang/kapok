Expression
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

Aliases are also supported to retain destructed collections in pattern matching of the `let` form. For example:

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

The `case` form is the only special form built in the compiler for conditional expression. It has the syntax like:

```clojure
(case condition
  (pattern1 action-sequence-1)
  (pattern2 action-sequence-2)
  ...)
```

`case` takes an expression `condition` as its first argument, and then the sequence of the "pattern, action-sequence" clauses parenthesized in a general list. These "pattern, action-sequence" clauses are matched from top down one by one. If the value returned by `condition` matches any pattern, then the corresponding action expression sequence is evaluated, the result of evaluating the expression sequence is the value of the whole `case` form. The "action-sequence" is wrapped in a `do` form(see the [do](#do) section for more info) and could contain arbitrary number of expressions. Once the condition value is matched, the following clauses would not be checked or run any more. If no pattern matches, then an exception is raised. So it's usual to write code like:

```
(case arbitrary-condition
  (pattern1 action-sequence-1)
  ...
  (_ (fallback-action-sequence)))
```

The last clause uses an underscore `_` to match any value for `arbitrary-condition` to avoid raising exception. In this example, if no pattern matches, the `fallback-action-sequence` will be evaluated as a fail-safe clause.

#### if, when, cond, other conditional expressions

There are other special forms for conditional expressions: if, if-not, when, when-not, unless, cond. They are macros defined in standard library `kapok.core` and imported by default. Since they are defined in standard library rather than the compiler, strictly speaking they are not as special as `case` because you could perform a hack to override them by introducing a new definition to the same name. This hack would not happen in normal usage. So we could still take these forms as special forms usually. Let's take a glance over them one by one.

`if` takes a condition "test", a "then" expression and an optional "else" expression as its arguments, as the following syntax describes:

```clojure
(if test
    then)

(if test
    then
  else)
```

If "test" evaluated to `:true`, "then" will be evaluated. Otherwise "else" will be evaluated. If "else" is missing and "test" evaluated to `:false`, then `:nil` will be returned for whole `if` form. Either "test" or "then" branch could contain a single expression. If you need pack multiple expressions into the branch, use `do` form(refer to [do](#do) section if necessary).

`if-not` is the reversed version of `if`. It has the similar syntax of `if`:

```clojure
(if-not test
        then)

(if-not test
        then
  else)
```

"then" is evaluated when "test" evaluated to `:false`, otherwise "else" is evaluated.

`when` takes a condition "test" and an expression sequence "body" as its arguments. It has the following syntax:

```clojure
(when test
  ;; body
  expression-1
  expression-2
  ...)
```

The value evaluated by the last expression of "body" would become the value of the whole `when` form. If "test" is evaluated to `:false`, the `when` form returns `#ok`. Since "body" is a expression sequence, it could contain arbitrary number of expressions, no need to wrap them in a `do` block.

`when-not` is the reversed version of `when`. It has the similar syntax of `when`:

```clojure
(when-not test
  ;; body
  expression-1
  expression-2
  ...)
```

Only when "test" evaluated to `:false`, the "body" is evaluated. 

`unless` is an alias to `when-not`. It has the same arguments and semantic like `when-not`, except than the macro name is `unless` but not `when-not`.

`cond` takes a sequence of "condition, action" pairs as it arguments. Either "condition" or "action" is a single expression. They are matched by position like what's in the binding part of `let` form. `cond` has the following syntax:

```clojure
(cond
  condition-1 expression-1
  condition-2 expression-2
  ...
  )
```

`cond` is evaluated as follows: First, "condition-1" is evaluated. If it's evaluated to `:true`, "expression-1" will be evaluated and its value will be the value of the whole `cond` form. If "condition-1" is evaluated to `:false`, "expression-1" will not be evaluated and it will go to the following condition expressions, which is "condition-2" in turn, and do the evaluation likewise. If no condition is evaluated to `:true`, no exception would be raised and the whole `cond` just return value `:nil`. Note the difference of `cond` and `case`: 

1. in the syntax of `case`, "condition", "expression-sequence" pairs require to parenthesized; while they don't in `cond`
1. it supports expression sequence in `case` while just expression in `cond`
1. in `case` form, an exception will be raised if no match; while it will not in `cond`
1. `case` is used in case that we need to switch to different branch of code according to the value of the same condition expression; while `cond` is used when we need to switch to different branch according to different condition expression

One last thing about these condition forms: that the value of "test" expression, or "condition" expression mentioned above, is checked by the `true?` or `:false?` function in `kapok.core`. They treat only literal `:false` to be logical false, and only literal `:true` to be logical true. Don't use any other value rather than literal `:false`, `:true` in this case. You could refer to the [boolean data type](./data-type.md#boolean) for more info.

#### try-catch

`try-catch` have the following syntax:

```clojure
(try expression
    ;; try body
    ((pattern1 expression-sequence-1)
     (pattern2 expression-sequence-2)
     ...)
  (catch
    (exception-pattern-1 expression-sequence-3)
    (exception-pattern-1 expression-sequence-4)
    ...)
  (after
    expression-sequence-5)
  )
```

The first part of this form works like `case` form. Basically it's a case expression `catch` and `after` blocks at the end. It works as follows: First "expression" is evaluated. If this finishes without raising an exception, then the return value of "expression" is pattern matched against the patterns in the try body, "pattern1", "pattern2", and so on, until a match is found. If a match is found, then the following expression sequence is evaluated and its value will become the value of the whole `try-catch` form.

If an exception is raised within "expression", then the catch patterns "exception-pattern-1", and so on, are matched to find which expression sequence should be evaluated correspondingly. The expression pattern has the format of `(excetpion-type error)`. "exception-type" is an atom (one of `#throw`, `#exit`, `#error`) that tells us the how the exception was generated. If "exception-type" is omitted, then the value defaults to `#throw`. "error" could be any term related to this exception.

`try-catch` form has an optional `after` clause. Usually it's used for cleaning up. The code inside the `after` clause is guaranteed to be executed, even if an exception is raised. The code inside the `after` clause is run immediately after any code in "expression", body(if no exception is raised), or `catch` clause(if an exception is raised). The return value of the code in `after` clause is lost.

The ones who is familiar with Erlang will notice that it's the same as its counterpart in Erlang.

#### fn

In kapok, function is a data type, it could be used as arguments to functions and the functions can return functions. We could use `fn` form to define an anonymous function or refer to an existing function. It has the syntax:

```clojure
;; create reference to a local function
(fn function arity)

;; create reference to a remote function
(fn module function arity)

;; define an anonymous function with single clause
(fn [arguments] guard
  function-body)

;; define an anonymous function with multiple clauses
(fn
  ([arguments1] guard1
   body1)
  ([arguments2] guard2
   body2)
  ...)

;; define a named function with single clause
(fn function [arguments] guard
  body)

;; define a named function with multiple clauses
(fn function
  ([arguments1] guard1
   body1)
  ([arguments2] guard2
   body2)
  ...)
```

The "function" argument could be any valid identifier, "module" could be identifier or dot-identifier, "arity" is the integer which represent the function arity. The "arguments", "guard", "body", "clause" follow the same rules of a normal function defined in top level of a namespace. Refer to [function](./function.md) chapter for more info.

All these `fn` forms are similiar to their counterparts in Erlang.

#### <a id="do">do</a>

The `do` special form evaluates expressions in order and returns the value of the last. It construct a code block of multiple expressions, and is usually used in place where only one expression is allowed. For example:

```clojure
(if (state-is-valid?)
    ;; code block of doing something when state is valid
    (do
      (do-something-1)
      (do-something-2))
  (if (is-invalid-state-1)
      ;; code block of handling invalid state
      (do
        (log-message)
        (handle-invalid-state))
    (trigger-error)))
```

In this `if` expression, only one expression is allowed in each branch. However we need to call multiple functions sometimes, so the `do` forms are used to construct two code blocks when needed. In general, this kind of `if` expressions could be revised to a equivalent `case` form. But sometimes it's clearer to be `if` expression and `do` forms do help.

#### send, receive

`send` takes a "pid" and a "message" as its arguments. "pid" is process id, "message" could be any term. It has the syntax:

```clojure
(send pid message)
```

Like its counterpart in Erlang, `send` sends "message" to the processed with identifier "pid". Message sending is asynchronous. The sender does not wait but continues with what it was doing. The value of `send` form is defined to be "message". So that you could chain the calls like:

```(clojure)
(send pid1 (send pid2 message))
```

This example means: send the "message" to all the process "pid1", "pid2".

`receive` receives a message that has been sent to a process. It has the following syntax:

```clojure
(receive
  (pattern1 guard1
   expression-sequence-1)
  (pattern2 guard2
   expression-sequence-2)
  ...
  (after timeout
    expression-sequence-n)
  )
```

All the guards along the patterns are optional. When a message arrives at the process, the system tries to match it against "pattern1"(with optional guard); if this succeeds, it evaluates the corresponding expression sequence. If the first pattern does not match, it tries the following "pattern2", and so on. If no pattern matches, the message is saved for later processing, and the process waits for the next message.

The "after" clause is optional. This clause has a "timeout" expression and a exprssion sequence to be evaluated when the timeout value is reached without receive any matched message. It's useful when we need to set a timeout when waiting to receive a message.

The ones who is familiar with Erlang would notice that the `receive` form is the same as Erlang in semantics.

#### Logical Operators

In Erlang, there are logical operators: `not`, `or`, `and`, `xor`, `andalso`, `orelse`. In Kapok, we have functions and macros for logical "not", "and", "or" in the standard library `kapok.core`. But these logical operators are still useful, when we need to use them on guards or when we need effective code. Because in the pattern matching guards or function argument guards, only operators are allowed, but not standard library functions or macros. We would discuss guard later when we get to function section. Let's focus on these operators here.

`op-not` takes a single expression as its argument. It checks whether the given expression is evaluated to literal `:false`. It has the syntax:

```
(op-not expression)
```

`op-or`, `op-and`, `op-xor` takes one or more than one expression as its arguments. `op-or` checks whether one of the expression is evaluated to literal `:true`. `op-and` checks whether all the expressions are evaluated to literal `:true`. `op-xor` checks whether two expressions have different value from literal `:true`, `:false`. They have the samilar syntax as:

```clojure
(op-or/op-and/op-xor expression-1)
                     expression-2
                     ...)
```

Please note that when argument number is 1 or more than 2, they have the equivalent relationships as:

```clojure
(op-or/op-and/op-xor expression-1)
;; equal to
expression-1

(op-or/op-and/op-xor expression-1
                     expression-2
                     expression-3)
;; equal to
(op-or/op-and/op-xor (op-or/op-and/op-xor expression-1
                                          expression-2)
                     expression-3)
```

All `op-or`, `op-and`, `op-xor` will evaluated all their arguments. However there are short-circuit version of them, `op-orelse` and `op-andalso`, which evaluate their arguments only when necessary. They have the syntax:

```clojure
(op-orelse expression-1
           expression-2
           ...)

(op-andalso expression-1
            expression-2
            ...)
```

`op-orelse` first evaluates "expression-1". If "expression-1" evaluates to literal `:true`, "expression-2" is not evaluated. If "expression-1" evaluates to literal `:false`, "expression-2" is evaluated. And so on.

`op-andalso` first evaluates "expression-1". If "expression-1" evaluates to literal `:true`, "expression-2" is evaluated. If "expression-1" evaluates to literal `:false`, "expression-2" is not evaluated.

When the argument number is 1 or more than 2, they have the equivalent relationships as:

```clojure
(op-orelse/op-andalso expression-1)
;; equal to
expression-1

(op-orelse/op-andalso expression-1
                      expression-2
                      expression-3)
;; equal to
(op-orelse/op-andalso expression-1
                      (op-orelse/op-andalso expression-2
                                            expression-3))
```
#### attribute, behaviour

`attribute` define a global named value for a module. And `behaviour` is a special attribute directive to tell the Erlang compiler that the module implement some callbacks following some general rules. They have the following syntax in Kapok:

```
;; define an attribute `name` with value to be `value`
(attribute name value)

;; declare a behaviour this module implements
(behaviour name)
```

Besides, there are util macros in the standard library `kapok.core` such as `attr`, `get-attr` to access the module attributes.
