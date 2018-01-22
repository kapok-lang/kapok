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

One last thing about these condition forms: that the value of "test" expression, or "condition" expression mentioned above, is checked by the `true?` or `:false?` function in `kapok.core`. They treat `:nil`, `[]`, `:false` to be logical false, and any other value to be logical true. This is different than Erlang, where literally atom `:false` is taken as logical false, atom `:true` is logical true. This may be tricky when you need a boolean value to interact with Erlang library interfaces. You could refer to the [boolean data type](./data-type.md#boolean) for more info.

#### try

#### fn

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

#### Operators

op-not
op-or, op-and, op-xor
op-andalse, op-orelse

#### attribute, behaviour
