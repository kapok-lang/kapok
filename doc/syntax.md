Syntax
==========

### Primitive Types

#### Integer

An integer could be represented in these ways:

1. tranditional notation

    It starts with optional + or - sign, a non-zero digit, and then other digits. E.g. -101, 987654321, +2016.

2. Octal notation

    Literals starting with a zero are interpreted as octal numbers. For example, the octal 040 is 32 in the usual base-10 notation.

3. Hexadecimal notation

    Just as in most languages, typical hexadecimal notation is supported. 0xff is 255, 0xd055 is 53333, and so on.

4. Flexible numeral bases

    You can specify the base of an integer in a prefix BrN, where N is the digits that represent the desired number, and B is the base or radix by which N should be interpreted. So we can use a prefix of 2r for binary integers (2r111 is 7), 16r for hexadecimal (16rff is 255), and so on. This is supported up to base 36.

All integers will be transformed to Erlang Integers, so they are big number integers and could have arbitrary big value.

Also notice that there is no literal syntax for rational numbers, which is quite common in Lisp dialects.

#### Float

A floating-point number has five parts: an optional sign, a whole number part, a decimal point, a fractional part, and an optional exponent part.

Here are some examples of floats:

```clojure
1.0
3.14159
-2.3e+6
23.56E-27
```

All floating-point numbers are transformed to Erlang floating-point numbers, which is internally in IEEE 754 64-bit format, and limited in the range 10^-323 to 10^308.

#### Char

A single character literal could be represented in either `\xhh` or `\x{hhhhhh}`.

`\xhh` is for ascii character, e.g. `\xa` is a LineFeed character with value 10 in base-10 notation. And `\x41` is char 'A'.

`\x{hhhhhh}` is for utf8 character. You could put 1-6 hexadecimal digit(0-9, a-f, A-F) inside `{}` to represent a code point in utf8 encoding.

For some special characters, there are some constant literals:

```clojure
\space
\tab
\formfeed
\backspace
\newline
\return
```

#### String

There are two string types: character list, binary string.

In Erlang, string is represented by character list. so you could get this in interative shell

```Erlang
> "hello" == [$h, $e, $l, $l, $o].
true
```

In Kapok, we use the syntax `#"some string"` for this traditional character list string, it's call list string.

Meanwhile Kapok add a string type called binary string, with the syntax `"some string"`. It is represented as binary interally, and provide a modern utf8 string implemetation.

Both list string and binary string are naturally multiline-capable, without any special syntax (as in, for example, Python):

```Clojure
"multiline strings
are very handy"
;= "multiline strings\nare very handy"
```

If there is any " char inside the string, escape char could be used to escape that char

```
"This string need an escape char, since \" is a terminator"
```

Or, use the triple quote separators instead of single one.

```
"""This string need no escape, since a single " would not be mistaken for terminator.
And it support multiline as well."""

'''The triple single-quotes act the same as triple double-quotes as string terminator.'''
```

#### Symbol

Symbols are names (or called identifiers) which could represent namespaces, functions, variables, etc.

Symbols must begin with a non-numeric character, and in addition to any alphanumeric characters, it could contain these characters:

```text
! $ % # + - / < = > ? @ _ | ~ & # ^
```

Like other Lisp dialects, the valid characters for symbols is far more than non-Lisp language. For example, valid characters for identifiers in Python could only contain alphanumeric characters and underscore. Notice that the last a few characters are preserved for some other keywords or literal types, as listed below:

```clojure
~ ~@                    ;; is macro unquote, unquote splicing keyword
&optional &rest &key    ;; function argument specification
#""                     ;; list string
^                       ;; atom
```

Using `~ & # ^` as the start of a symbol would not works. But you could use them in any position after the first char in a symbol, as long as it would not cause confusion with the preserved literal types.

Also notice that symbols/identifiers in other Erlang VM based programming languages have fewer valid characters. For example, identifiers in Elixir contain only alphanumeric characters and underscore. If you need to write a Kapok module for Elixir code to call, please make the symbol name to be compactible.

If a symbol that start with underscore(_) and with tailing other characters, the Kapok compiler would not report warning if the symbol is not used. If a symbol is a underscore, it acts like a placeholder. For example, if we didn’t need to capture a value during the pattern matching, we could specify the special variable _ (an underscore). This acts like a variable but immediately discards any value given to it—in a pattern match, it is like a wildcard saying, “I’ll accept any value here.”

A symbol that contains a dot character(.) denotes a namespaced-symbol, and will evaluate to the named value in the specified namespace. For example, we could specify a embedded namespace in the ns special form, as

```clojure
(ns com.kapok.some-module
  ;; ...
  )

(defn f []
  ;; ...
  )
```

Or call a function of this namespace like this

```clojure
(com.kapok.some-module.f ...)
```

Multiple dot characters could occur in a single symbol, which is a way to support multiple embedded levels of namespaces and their members.

#### Atom

Atoms are used to represent constant values in Erlang. They are global constants evaluated to themself.

In Kapok, there are a few ways to write a literal atom:

```clojure
^true
^'this atom have space, so we have to use single-quotes as terminators'
^"this atom uses double-quotes instead of single-quotes as terminators"
```

Each of them starts with a ^ character, followed by a symbol. If there is any space/tab or any other non-printable character, you need to use single-quotes or double-quotes as terminators. It is not recommanded that using a complex combination of non-printable characters or lots of non-printable characters for atoms, since that would be hard to read and write.

#### Keyword

Keywords are like atoms, they represent constant values and are evaluated to themself.
Keywords have different prefix comparing to atoms, you could write keywords as below:

```clojure
:name
:'this keyword have space, so we have to use single-quotes as terminators'
:"this keyword uses double-quotes instead of single-quotes as terminators"
```

Each of them starts with a : character, followed by a symbol. If there is any space/tab or any other non-printable character, you need to use single-quotes or double-quotes as terminators. That same recommandation for atoms applies to keyword as well. It is not good to use a complex combination of non-printable characters or lots of non-printable characters for keywords.

Keywords are used in these occasions.

1. special forms and literal types

  Keywords are widely used in special forms and literal types, for example

  ```clojure
  ;; a ns special form
  (ns sample-ns
    (use (io :only (format))))

  <<(75 (:size 8) :big :unsigned :integer (:unit 1)) (97) (112 :native) (111) (75 (:unit 1))>>
  ```
 
2. function arguments

  Keywords could be used in key-value arguments for function as in Common Lisp.

  ```clojure
  (defn f [&key (key1 1) (key2 2)]
    ...
    )

  (f :key1 value1 :key2 value2)
  ```

  Notice that Clojure and other Lisp dialects based on Erlang VM, such as LFE and Joxa don't support key-value arguments for function.

3. map accessors and constants

  Keywords are implemented as atoms in Erlang. So except for these usages above, keywords and atoms are identical and interchangeable in other occasions. For example, it's ok to using keywords or atoms as map keys, global contants, etc. Please follow the same convention to use one of them in the same occasion consistently. It's recommand that using keyword for map keys, and using atom for globla contants.

Notice that Clojure supports namespaced keywords, which gives the same keyword different meanings for different namespaces. In Kapok, keywords are global and used without namespace.

#### Boolean

Boolean type in Kapok is the same with Erlang. There is no distinct boolean type; instead, the atoms true and false are given a special interpretation and are used to represent boolean literals.

```clojure
^true  ;=> evaluate to boolean true
^false ;=> boolean false
;; or write them in keywords
:true  ;=> boolean true
:false ;=> boolean false
```

Please notice in most Lisp dialects, `nil` is logically false in conditionals. But in Erlang, there is no `nil` and the only logically false is atom false. In kapok. if you want to use `nil` or forms which rerturns `nil` as a boolean, please use the standard library function

```clojure
;; use `nil?`
(nil? ^nil)       ;=> ^true
(nil? [])         ;=> ^true
(nil? ^false)     ;=> ^true
(nil? ^abc)       ;=> ^false
;; `true?` reverses the result of calling `nil?` 
(true? ^nil)      ;=> ^false
```

Please notice in these occasions only the Erlang strict version booleans are allowed:

```text
guards (function, case)
inter-operations with Erlang function which expects booleans
```

#### Comment

Single-line comments are indicated by prefixing the comment with a semicolon (;); all content following a semicolon is ignored entirely. These are equivalent to // in C and Java, and # in Ruby and Python.

The form-level comment using the `#_` reader macro in Clojure is not supported currently.

#### Space and Commas

Most of the time, there is no commas between forms, parameters to function calls, elements in data structure literals, and so on:

```clojure
(defn some-function [x y]
  (+ x y))
```

Because spaces are enough to separate them. If you feel like adding commas, you could have

```clojure
(defn some-function [x, y]
  (+, x, y))
```

These commas are considered whitespace and striped after source code is parsed. Whether to use commas or not is entirely a question of personal style and preference. They could be used when doing so will enhance the human readability of the code. It is most common in cases where pairs of values are listed, but more the one pair appears per line:

```clojure
;; a literal map constructed by two keyword-value pairs in one line
#{:name new-username, :email email}
```

### Containers

#### Bitstring and Binary

Bitstring and binary in Kapok are samilir to what in Erlang. A bitstring is a sequence of bits, and a binary is a sequence of bytes. Both bitstring and binary represent a pack of bits except the number of bits in bitstring is not exactly divisible by 8. And they share the same syntax as below:

```clojure
;; in list string
<<#"hello">>
;; in binary string
<<"hello">>
;; in integer lists
<<(5), (10), (20)>>
;; in bit syntax
;; with default type specifier list
<<(2 (:size 5)) (61 (:size 6)) (20 (:size 5))>>
;; with specified type specifier list
<<(2 (:size 5) :little :unsigned :integer (:unit 1))
  (61 (:size 6) :little :unsigned :integer (:unit 1))
  (20 (:size 5) :little :unsigned :integer (:unit 1))>>
```

The bit syntax in Kapok is similar to what it is in Erlang. It could be taken as a parenthesized version of bit syntax in Erlang.

Bit syntax expressions are used to constructed binaries and bitstrings. They have the following form:

```clojure
<<>>
<<E1, E2, ..., En>>
```

Each element Ei specifies a single segment of the binary or bitstring. Each element Ei can have one of four possible forms.

```text
Ei = (Value) |
     (Value (:size Size)) |
     (Value <TypeSpecifierList>) |
     (Value (:size Size) <TypeSpecifierList>)
```

If the total number of bits in the expression is evenly divisible by 8, then this will construct a binary; otherwise, it will construct a bitstring.

when you construct a binary, `Value` must be a bound variable, a literal string, or an expression that evaluates to an integer, a float, or a binary. When used in a pattern matching operation, `Value` can be a bound or unbound varibale, integer, literal string, float, or binary.

`Size` must be an expression that evaluates to an integer. In pattern matching, `Size` must be an intger or a bound variable whose value is an integer. `Size` must be a bound variable, at the point in the pattern where the value is needed. The value of the `Size` can be obtained from earlier pattern matches in the binary. For example, the following:

```clojure
<<(Size (:size 4)) (Data (:size Size) :binary) ...>>
```

is a legal pattern, since the value of `Size` is unpacked from the first four bits of the binary and then used to denote the size of the next segment in the binary.

The value of `Size` specifies the size of the segement. The default value depends on the type. For an integer it is 8, for a float it is 64, and for a binary it is the size of the binary. In pattern matching, this default value is valid only for the very last element. If the size of segement is not specified, a default value will be assumed.

`<TypeSpecifierList>` is a list of items `End Sign Type Unit`. Any of the items can be omitted, and the items can occur in any order. If an item is omitted, then a default value for the item is used.

The items in the specifer list can have the following values:

```text
End = :big | :little | :native
```

This specifies the endianess of the machine. `:native` means that the endianess will be determined at runtime to be either big-endian or little-endian, depending upon the CPU which the Erlang VM is run on. The default is `:big`, which is also known as nekwork byte order. 

```text
Sign = :signed | :unsigned
```

This parameter is used only in pattern matching. The default is `:unsigned`.

```text
Type = :integer | :float | :binary | :bytes | :bitstring | :bits | :utf8 | :utf16 | :utf32
```

The default is `:integer`. The default type does not depends on the value, even if the value is a literal. For instance, the default type in the only segment of `<<(3.14)>>` is `:integer` not `:float`.

```text
Unit = (:unit 1|2|...256)
```

The default valueu of `Unit` is 1 for `:integer`, `:float`, and `:bitstring` and is 8 for `:binary`. No value is required for types `:utf8`, `:utf16`, `:utf32`.

The total size of the segement is `Size` * `Unit` bits long. A segment of type `:binary` must have a size that is evenly divisible by 8.

Although there is a literal type binary string, which is implemented as binary. It's remcommended that use always use the right syntax for the corresponding type, e.g. don't use the binary syntax when a string is needed. It helps to clarify the source code.

#### List and Literal List

List is the essential to every Lisp dialect: the language syntax is mainly composed of lists. There are two kinds of list in Kapok, the general list and the literal list, the later is usually called list for short.

```clojure
;; a general list
(a b c 1 2 3)
;; a literal list
[a b c 1 2 3]
```

Lists in Lisps are often called s-expression or sexprs -- short for symbolic expressions. The rules for the evaluation of Lisp code is simple:

1. Lists (denoted by parentheses) are calls, where the first value in the list is the operator and the rest of the values are parameters.
2. Symbols evaluate to the named value in the current scope, which can be a named local value, a function, a macro or a special form.
3. All other expressions evaluate to the literal values they describe.

The literal list means to represent the list type for data. So a literal list is treated as a literal data, just as a tuple or a map. It will not be evaluated as macro/function calls.

There are a few reasons to separate the syntax of literal list from general list:

1. List in Erlang has the syntax of square brackets. And we would like to keep the syntax of data list type compactible with Erlang.
2. Square brackets are used for literal vector type in Clojure. And vector is used often in Clojure code. For instance, the parameters are put inside a vector in a function definition. We need to support defining multiple clauses for a function name, it's samilar to define function with multiple arities in Clojure, so the spuare brackets are needed for the syntax.
3. Adding a new syntax for data list would help to clarity the code, although it would add complexity as well.

So we combine the syntax of list in Erlang and the syntax of vector in Clojure, and add a literal list type to use this syntax.

#### Tuple

A tuple in Kapok are just a tuple in Erlang, which is a single entity to group a fixed number of items. It works like anonymous struct in C and is usually used as a short, internal data. The syntax for literal tuple in Kapok is the same with Erlang as well.

```clojure
{10, 45}
{"foo", "bar", "foobar"}
```

Please notice that curly braces are used for literal map in Clojure. We use them for tuple in Kapok.

#### Record

**TODO** impl record and write docs for it

#### Map

Maps in Kapok are just maps in Erlang, which are associative collections of key-value pairs. They are like maps(or dictionaries) in other programming languages. The syntax for literal map in Kapok is a combination of Erlang and Clojure.

```erlang
%% a map in Erlang
#{a => 1, b => 2}
```

```clojure
;; a map in Clojure
{a 1 b 2}
```

```clojure
;; a map in Kapok
#{^a 1 ^b 2}
```

The surrounding `#{}` comes from Erlang. And the key-value pairs are matched by their positions, which is like Clojure. Also notice that `#{}` are used for literal set in Clojure.

#### Set

Sets are collections of elements with no duplicate elements. In Kapok sets are implemented as `gb_sets` in Erlang. They are like sets in other programming languages. The syntax for literal map is

```lisp
%{1 2 3}
```

**TODO** add operators

### Namespace

Namespaces in Kapok are like namespaces in Clojure, which are named space to hold function definitions. They are roughly analogous to packages in Java and modules in Python and Ruby. The difference is that Kapok is a functional programming language and does not support constant and variables in namespace.

Every Kapok namespace is implemented and transformed to a Erlang module, with the name of Kapok namespace mapping to the corresponding Erlang module name. One special form `ns` could be used to define a namespace. A basic namespace declaration looks as below:

```clojure
(ns some-module)

;; the followings are function/macro definitions
```

This defines a namespace called `some-module`, and the function/macro definitions follow this `ns` special form. The namespace must be defined before the function/macro definitions, like what we usually do in Clojure (in Erlang, module declaration is written at the start of a module as well). A namespace is usually put into a single file, and it is a must that a single source file only contains a single namespace. It is recommand that the source file have the same name as namespace, although it is not a must. For example

```clojure
;; the content of file my-domain.example.kpk
(ns my-domain.example)

(defn do-something []
  ;; ....
  )
```

Both the module name and the file name without suffix are `my-domain.example`. Keeping them the same would make it easier to search for namespaces, no matter for project management tools or human.

You might notice that the namespace name in previous example is not a identifier, since it contains a dot(`.`) charactor, which is not a valid identifier charactor. When dot characters are put between identifiers, they are composed to new syntax entity called a dot-identifier. A dot-identifier could be used as the namespace name, first element of a list call (to call a remote function of another namespace). The dot character is to separate the identifiers and to form a name hierarchy, like domain. In Clojure, dot character is used for package hierarchy, and slash(/) character is used between packages and their members, e.g.

```clojure
;; call a function true? in namespace clojure.core
(clojure.core/true? nil)
```

In Kapok, we use dot character for both of these occasions.

```clojure
;; call a function true? in namespace kapok.core
(kapok.core.true? ^nil)
```

The `ns` special form could have `require` and `use` clauses. The `require` clause could have `:as` argument. And the `use` clause could have `:as`, `:only`, `:exclude`, `:rename` arguments. These examples below would show how to use them:

```clojure
(ns example-for-require
  (require io)               ;; require a single erlang module 'io'
  (require (base64 :as bs))  ;; require a single erlang module 'base64' and give it an alias 'bs'
  (require compile           ;; require multiple erlang modules
           (rand :as r)
           re)
  (require kapok.atom)       ;; require a kapok standard library namespace
  )
```
  
```clojure
(ns example-for-use
  (use lists)                ;; use a single erlang module 'lists'
  (use (erlang :as er        ;; use a erlang module 'erlang' and give it an alias 'er'
               :only (apply node)  ;; only import apply/2 and node/0 from module 'erlang'
               :rename (apply ap)  ;; rename 'apply' to 'ap'
               ))
  (use kapok.process                ;; use multiple modules/namespaces
       (kapok.core :as core
                   :exclude (abs))  ;; :exclude agrument
       gb_sets)
  )
```

`:as`, `:rename` are used to give aliases to modules/namespaces or functions/macros. When there is name clash, or a shorter name is wanted, aliases would be helpful. Since `:only` is inclusive, but `:exclude` is exclusive, they could not be used together in the same use clause.

### Binding and Pattern matching

Like Clojure, the special form to define local bindings in Kapok is `let`.


TODO add examples for local bindings

And `let` supports destructing like Clojure. In Erlang, there is a similar concept of destructing, called pattern matching. There are two kinds of destructing

1. Sequential destructing

  Sequential destructing works with the below types: lists, tuples, bitstring, binary, binary string.

2. Map destructing

  Map destructing is conceptully identical to sequential destructing, except that it works only for maps.

TODO add more details for destructing.

Please notice that set is not supported in destructing.

### Block expression

### Conditional

### Exception

### Function and Lambda Function
#### Guard

### Macro
