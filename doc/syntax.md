Syntax
==========

## Types

### primitive types

#### Integer

An integer could be represented in these ways:

1. trandition notation

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
;; or represent them in keyword
:true  ;=> boolean true
:false ;=> boolean false
```

Please notice in most Lisp dialects, `nil` is logically false in conditionals. But in Kapok, there is no `nil` and the only logically false is atom false.

#### Comment

#### Space and Commas

#### Containers

