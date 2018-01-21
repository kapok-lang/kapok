Data Types
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

#### <a id="char">Char</a>

A single character literal could be represented in either `$c`, `$\xhh` or `$\x{hhhhhh}`.

`$c` is for special escape charactor and other printable ascii character. There are a few special escape chars as listed below:

| char | stands for | integer value(in decimal) |
| --- | --- | --- |
| $\a | bell | 7 |
| $\b | backspace | 8 |
| $\d | delete | 127 |
| $\e | escape | 27 |
| $\f | form feed | 12 |
| $\n | line feed | 10 | 
| $\r | carriage return | 13 |
| $\s | space | 32 |
| $\t | horizontal tab | 9 |
| $\v | vertical tab | 11 |
| $\x | the prefix of hexadecimal format char(described below) | |

These escape chars are also available in string literal, e.g., it's written `"\n\t"` if you need a binary string which consist of one line feed character and then one horizontal tab.

`$\xhh` is the hexadecimal format for ascii characters, e.g. `$\xa` is a line feed character with value 10 in base-10 notation. And `$\x41` is char 'A'.

`$\x{hhhhhh}` is for utf8 character. You could put 1-6 hexadecimal digit(0-9, a-f, A-F) inside `{}` to represent a code point in utf8 encoding.

#### String

There are two string types: list string, binary string.

In Erlang, string is represented by character list. so you could get this in interative shell

```Erlang
> "hello" == [$h, $e, $l, $l, $o].
true
```

In Kapok, we use the syntax `#"some string"` for this traditional character list string, it's call list string.

Meanwhile Kapok adds a string type called binary string, with the syntax `"some string"`. It is represented as binary interally, and provide a modern utf8 string implemetation.

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

The specical escape character are supported in both string types. Refer to previous [Char](#char) section for more info.

#### Atom

Atoms are used to represent constant values in Erlang. They are global constants evaluated to themself.

In Kapok, there are a few ways to write a literal atom:

```clojure
#atom
#'this atom have space, so we have to use single-quotes as terminators'
```

Each of them starts with a # character, followed by a identifier. If there is any space/tab or any other non-printable character, you need to use single-quotes as terminators. It is not recommanded that using a complex combination of non-printable characters or lots of non-printable characters for atoms, since that would be hard to read and write.

#### Keyword

Keywords are like atoms, they represent constant values and are evaluated to themself.
Keywords have different prefix comparing to atoms, you could write keywords as below:

```clojure
:name
:'this keyword have space, so we have to use single-quotes as terminators'
```

Each of them starts with a : character, followed by a identifier. If there is any space/tab or any other non-printable character, you need to use single-quotes or double-quotes as terminators. That same recommandation for atoms applies to keyword as well. It is not good to use a complex combination of non-printable characters or lots of non-printable characters for keywords.

Keywords are used in these occasions.

1. special forms and literal types

  Keywords are widely used in literal types and special forms, for example

  ```clojure
  ;; boolean
  :true
  :false

  <<(75 (:size 8) :big :unsigned :integer (:unit 1)) 97 (112 :native) 111 (75 (:unit 1))>>

  ;; a ns special form
  (ns sample-ns
    (use (io :only (format))))
  ```
 
2. function arguments

  Keywords could be used in key-value arguments for functions as in Common Lisp.

  ```clojure
  (defn f [&key (key1 1) (key2 2)]
    ...
    )

  (f :key1 value1 :key2 value2)
  ```

  Except for these special boolean keywords: `:true`, `:false`, `:nil` (refer to below [Boolean](#boolean) section for more info). Keywords occur in function arguments would be translated to the keys of key-value arguments. But the special boolean keywords mentioned above would be always treated as literal values, even if they are present in the argument list. E.g.
  
  ```clojure
  (defn f [&key a]
    ...
   )
   
  (f :a :true)      ;; via key `:a`, var `a` is set to value `:true`
  
  (g :true :true)   ;; call function `g` with two arguments which are literal boolean `:true`, the first `:true` doesn't present a key
  ```
  
  With such a keyword priority definition, these special keywords `:true`, `:false`, `:nil` could never be used as function argument keys. E.g., it causes a compile error to write
  
  ```clojure
  (defn h [&key (true 1)]
    ...
   )
  ```
  
  The identifier `true` is only allowed to used as argument key, the same rule applies to `false` and `nil`. Please use another name as you could always find one.

  Notice that Clojure and other Lisp dialects based on Erlang VM, such as LFE and Joxa don't support key-value arguments for function.

3. map accessors and constants

  Keywords are implemented as atoms in Erlang. So except for these usages above, keywords and atoms are identical and interchangeable in other occasions. For example, it's ok to using keywords or atoms as map keys, global contants, etc. Please follow the same convention to use one of them in the same occasion consistently. It's recommand that using keyword for map keys, and using atom for globla contants.

Notice that Clojure supports namespaced keywords, which gives the same keyword different meanings for different namespaces. In Kapok, keywords are global and used without namespace.

#### <a id="boolean">Boolean</a>

Like what's in Erlang, In Kapok there is no distinct boolean type; instead, the keywords true and false are given a special interpretation and are used to represent boolean literals.

```clojure
#true  ;=> evaluate to boolean true
#false ;=> boolean false
;; or write them in keywords
:true  ;=> boolean true
:false ;=> boolean false
```

Since atom and keywords share the same implementation in Erlang VM indeed, the atom `#true`, `#false` act as the same boolean values. But it's a convention to use `:true` and `:false`, which are in the keyword format, as the boolean true and false in Kapok source code. Don't mess them in source code.

Please notice in most Lisp dialects, `nil` is logically false in conditionals. But in Erlang, there is no `nil` and the only logically false is atom false. In Kapok. if you want to use `nil` or forms which rerturns `nil` as a boolean, please use the standard library functions: `nil?`, `false?`, `true?`.

```clojure
;; use `nil?`
(nil? :nil)       ;=> :true
(nil? [])         ;=> :true
(nil? :false)     ;=> :true
(nil? #abc)       ;=> :false

;; `false?` is just an alias to `nil?`, so it returns the same result as `nil?`.
(false? :nil)       ;=> :true
(false? [])         ;=> :true
(false? :false)     ;=> :true
(false? #abc)       ;=> :false

;; `true?` reverses the result of calling `nil?` 
(true? :nil)       ;=> :false
(true? [])         ;=> :false
(true? :false)     ;=> :false
(true? #abc)       ;=> :true
```

Please notice in the following occasions only the Erlang strict version booleans are allowed:

```text
guards (function, case)
inter-operations with Erlang function which expects booleans
```

#### Comment

Comments are prefixed by a semicolon. All content following a semicolon is ignored entirely. These are equivalent to // in C and Java, and # in Ruby and Python. These conventions for comments are recommended as usual in Lisp:

1. ';'

    Comments that start with a single semicolon, ';', should all be aligned to the same column on the right of the source code. Such comments usually explain how the code on that line does its job. For example:
    
    ```clojure
    (defn in [n start end]
      (and (>= n start)  ; check whether n equal-or-bigger than start
           (<= n end)    ; check whether n less-or-equal than end
       ))
    ```

1. ';;'

    Comments that start with two semicolons, ';;', should be aligned to the same level of indentation as the code. Such comments usually describe the purpose of the following lines or the state of the program at that point. For example:
    
    ```clojure
    (let [_ (setup-state)]
      ...
      (some-action)
      
      ;; verify that state is still valid
      (verify-state)
      )
    ```

    We also normally use two semicolons for comments outside functions.
    
    ```clojure
    ;; Traverse the syntax tree and add tags to internal nodes
    (defn visit-syntax-tree []
      ...
      )
    ```
    
    Note that these comments are different from the function docs. Comments are not stored and access via dev tools like docs. If you want to put down some text to explain what the function does and how to call it properly, to explain precisely what each argument means and how the function inteprets its possible values, it's much better to write docs.
    
1. ';;;'

    Comments that start with three semicolons, ';;;', should start at the left margin. We use them for comments which should be considered a heading of major sections of a program. More semicolons such as four ';;;;', are useful when the codes have complex hierarchy of sections. Usually heading comments are not used within a function.

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

### Collections

#### Bitstring and Binary

Bitstring and binary in Kapok are samilir to their counterparts in Erlang. A bitstring is a sequence of bits, and a binary is a sequence of bytes. Both bitstring and binary represent a pack of bits except the number of bits in bitstring is not exactly divisible by 8. And they share the same syntax as below:

```clojure
;; in list string
<<#"hello">>

;; in binary string
<<"hello">>

;; in integer lists
<< 5 10 20 >>

;;; in bit syntax

;; with default type specifier list
<<(2 (:size 5)) (61 (:size 6)) (20 (:size 5))>>

;; with specified type specifier list
<<(2 (:size 5) :little :unsigned :integer (:unit 1))
  (61 (:size 6) :little :unsigned :integer (:unit 1))
  (20 (:size 5) :little :unsigned :integer (:unit 1))>>

;; or the mix of all above

(let [v 96]
  << "hello" v (2 (:size 5)) (20 (:size 5) :little :unsigned :integer (:unit 1)) $? >>)
```

Between its special terminators `<<` and `>>`, a bitstring/binary could consisted of 

1. A literal list string or a literal binary string
1. Elements in a sequence. Each element could be an integer, float, list string, binary string, indentifier or an expression in bit syntax.

With paretheses `(`, `)` as terminators, an expression in bit syntax is call a bit syntax expression. Each expression represents an element of the whole bitstring or binaly. For a simple element which is a simple integer, list string, binary string or indentifier, paretheses could be omitted since it's clear enough it's a standalone element. For example:

```clojure
;; the following binary are equal
<< ($h) ($i) >>
<< $h $i >>
<< "hi" >>
(let [a $h]
  << a $i >>)
```

The bit syntax in Kapok is similar to its counterpart in Erlang. It could be taken as a parenthesized version of bit syntax in Erlang. In general, a bitstring or binary is constructed by a sequence of bit syntax expressions. It has the following form:

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

List is the essential to every Lisp dialect: the language syntax is mainly composed of lists. There are two kinds of list in Kapok, the general list and the literal list, sometimes the later is also called list for short.

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
2. Square brackets are used for literal vector type in Clojure. And vector is used often in Clojure code. For instance, the parameters are put inside a vector in a function definition. We need to support defining multiple clauses for a function name, it's samilar to define a function with multiple arities in Clojure, so the spuare brackets are needed for the syntax.
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

Records are tagged tuple in Erlang. It usually represents a fixed number of data using predetermined atoms as names. A record is declarated in the following syntax:

```erlang
-record(Name, {
               %% the next key has default value
               key1 = DefaultValue1,
               %% the next line is equivalent to
               %% key2 = undefined
               key2,
               ...
               })
```

Usually a record declaration is put down in a header file(with extension .hrl), which is then included by other Erlang source code files to ensure the record is consistent all over its usage.

Generally it's not recommended to use records in Kapok. There is another type of composed data structure, struct(Refer to [Struct](#struct) section for more info), which is used to hold a group of data. And protocols could be defined to work along structs to archive dynamic dispatching during runtime. Sometimes we have to deal with Erlang libraries which uses records in their interfaces, in these cases records must be used. Otherwise structs should be used rather than records.

There are util functions defined in the Kapok standard library for records, such as `defrecord`(define a record) `extract`(extract records from file), etc. Check them out if you need to work with records.

#### Map

A maps is an associative collection of key-value pairs, just are like a map(or a dictionary) in other programming languages. The syntax for literal map in Kapok is a combination of Erlang and Clojure.

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
#{#a 1 #b 2}
```

The surrounding `#{}` comes from Erlang. And the key-value pairs are matched by their positions, which is like Clojure. Also notice that `#{}` are used for literal set in Clojure.

Maps in Kapok are implemented as their counterparts in Erlang. You could also use maps in pattern matching likewise:

```clojure
(let [m #{#a 1
          #b 2}]
  (let [#{#a v} m]
    v))
;;=> 1
```

In the second `let` expression, we omit key `#b` in the pattern and only fetch the value of key `#a`, and refer the value as the local `v`.

#### Set

A set is a collection of elements with no duplicate elements, like a set in other programming languages. The syntax for literal map is

```lisp
%{1 2 3}
```

Sets in Kapok are implemented as `gb_sets` in Erlang. They are not supported in pattern matching. Although there are functions to manipulate sets in the Kapok standard library, currently sets are not supported in most of the protocol defined in the standard library, such as `seq` protocol, etc. Efforts would be taken to add support for sets in these protocols in the future.

### <a id="struct">Struct</a>

A struct is a tagged map to wrap a limited number of data with predefined names. The keys for a struct musts be atoms or keywords. A struct definition sits inside the module. The name of the module becomes the name of the map type. Inside the module, the `defstruct` mocro is used to define the map's characteristics. For example:

```clojure
(defns customer
  (defstruct [kapok.access]
    :a            ;; default value `:nil`
    (:b :nil)
    (:c (+ 1 1))
    :d))
```

The defstruct macro takes a optional literal list of derive protocol, and a sequence of fields as its arguments. Each field could be either:

1. A keyword or atom for the field name, with default value `:nil`
1. A general list of name, default value pair, as the example above

And the defstruct macro will generate a `new` function as a constructor of this struct, and corresonding functions to hook up elixir protocol impl, including derived protocol mapping.

And then we could use this struct as a map, or manipulate it by protocol interface:

```clojure
(let [s (customer.new :a "no news")]
  (io.format "s: ~p~n" [s])
  (io.format "a: ~p~n" [(maps.get #a s)])
  (io.format "d: ~p~n" [(access.get s #d)]))
;; the above code outputs:
;; s: #{'__struct__' => customer,a => <<"no news">>,b => nil,c => 2,d => nil}
;; a: <<"no news">>
;; d: nil
```

A struct is usually connected to protocols. When a struct is defined to derive some protocols, the protocols must be implemented by maps, and this `derive` means to map the protocol implementation for `map` to this struct. Sometimes we need to implement a protocol for a struct using `defimpl` protocol. Refer to protocol section for elaborate details of protocol, its implementation and its interaction with structs.
