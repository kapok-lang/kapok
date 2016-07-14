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

1.0 3.14159 -2.3e+6 23.56E-27

All floating-point numbers are transformed to Erlang floating-point numbers, which is internally in IEEE 754 64-bit format, and limited in the range 10^-323 to 10^308.


