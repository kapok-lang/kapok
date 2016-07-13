Introduction
==========

Kapok is a Lisp dialect designed to support general programming with functional style. It's a powerful, easy-to-use programming language with rich libraries and features. Kapok is implemented as a compiler and libraries, and keeps compatible with the Erlang ecosystem.

## Examples

Let's take a look at some examples and see what it looks like, which is the very first thing that everyone would like to do when exploring a new language.

#### Sieve of Eratosthenes

Here we see the Sieve of Eratosthens implemented as a Namespace.

```clojure
(ns sieve-of-eratosthenes
  (require lists)
  (use (io :only (format)))
  (use (erlang :only (/=
                      rem
                      (+ 2))
               :rename (((/= 2) !=)))))

(defn- sieve [[] primes]
  (lists.reverse primes))

(defn- sieve [[h & t] primes]
  (sieve (lists.filter (fn [x]
                         (!= (rem x h) 0))
                       t)
         [h & primes]))

(defn sieve [v]
  (sieve (lists.seq 2 v) []))

(defn main []
  (let [v 1000]
    (format #"run sieve(~B) return: ~p~n" [v (sieve v)])))
```

The first thing which must occur in a source file is the namespace special form. You can call a fully qualified macro to create the namespace, but that macro **must** create the namespace first. A namespace is defined with the `ns` special form.

```clojure
(ns sieve-of-eratosthenes
  (require lists)
  (use (io :only (format)))
  (use (erlang :only (/=
                      rem
                      (+ 2))
               :rename (((/= 2) !=)))))
```

The ns special form consists of three main parts (we will go into more details later in other portion of this document). The first part is the name of the namespace, which is an identifier. It's generally a good idea for the namespace name to match the file name, although this is not a must.

The second part of the ns special form is the `require` form. The require form provides a list of those namespaces that will be used by this namespace. This is not strictly required (namespaces which are used in this namespace being defined but not required will be automatically required). However, it **is** very good documentation and all namespaces should be required before used.

The third part is the `use` form. The use form allow you to import functions and macros into this namespace being defined. So you do not have to write the fully qualified name. This is especially useful for functions and macros which are used often(e.g. as a operators). Don't go crazy with it though. It is a spice that should be used only where it enhances clarity.

Any number of require and use expression can appear in the ns special in any order.

Next we see the function definitions

```clojure
(defn- sieve [[] primes]
  (lists.reverse primes))

(defn- sieve [[h & t] primes]
  (sieve (lists.filter (fn [x]
                         (!= (rem x h) 0))
                       t)
         [h & primes]))
```


