Introduction
==========

Kapok is a Lisp dialect designed to support general programming with functional style. It's a powerful, easy-to-use programming language with rich libraries and features. Kapok is implemented as a compiler and libraries, and keeps compatible with the Erlang ecosystem.

## Examples

Let's take a look at some examples and see what it looks like, which is the very first thing that everyone would like to do when exploring a new language.

#### Sieve of Eratosthenes

Here we see the Sieve of Eratosthens implemented as a namespace.

```clojure
(ns sieve-of-eratosthenes
  (require lists)
  (use (io :only (format)))
  (use (erlang :only (/=
                      rem
                      +)
               :rename ((/= !=)))))

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
[source file](../example/sieve-of-eratosthenes.kpk)

The first thing which must occur in a source file is the namespace special form. You can call a fully qualified macro to create the namespace, but that macro **must** create the namespace first. A namespace is defined with the `ns` special form.

```clojure
(ns sieve-of-eratosthenes
  (require lists)
  (use (io :only (format)))
  (use (erlang :only (/=
                      rem
                      +)
               :rename ((/= !=)))))
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

We define a function called `sieve` that takes two arguments. The first argument is a list, and the second argument `primes`. There is a pattern matching for the first argument. If it is a empty list, the first clause of function `sieve` will be run. Or, if the first argument is a non-empty list, the second clause will be run and a recursive call of `sieve` will be invoked.

And then, we define our public api

```clojure
(defn sieve [v]
  (sieve (lists.seq 2 v) []))
```

Notice that we define three clauses for the same function name `sieve`. The dispatching of different clauses is based on argument number and the pattern matching. Also notice that we use `defn` to define the third clause, while in these previous clauses we use `defn-`. There are two types of function definitions in Kapok (which is similar to clojure): *public* and *private* functions. Public functions are exported and available outside of the namespace, while private functions are only available inside the namespace itself. The difference in declaration is the use of `defn` for public functions in place of `defn-` for private functions. In this example you see us call the private `sieve` function. In Kapok, functions must be defined before they are used. So the private function `sieve` with 2 arguments must be defined before they are used in public function `sieve` with 1 argument.

Finally, we get to the last part of this namespace: the `main` function.

```clojure
(defn main []
  (let [v 1000]
    (format #"run sieve(~B) return: ~p~n" [v (sieve v)])))
```

The public function with the name `main` and no argument is a special function to a namespace. It is an entrance and start point to the whole namespace and the program. This is simaliar to other programming languages like Clojure, Python, Golang. If a soure file is run as script in command line as below:

```shell
## assume kapok is in env var "PATH"
$ kapok some-namespace.kpk
```

Then if the namespace in file `some-namespace.kpk` has a public function named `main` without any argument, this function `main` would be invoked after the whole namespace is compiled. If we run our example code, we would get

```shell
$ kapok sieve-of-eratosthenes.kpk
run sieve(1000) return: [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,
                         71,73,79,83,89,97,101,103,107,109,113,127,131,137,
                         139,149,151,157,163,167,173,179,181,191,193,197,199,
                         211,223,227,229,233,239,241,251,257,263,269,271,277,
                         281,283,293,307,311,313,317,331,337,347,349,353,359,
                         367,373,379,383,389,397,401,409,419,421,431,433,439,
                         443,449,457,461,463,467,479,487,491,499,503,509,521,
                         523,541,547,557,563,569,571,577,587,593,599,601,607,
                         613,617,619,631,641,643,647,653,659,661,673,677,683,
                         691,701,709,719,727,733,739,743,751,757,761,769,773,
                         787,797,809,811,821,823,827,829,839,853,857,859,863,
                         877,881,883,887,907,911,919,929,937,941,947,953,967,
                         971,977,983,991,997]
```


#### Fibonacci

Here we see the Fibonacci implemented as a namespace

```clojure
(ns fibonacci
  (use (erlang :only (> - +)))
  (require io))

(defn fibo [0]
  0)

(defn fibo [1]
  1)

(defn fibo [n] (when (> n 0))
  (+ (fibo (- n 1))
     (fibo (- n 2))))

(defn main []
  (let [n 4]
    (io.format #"fibonacci(~B) => ~B~n" [n (fibo n)])))
```
[source file](../example/fibonacci.kpk)

Run it in command line, we get the result as below

```shell
$ kapok fibonacci.kpk
fibonacci(4) => 3
```

