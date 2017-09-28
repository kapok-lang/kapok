Namespace
==========

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
;; call a function true? in namespace core
(core.true? ^nil)
```

The `ns` special form could have `require` and `use` clauses. The `require` clause could have `:as` argument. And the `use` clause could have `:as`, `:only`, `:exclude`, `:rename` arguments. These examples below would show how to use them:

```clojure
(ns example-for-require
  (require io)               ;; require a single erlang module 'io'
  (require (base64 :as bs))  ;; require a single erlang module 'base64' and give it an alias 'bs'
  (require compile           ;; require multiple erlang modules
           (rand :as r)
           re)
  (require atom)       ;; require a kapok standard library namespace
  )
```
  
```clojure
(ns example-for-use
  (use lists)                ;; use a single erlang module 'lists'
  (use (erlang :as er        ;; use a erlang module 'erlang' and give it an alias 'er'
               :only (apply node)   ;; only import apply/2 and node/0 from module 'erlang'
               :rename ((apply ap))  ;; rename 'apply' to 'ap'
               ))
  (use process                ;; use multiple modules/namespaces
       (core :exclude (abs))  ;; :exclude agrument
       gb_sets)
  )
```

`:as`, `:rename` are used to give aliases to modules/namespaces or functions/macros. When there is name clash, or a shorter name is wanted, aliases would be helpful. Since `:only` is inclusive, but `:exclude` is exclusive, they could not be used together in the same use clause.
