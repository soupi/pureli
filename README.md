Pureli
======

![(Pureli)](manual/assets/pureli-light-small-transpar.png)

A purely functional, dynamically typed, parallel evaluated Lisp-like programming language interpreter written in Haskell.

[Website](http://soupi.github.io/pureli)

> **Status**: Prototype


Installation
------------

From source:

Install GHC 7.10.* and cabal and run the following commands:

```sh
git clone https://github.com/soupi/pureli
cd pureli
cabal sandbox init
cabal install
```

HOWTO
-----

Read [The Manual](http://soupi.github.io/pureli/manual.html) for information about pureli and how to write pureli programs.

Atomic Expressions
------------------

- Integers (`0`, `-1`, `123`)
- Double precision floating points (`0.002`, `-991.12`)
- Booleans (`#t`, `#f`)
- Strings (`"hello, world!"`)
- Keywords (`:hello`, `:world`)
- Nil (`nil`, `()`)

Built-in Procedures
-------------------

- Arithmetic operations (`+`, `-`, `*`, `/`, `mod`)
- Tests (`zero?`, `empty?`, `nil?`, `number?`, `integer?`, `real?`, `list?`. `string?`, `procedure?`, `symbol?`, `keyword?`)
- Comparison (`=`, `<>`, `>`, `<`, `>=`, `<=`)
- List operations (`list`, `car`, `cdr`)
- List and String operations (`++`, `slice`, `length`)
- String operations (`str->lines`, `str->words`, `lines->str`, `words->str`, `to-upper`, `to-lower`)
- `round` operation on reals
- `show` expression
- `if` expression
- `let` and `letrec`
- `quote`, `eval` and `read-str`
- `error`, `try` and `trace`
- `lambda` expression

Built-in IO Actions
-------------------

- `do!` sequence IO actions
- `let!` binds an IO result to a variable
- `read!` reads a line from the standard input
- `read-file!` reads a file
- `print!` writes to the standard output without newline
- `println!` writes to the standard output with newline
- `print-file!` writes a string to a file
- `pure` raises a pure computation into IO context


Definitions
-----------

Top level functions and values are defined using the keyword `define`.

For example:

- `(define id (x) x)`
- `(define message "Hello, World!")`


A program must have the top level value `main` - an IO action which is the entry point.

For example:

- `(define main (println! "Hello, World!"))`


### Unevaluated parameters

It is also possible to define functions with receives unevaluated parameters

```rkt

(module main)


(define unless (test ~true-branch ~false-branch)
  (if
    test
    false-branch
    true-branch))


(define main
  (println!
    (unless
      #t
      (error "not thrown")
      (+ 1 1)))) ;; => 2

```

Modules
-------

### Definition

It is possible to define multiple modules per file. In order to run a file, a 'main' module must be present.

Syntax: `(module <name> ?(<exported definitions>))`

- `(<exported definitions>)` will only export definitions listed. Optional.



### Requires

It is possible to import a source file using the `require` keyword at the top of the module. Cyclic imports are currently not allowed.

Syntax: `(require <filepath> <module name> ?(<imported definitions>) <?new name>)`

Or when required module is in the same file: `(module <name>)`

- `(<imported definitions>)` will only imported definitions listed. Optional.
- `<new name>` will give the module a new name. Optional.
- A definition from a module can be accessed using `/`.


Examples
--------

### Factorial

```rkt
;The program will calculate the factorial of 5 and will print it to screen
(module main)

(define main
  (do!
    [let! result (pure (factorial 5))]
    [println! "factorial 5 is: "]
    [println! result]))

(define factorial [n]
  (letrec ((go (lambda (n prod)
    (if (zero? n)
      prod
      (go (- n 1) (* prod n))))))
    (go n 1)))

```

### Echo

```rkt
;The program will repeat anything the user writes until ^C
(module main)

(module "stdlib/std.pli" io (bind!))

(define main
  (do!
    [println! "echo program"]
    [letrec ([go!
      (lambda ()
        (do!
          [io/bind! (read!) println!]
          [go!]))])
      (go!)]))
```

### Module 1

```rkt
(module main)

(require "stdlib/std.pli" list)

(define main
  (do!
    [println! (list/elem 2 (list 1 2 3))])) ; ==> #t
```


### Module 2

```rkt
(module main)

(require "stdlib/std.pli" list (elem) my-std)

(define main
  (do!
    [println! (my-std/elem 4 (list 1 2 3))])) ; ==> #f

```

### A Pureli REPL
```rkt
(module main)

(define main
  (do!
    (println! "Pureli REPL made in Pureli")
    (repl)))

(define repl ()
  (do!
    (let! input (read!))
    (print! "=> ")
    (let! result
          (try (pure (eval (read-str input)))
               (eval (read-str input))
               (pure "Error in expression")))
    (if
      (nil? result)
      (pure nil)
      (println! result))
    (repl)))
```

More examples at `examples/` and `tests/`.
