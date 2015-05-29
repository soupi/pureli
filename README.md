Project-lang
=========

A purely functional, dynamically typed, lisp-like toy programming language interpreter written in Haskell.


Atomic Expressions
------------------

- Integers (`0`, `-1`, `123`)
- Double precision floating points (`0.002`, `-991.12`)
- Booleans (`#t`, `#f`)
- Strings (`"hello, world!"`)
- Nil (`nil`, `()`)

Built-in Procedures
-------------------

- Arithmetic operations (`+`, `-`, `*`, `/`, `mod`)
- Tests (`zero?`, `empty?`, `nil?`, `number?`, `integer?`, `real?`, `list?`. `string?`, `procedure?`)
- Comparison (`==`, `<>`, `>`, `<`, `>=`, `<=`)
- List operations (`list`, `car`, `cdr`, `++`, `slice`, `length`)
- `show` expression
- `if` expression
- `let` and `letrec`
- `quote`, `mquote` and `eval`
- `error`, `try` and `trace`
- `lambda` expression

Built-in IO Actions
-------------------

- `do!` sequence IO actions
- `let!` binds an IO result to a variable
- `read!` reads a line from the standard input
- `read-file!` reads a file
- `print!` writes a line to the standard output
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

- `(define main (print! "Hello, World!"))`


### Macros

It is also possible to define macros using the `defmacro` keyword.

Modules
-------

### Definition

It is possible to define multiple modules per file. In order to run a file, a 'main' module must be present.

Syntax: `(module <name> (<exported definitions>))`

- `(<exported definitions>)` will only export definitions listed. Optional.


### Requires

It is possible to import a source file using the `require` keyword at the top of the file. Cyclic imports are currently not allowed.

Syntax: `(require <filepath> <module name> ?(<imported definitions>) <?new name>)`

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
    [print! "factorial 5 is: "]
    [print! result]))

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

(defmacro bind!
  ((m f)
    (do!
      [let! x (m)]
      [f x])))

(define main
  (do!
    [print! "echo program"]
    [letrec ([go!
      (lambda ()
        (do!
          [bind! (m read!) print!]
          [go!]))])
      (go!)]))
```

### Module 1

```rkt
(module main)

(require "std.pli" std)

(define main
  (do!
    [print! (std/elem 2 (list 1 2 3))])) ; ==> #t
```


### Module 2

```rkt
(module main)

(require "std.pli" std (elem) my-std)

(define main
  (do!
    [print! (my-std/elem 4 (list 1 2 3))])) ; ==> #f

```
