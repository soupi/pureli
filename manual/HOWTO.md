How To Install
--------------

From source:

Install GHC 7.8.* and cabal and run the following commands:

```sh
git clone https://github.com/soupi/pureli
cd pureli
cabal sandbox init
cabal install
```

How To Run
----------

### Execution

To run the pureli REPL, write:
```sh
pureli
```

To run a pureli program, write:
```sh
pureli <filepath>
```

### Parallelism

To run with parallel execution, add `+RTS -N<n>` to the end of an execution command, where <n> is the number of core you would like to use

Adding a `-s` flag will print run time information at the end of the execution.

### example:

```sh
pureli examples/exponent.pli +RTS -N4 -s
```

Or

```sh
pureli examples/exponent.pli +RTS -N2
```
