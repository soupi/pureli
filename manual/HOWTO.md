How To Run
----------


## Execution

To run the pureli REPL, write:
```sh
pureli
```

To run a pureli program, write:
```sh
pureli <filepath>
```

## Parallelism

To run with parallel execution, add `+RTS -N<n>` to the end of an execution command, where <n> is the number of core you would like to use

Adding a `-s` flag will print run time information at the end of the execution.

### example:

```sh
pureli examples/parallel/exponent.pli +RTS -N4 -s
```

Or

```sh
pureli examples/parallel/exponent.pli +RTS -N2
```
