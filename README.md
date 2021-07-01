# Artifact README

This artifact is being submitted to support the paper "Statically Bounded-Memory Delayed Sampling for Probabilistic Streams" by Atkinson, Baudart, Mandel, Yuan, and Carbin. The artifact contains, in addition to this document, one file `Debian.ova` which is a virtual machine containing source code, pre-built binaries, and benchmark programs.

## Getting Started

First, import `Debian.ova` into your virtualization software. In our testing, we used [VirtualBox](https://www.virtualbox.org) 6.1.22 on macOS Big Sur. This VM is packaged in the Open Virtual Appliance format and can be imported into VirtualBox through `File -> Import Appliance`. The VM contains an installation of Debian Linux and has no particular hardware or network requirements.

Once the VM boots, it should present a shell as the root user with no password necessary (the root password is `root` in case it is ever required). Change into the `probzelus-analysis-impl-master` directory to access the main artifact files.

In this directory, type `make install` to rebuild the artifact from scratch.
You should now have the `mufc` compiler in your path.

```
$ mufc --help
Options are:
  --only-check        Only run the static analysis
  --simulate <node>   Simulates the node <node> and generates a file <node>.ml
  -help               Display this list of options
  --help              Display this list of options
```

The `tests` directory contains a set of `.muf` programs corresponding to the benchmarks in the paper. Execute the artifact on each program by supplying it as an argument, for example:

```shell
$ cd tests
$ mufc outlier.muf
```
Which will display the results of the static analysis, compile the code to OCaml and compile the OCaml code to build an executable that runs the program.

```shell
$ mufc outlier.muf 
-- Analyzing outlier.muf
  Checking node outlier:
    m-consumed analysis: success
    Unseparated paths analysis: success
  Checking node main:
    m-consumed analysis: failed
    Unseparated paths analysis: success
-- Generating outlier.ml
-- Generating main.ml
ocamlfind ocamlc -linkpkg -package muf outlier.ml main.ml -o outlier_main.exe
```

You can now execute the program:

```shell
$ ./outlier_main.exe
gaussian (0.990099, 0.990099)
gaussian (0.996689, 0.665563)
gaussian (0.998758, 0.624845)
...
```

Note that stream processors never stops. Use `^C` to stop the execution.

You can also run `make tests` from the root of the project to compile all the examples.

## Step by Step Instructions

### Reproducing Paper Benchmarks

To execute the benchmarks found in the paper, run the analysis on the following files:

```text
| Paper Benchmark      | Filename              |
| -------------------- | --------------------- |
| Kalman               | kalman_normal.muf     |
| Kalman Hold-First    | kalman_first.muf      |
| Gaussian Random Walk | kalman_generative.muf |
| Robot                | robot.muf             |
| Coin                 | coin.muf              |
| Gaussian-Gaussian    | gaussian_gaussian.muf |
| Outlier              | outlier.muf           |
| MTT                  | mtt.muf               |
| SLAM                 | slam.muf              |
```

The remaining benchmarks in the `tests/` directory can also be executed but are not described in the paper.

Each benchmark should execute and immediately display the outcome of the analysis. If the analysis fails for m-consumed or unseparated paths, an error message will be displayed for each sub-analysis.

### Writing uF programs

The best way to write a new uF test program is to follow the syntax of the examples. Every uF program is a series of `stream` declarations, with the final declaration being the entry point of the program. For example, in `tests/robot.muf`, there are three declarations `kalman`, `controller`, and `robot`, with `robot` being the entry point.

Every stream declaration is of the form

```text
val <stream-name> = stream {
    init = <init-val>;
    step (<state-pat>, <arg-pat>) =
        <step-exp>
}
```

where `stream-name` is the identifier for the stream, `init-val` is the initial value of the stream, and `step-exp` is the expression evaluated at every iteration. `state-pat` is a pattern that binds the input state of the step function and `arg-pat` is a pattern that binds the argument being supplied to the stream. Note that `init-val` must be a syntactic value (such as a numeric literal) or the instantiation of a stream function (`init` or `infer`) and that `state-pat` should be compatible with the type of the initial value.

The step function accepts the current state and a supplied argument to compute a pair of (new state, output value). Inside the step function, the following core constructs can be used:

- `sample`, which accepts a distribution and yields a sample from that distribution
- `observe`, which accepts a (distribution, value) pair and observes that distribution to be that value.
- `unfold`, which accepts a (stream, value) pair and yields a pair (output, new stream).

If the step function uses `sample` or `observe` it is considered a probabilistic stream.

Furthermore, these core constructs may also be used inside the initial value:

- `init`, which accepts the name of a stream declaration and creates an instance of that non-probabilistic stream.
- `infer`, which accepts a (number of particles, name of stream declaration) pair and creates an instance of that probabilistic stream.

There are a number of built-in distributions and operators such as `gaussian`. Please examine the supplied programs to see examples of these built-in operators.

### Claims Supported by Artifact

The artifact supports all claims in the Evaluation section of the paper. In particular, executing the analysis on the benchmark programs produces the results in Table 1 of the paper indicating whether each program satisfies the m-consumed and unseparated paths properties. In the artifact, the iteration bound for the unseparated paths analysis is set to 10 iterations, which is the same as in the paper. The implementation of the type system presented in the paper is available in the file `src/compiler/analysis.ml`.
