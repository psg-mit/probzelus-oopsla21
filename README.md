# Artifact README

This artifact is being submitted to support the paper "Statically Bounded-Memory Delayed Sampling for Probabilistic Streams" by Atkinson, Baudart, Mandel, Yuan, and Carbin. The artifact contains, in addition to this document, one file `Debian.ova` which is a virtual machine containing source code, pre-built binaries, and benchmark programs.

## Getting Started

First, import `Debian.ova` into your virtualization software. In our testing, we used [VirtualBox](https://www.virtualbox.org) 6.1.22 on macOS Big Sur. This VM is packaged in the Open Virtual Appliance format and can be imported into VirtualBox through `File -> Import Appliance`. The VM contains an installation of Debian Linux and has no particular hardware or network requirements.

Once the VM boots, it should present a shell as the root user with no password necessary (the root password is `root` in case it is ever required). Change into the `probzelus-analysis-impl-master` directory to access the main artifact files.

The artifact is in the `artifact` directory.
The `mufc` compiler is already installed.

```
$ cd artifact
$ mufc --help
The muF Compiler. Options are:
  --only-check        Only run the static analysis (default false)
  --simulate <node>   Simulates the node <node> and generates a file <node>.ml (default main)
  --up-bound <int>    iteration bound for the unseparated paths analysis (default 10)
  -help               Display this list of options
  --help              Display this list of options
```

The `tests` directory contains a set of `.muf` programs corresponding to the benchmarks in the paper. Execute the artifact on each program by supplying it as an argument, for example:

```shell
$ cd tests
$ mufc outlier.muf
```
Which 
1. Display the results of the static analysis, 
2. compile the muF program to OCaml (`outlier.ml`) 
3. generate a simple simulation OCaml program (`main.ml`)
4. build an executable that runs the program (`outlier_main.exe`)

```shell
$ mufc outlier.muf 
-- Analyzing outlier.muf
     x m-consumed analysis failure
     o Unseparated paths analysis success
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

:warning: Note that stream processors never stops. Use `^C` to stop the execution.

You can also run `make tests` from the root of the project to compile all the examples.

Option `--only-check` only runs the analysis (without compilation).
Run `make bench` to run the analysis on all the benchmarks presented in the paper (see below).


Note that the following programs are not able to compile, and must be passed to `mufc` with the `--only-check` option: `mtt.muf`, `robot.muf`, `slam_array.muf`, `slam_tuple.muf`, and `slam.muf`. This is expected behavior. The `mtt.muf` example requires a list library that is not yet implemented in the runtime; the `robot.muf` example requires a stream of observation inputs that each depend on program outputs, and we have not yet ported the code to generate these inputs; and the `slam` examples require array libraries that are not yet linked to the runtime. Note that the missing dependencies do not impact this artifact's support of the claims made the paper, as the analysis nevertheless runs on all of these examples.

## Relationship with the paper

### Section 2

The muF program of Figure 1 is available in `robot.muf`.
The stream function `controller` uses a `lqr` function that acts as a place-holder for a Linear–quadratic regulator.
The implementation of `lqr` makes no difference for the analysis but requires matrix operations that are not yet supported.

### Section 4

Compared to the syntax given in the paper, in the examples, our implementation requires the programmer to explicitly build symbolic term values as described in Section 4.1 using special constructs such as `const` (constants), and `pair` (tuple).
Concrete values can be obtained from symbolic values with the `eval` function (e.g., for the condition of a `if` statement).

- `x = sample (bernoulli (0.5))` should be written `x = sample (bernoulli (const (0.5))` (see coin_outlier.muf).

- `x = sample (gaussian (0., 1.))` should be written `x = sample (gaussian (const (0.), 1.)` (see gaussian_gaussian.muf). Note that the `gaussian` construct uses a symbolic value for the mean but a concrete value for the variance.

- If `x` is a random variable (defined with `x = sample ...`), `if x then ...` should thus be written `if eval (x) then ...` (see `coin_outlier.muf`).

The muF compiler is a prototype focusing on the static analysis presented in the paper.
These discrepancies could be addressed with a simple compilation pass that we leave for future work.

### Section 7

To test the benchmarks presented in Table 1, run the analysis on the following files, or simply run `make bench`.

| Paper Benchmark      | Filename              |  m-consumed | Unseparated paths |
| -------------------- | --------------------- | ----------- | ----------------- |
| Kalman               | kalman_normal.muf     | o           | o                 |
| Kalman Hold-First    | kalman_first.muf      | o           | x                 |
| Gaussian Random Walk | kalman_generative.muf | x           | o                 |
| Robot                | robot.muf             | o           | o                 |
| Coin                 | coin.muf              | o           | o                 |
| Gaussian-Gaussian    | gaussian_gaussian.muf | o           | o                 |
| Outlier              | outlier.muf           | x           | o                 |
| MTT                  | mtt.muf               | x           | o                 |
| SLAM                 | slam_array.muf        | x           | o                 |

The remaining benchmarks in the `tests/` directory are valid programs but are not described in the paper.

For each benchmark, the compiler returns the outcome of two analyses: m-consumed and unseparated paths analysis.
For this experiment, the `--only-check` option is set to true. The compiler won't try to generate an executable.
To execute an example, you need to first compile it, e.g.,:

```
$ mufc kalman_normal.muf
$ ./kalman_normal_main.exe
```

## Step by Step Instructions

### Installation 

The muF runtime depends on [ProbZelus](https://github.com/ibm/probzelus).
Once ProbZelus is installed, in the `artifact` directory, type `make init` to install the artifact from scratch.

You should now have the `mufc` compiler in your path.
Run the following command to test your installation:
```
$ mufc --help
```

### Writing muF programs

The best way to write a new muF test program is to follow the syntax of the examples. Every muF program is a series of `stream` declarations, with the final declaration being the entry point of the program. For example, in `tests/robot.muf`, there are three declarations `kalman`, `controller`, and `robot`, with `robot` being the entry point.

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

As mentioned in Section _"Relationship with the paper, Section 4"_ (see above), distribution parameters can be symbolic values that must be built using `const` (constant) and `pair` (tuple).
Evaluation of a delayed sampling expression can be forced with the function `eval` to obtain a concrete value.

### Claims Supported by Artifact

The artifact supports all claims in the Evaluation section of the paper. In particular, executing the analysis on the benchmark programs produces the results in Table 1 of the paper indicating whether each program satisfies the m-consumed and unseparated paths properties.
The implementation of the type system presented in the paper is available in the file `src/compiler/analysis.ml`.
