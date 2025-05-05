# Probably
The official interpreter for the Probably language.

> [!WARNING]
> This software is unfinished, and still under development. Keep your expectations low.

Probably is a functional probabilistic programming language implemented in Haskell.
It supports both sampling-based evaluation and exact inference of probabilistic
programs, called *experiments*.
It is a small language expressive enough to model arithmetic expressions with
random variables and conditionals.

## Overview

This project provides two execution semantics for the language:

- Sampling-based evaluation, implemented in `src/Interpreter.hs`. It conducts an experiment by evaluating experiments using a pseudo-random source of randomness.
- Exact inference, implemented in `src/Probability.hs`, which computes a distribution over all possible outcomes.

## Features

- Numeric and Boolean types
- Arithmetic (`+`) and logical (`<=`, `<`, `not`) operations
- Conditional expressions (`if ... then ... else ...`)
- Stochastic let-bindings over uniform distributions
- Finite occurrence types

## Usage

The project includes a command-line interface via `Main.hs`. Compile it using `ghc` or run it directly with `runghc`.

    probably --help                     # Show usage instructions
    probably --parse     program.prob   # Parse only
    probably --typecheck program.prob   # Parse and typecheck
    probably --show      program.prob   # Infer and output the probability distribution
    probably --equals 5  program.prob   # Compute P(result == 5)
    probably --less   7  program.prob   # Compute P(result < 7)
    probably             program.prob   # Evaluate using sampling

## Example Program

    let x ~ uniform Int 1 6 in
    let y ~ uniform Int 1 6 in
    x + y

This models rolling two dice and sums them.

## Building and Testing

You can use `stack` to build, run experiments and tests

To build:

    stack build

To run:

    stack run -- [<flag>] <program-name>

To clean up after stack:

    # Clean up most things
    stack clean

    # Clean up everything
    stack purge

We do property based testing with `QuickCheck` and some simple regression tests of our test programs

To run tests:

    stack test

To enable verbose test output:

    stack test --test-arguments="--verbose"

## Properties Checked

- All inferred distributions sum to a total probability of 1
- No outcomes fall outside their declared type
- No negative probabilities are produced

## Directory Structure

The implementation can be found in these files

    .
    ├── app
    │   └── Main.hs          -- The command line interface
    ├── src
    │   ├── Interpreter.hs   -- Evaluator for conductng experiments
    │   ├── Parser.hs        -- Language parser implemented using Parsec
    │   ├── Print.hs         -- Pretty-printers for AST and typed programs
    │   ├── Probability.hs   -- Evaluator for exact inference of experiments
    │   ├── Syntax.hs        -- The core language definition
    │   └── Typechecker.hs   -- Type inference and checking
    └── test
        └── Spec.hs          -- Property-based tests and unit tests


Experiments noted in Listings and Figures can be found in this directory

    .
    ├── example-experiments
        ├── bigRange.prob
        ├── eulerProblem205.prob
        ├── simple.prob
        ├── slotMachine.prob
        ├── throw3dice.prob
        └── throw5dice.prob
        └── ....


## License

This project is released under the GPL-3.0 License.
