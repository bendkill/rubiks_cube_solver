# Functional Rubik's Solver

![Now in technicolor.](https://raw.githubusercontent.com/benjamindkilleen/functional-rubiks-solver/master/images/cube.png)

A shell-based Rubik's cube solver written in Haskell, providing instructions for the White Cross
algorithm. It uses a state Monad. This is possibly useful for educational purposes but probably not
for [actually solving a Rubik's cube](https://arxiv.org/abs/1910.07113) (which I don't
actually know how to do).

## Installation

After installing Haskell (e.g. from [here](https://www.haskell.org/platform/)), install dependencies:
```bash
cabal update
cabal install --lib random mtl
```
clone this
repository and build from source:

```bash
ghc -o solver *.hs
```

## Usage

To start the solver, run

```bash
./solver
```

The solver will ask whether the user wants to solve a random cube, which is less useful than
entering in the location of all the squares, but much faster. The instructions will be given in
standard cube notation, color-coded:

![Some assembly required.](https://raw.githubusercontent.com/benjamindkilleen/functional-rubiks-solver/master/images/instructions.png)

## About

I wrote this solver for fun after thoroughly enjoying [Stuart
Kurtz](https://computerscience.uchicago.edu/people/profile/stuart-kurtz/)'s intro to computer
science. Feel free to modify if at all useful.
