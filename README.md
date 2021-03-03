# Functional Rubik's Solver

A shell-based Rubik's cube solver written in Haskell, providing instructions for the White Cross
algorithm. It uses a state Monad. This is possibly useful for educational purposes but probably not
for [actually solving a Rubik's cube](https://openai.com/blog/solving-rubiks-cube/) (which I don't
actually know how to do).

# Installation

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

# Usage

To start the solver, run

```bash
./solver
```

The solver will ask whether the user wants to solve a random cube, which is less useful than
entering in the location of all the squares, but much faster. The instructions will be given in
standard cube notation, color-coded:



