This is the code accompanying my blog post: [Generating Mazes with Inductive Graphs](http://jelv.is/blog/Generating-Mazes-with-Inductive-Graphs).

Build instructions courtesy of [Mike Xie](https://github.com/Mike-Xie) and [Rianna Morgan](https://github.com/R-Morgan)

### Libcairo

libcairo is a C++ library, their site and instructions on how to install by OS can be found here: http://cairographics.org/download/

On Ubuntu, you can install it with `apt-get`:

    sudo apt-get install libcairo2-dev

### Installation with Cabal

Get the project:

    git clone https://github.com/TikhonJelvis/inductive-mazes.git
    cd inductive-mazes

If your cabal bin directory is not in your `PATH`, add it:

    export PATH=$HOME/.cabal/bin:$PATH  – or wherever your cabal directory is

The project should probably be built inside of a sandbox.

    cabal sandbox init

    cabal install gtk2hs-buildtools
    cabal install inductive-mazes.cabal

### Installation with Stack

Building under [Stack](http://docs.haskellstack.org/en/stable/README/) is quite
simple and obviates the need for building within a cabal sandbox.

Get the project:

    git clone https://github.com/TikhonJelvis/inductive-mazes.git
    cd inductive-mazes

From within the project directory, run:

    stack init

Stack will then install the packages locally for the project.

### Interactive Use

Run the following inside the `inductive-mazes` directory:

    cabal repl

If using `stack`, run:

    stack ghci

Once in the `ghci` instance, run:

    *Draw> genPng defaults "my-maze.png" 40 40
    ^D – exiting cabal repl

After having returned to the terminal, view the new maze file with your
favourite `.png` views:

    eog my-maze.png -- or whatever your png viewer is
