This is the code accompanying my blog post: [Generating Mazes with Inductive Graphs](http://jelv.is/blog/Generating-Mazes-with-Inductive-Graphs).

Build instructions courtesy of Mike Xie, tested on Ubuntu 14.10:

### Libcairo

libcairo is a C++ library, their site and instructions on how to install by OS can be found here: http://cairographics.org/download/

On Ubuntu, you can install it with `apt-get`:

    sudo apt-get install libcairo2-dev

### Installation

Get the project:

    git clone https://github.com/TikhonJelvis/inductive-mazes.git
    cd inductive-mazes

If your cabal bin directory is not in your `PATH`, add it:

    export PATH=$HOME/.cabal/bin:$PATH  – or wherever your cabal directory is

The project should probably be built inside of a sandbox.

    cabal sandbox init

    cabal install gtk2hs-buildtools
    cabal install inductive-mazes.cabal

### Interactive Use

Run the following inside the `inductive-mazes` directory:

    cabal repl

    *Draw> genPng defaults "my-maze.png" 40 40
    ^D – exiting cabal repl

    eog my-maze.png -- or whatever your png viewer is
