Write yourself a Brainfuck in an hour
==========================

Following a [basic tutorial](https://github.com/quchen/articles/blob/master/write_yourself_a_brainfuck.md) on how to create a fully featured Brainfuck interpreter in Haskell

Usage:

    cabal sandbox init
    cabal install --dependencies-only
    cabal build
    cabal run

Currently the helloworld.bf is hardcoded into main. The first step to improve
this interpreter is to allow any input file via command line argument.

I have followed the tutorial and did a few exercises (but far from finishing
them all). According to the exercises a lot of code can be refactored and made a
bit nicer than what it looks right now.
