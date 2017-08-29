# BrainFort
Pure Fortran interpreter for BrainFuck

  Currently can interpret simple programs such as the ones provided in this repo.
  Can only interpret .bf files with one line. i.e the program has to be on 1 line only. See example files provided in repo

Included files:
  
  * brainfuck.f90  the interpreter
  * stack.f90      needed by brainfuck.f90 in order to track brackets

  * add.bf       brainfuck program that add 2 numbers
  * fortran.bf   prints out "Fortran\n"
  * helloeasy.bf prints out "Hello world!\n "
  * quine.bf     prints out the contents of its self. i.e its a [quine](https://en.wikipedia.org/wiki/Quine_(computing))

TODO:
    
  - [ ] fix small bugs that don't allow hellhello.bf to run correctly
  - [x] Allow multi-line .bf files
