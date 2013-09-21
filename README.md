hUML
====

A PlantUML-esque clone written in Haskell because I got bored one day and decided it would be fun to create such a tool.

hUML is a toy program, and should be considered permently under construction.
It should also be considerd to be weak in regards to industrial appropriatness.
hUML was designed to explore: a) the use of parser combinators; b) the use of pretty printers; and c) how to do this in Haskell.
Feature-wise PlantUML offers more than hUML; through using PlantUML one can created richer diagrams.
Currently hUML has support for:

1. Specifiying Class Diagrams.
2. Outputing Dot version of the input to STDOUT.

### The Input Language

The LaTeX file contains a copy of the eBNF of the input language.
Using a standard TeXlive installation the typeset version of the grammar can be produced.
The textual language it is a subset of the language provided by PlantUML with several major differences.

1. Attribute and method visibility needs to be explicitly stated.
2. Types must be explicitly stated.
3. Arrows representing relations have a fixed direction

Examples can be found within the examples directory.

### Note

Pull requests will be welcomed, though I do not guarantee that they will be accepted.
Any ideas for features/improvements, or ideas, please use githubs issue tracker.

### The Future

In future I may add the ability to:

+ save the output to file
+ have other output formats i.e. code generation
+ add testing
+ add cabal support
+ 
### What is PlantUML?

PlantUML is a Java program that provides a textual notation and tooling for UML Modelling.
Currently PlantUML supports the creation of the following types of UML Models:

+ sequence diagram
+ use case diagram
+ class diagram
+ activity diagram
+ component diagram
+ state diagram
+ object diagram

Using the PlantUML tool these textual representations can be converted into: ASCII, PNG, and SVG formats, through use of the Dot Tool.
