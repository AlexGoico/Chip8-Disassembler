Chip8 Disassembler
==================

A minimal disassembler in Haskell for [Chip8](https://en.wikipedia.org/wiki/CHIP-8)

Building
========

Prerequistes
------------

* Ensure you have stack installed on your local system
  * Check haskell.org for instructions for installing stack on your system: https://www.haskell.org/downloads/#stack

Stack Build
-----------

At the root of the project:
```
stack setup
stack build
```

Run with:
```
stack exec c8-disassmebler-exe <your_rom>
```
Where `<your_rom>` is the path to a c8 rom file.

Credits
=======

Small program I worked with [Mark](https://github.com/MarkMcCaskey) over
a summer.
