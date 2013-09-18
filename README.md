Group D1-3
==========
Software Specification &amp; Testing

Directory Structure:
====================
* Libraries from the book are placed in the 'lib' directory.
* Every week has its own folder.
* Inside every week folder, there are individual folders for each team member (e.g. ammar, mehmet and stevan folder). This is used sandbox before actually putting the final solution (e.g. week-1/{name}.hs).

How to compile:
===============
* Go to the root directory of this project
* Enter ghc interactive mode and include lib directory to import libraries from the book:
```
ghci -ilib/
```
* Compile the file:
```
:l week-nr/{x}.hs
```

