Group D1-3
==========
Software Specification &amp; Testing

Directory Structure:
====================
* Libraries from the book are placed in the 'lib' directory.
* Every chapter has its own folder.
* Inside every chapter folder, there are individual folders for each team member (e.g. ammar and mehmet folder). This is used sandbox before actually putting the final solution (e.g. chapter-1/Sol1.hs).

How to compile:
===============
* Go to the root directory of this project
* Enter ghc interactive mode and include lib directory to import libraries from the book:
```haskell
ghci -ilib/
```
* Compile the file:
```haskell
:l chapter-{X}/Sol{X}.hs
```

