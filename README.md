# mips-hike
 Virtual machine of a self-designed MIPS-like instruction set, written in Haskell. A toy compiler targeting the MIPS-like assemble language is also included.

 This is my course project of Computer Organzation. This project includes:
 - the design of a MIPS-like instruction set, named as MipsLike
 - a Haskell implementation of the virtual machine of MipsLike, named as Mips*H*ike
 - a toy high-level language, named as CLike, since most of its grammar is borrowed from C
 - a simple Haskell implementation of a CLike-to-MipsLike compiler, named as C*H*ike

## Build

The project can be built with `nix`.

```
nix-shell shell.nix --run "cabal configure; cabal build"
```
will use `nix` to take care of all the dependencies and build the project.
