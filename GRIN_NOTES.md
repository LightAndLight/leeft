the entrypoint to the program is called 'grinMain' - can we make this configurable?

primops are desugared from special strings, located here
https://github.com/grin-tech/grin/blob/master/grin/src/Reducer/LLVM/PrimOps.hs
- I think it would be nice if those primops were a sum type and were exposed to
  the user through the grin AST

Why is Program a constructor for Exp? It should be its own type. Those
datatypes should mimic the abstract syntax from the paper.

Variables in your language need to be fetched from
