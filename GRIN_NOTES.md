the entrypoint to the program is called 'grinMain' - can we make this configurable?

primops are desugared from special strings, located here
https://github.com/grin-tech/grin/blob/master/grin/src/Reducer/LLVM/PrimOps.hs
- I think it would be nice if those primops were a sum type and were exposed to
  the user through the grin AST
