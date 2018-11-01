{ mkDerivation, base, bound, deriving-compat, hedgehog, lambda-expr
, lambda-subst, lens, mtl, stdenv, transformers
}:
mkDerivation {
  pname = "leeft";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bound deriving-compat lambda-expr lambda-subst lens mtl
    transformers
  ];
  testHaskellDepends = [ base bound hedgehog mtl ];
  license = stdenv.lib.licenses.bsd3;
}
