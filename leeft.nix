{ mkDerivation, base, hspec, lens, mtl, stdenv, subst, transformers
}:
mkDerivation {
  pname = "leeft";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base lens mtl subst transformers ];
  testHaskellDepends = [ base hspec ];
  doHaddock = false;
  license = stdenv.lib.licenses.bsd3;
}
