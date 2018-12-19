{ mkDerivation, ansi-wl-pprint, base, bound, deriving-compat, grin, hedgehog
, lens, mtl, stdenv, recursion-schemes, transformers, llc-7
}:
mkDerivation {
  pname = "leeft";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-wl-pprint base bound deriving-compat grin lens mtl
    recursion-schemes transformers
  ];
  testHaskellDepends = [ base bound hedgehog mtl ];
  buildTools = [ llc-7 ];
  license = stdenv.lib.licenses.bsd3;
}
