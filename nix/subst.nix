{ mkDerivation, base, fetchgit, lens, stdenv }:
mkDerivation {
  pname = "subst";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lightandlight/subst";
    sha256 = "0rk4k9h0l045v6ba9ar8bs9yh8s6bnchqgzifsblh20p17qiv9if";
    rev = "10445211a173633955a1498df4a2e010e228fca8";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base lens ];
  description = "De Bruijn indices using Backpack-y Prisms and Plated";
  license = stdenv.lib.licenses.bsd3;
}
