{ mkDerivation, base, fetchgit, lens, stdenv }:
mkDerivation {
  pname = "subst";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/lightandlight/subst";
    sha256 = "08dz6gp032is2riffmwx1lrz1d0wsn2jcarfhmjk5kg8p1g4lm2c";
    rev = "d3d42f0f3aba29c9c8e37332242e1a30caecc747";
  };
  postUnpack = "sourceRoot+=/subst; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base lens ];
  description = "De Bruijn indices using Backpack-y Prisms and Plated";
  license = stdenv.lib.licenses.bsd3;
}
