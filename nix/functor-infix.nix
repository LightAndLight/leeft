{ mkDerivation, base, fetchgit, stdenv, template-haskell }:
mkDerivation {
  pname = "functor-infix";
  version = "0.0.6";
  src = fetchgit {
    url = "https://github.com/lightandlight/functor-infix.git";
    sha256 = "1h0863i865hzcdv44sq302j4z59hryymbawh4y75lbhfzg5byjj4";
    rev = "ffa63000753a939e05fe08b5b7409896a0ca9773";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base template-haskell ];
  homepage = "https://github.com/fmap/functor-infix";
  description = "Infix operators for mapping over compositions of functors. Lots of them.";
  license = stdenv.lib.licenses.mit;
}
