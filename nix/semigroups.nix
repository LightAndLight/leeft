{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "semigroups";
  version = "0.18.5";
  sha256 = "ab2a96af6e81e31b909c37ba65f436f1493dbf387cfe0de10b6586270c4ce29d";
  libraryHaskellDepends = [ base ];
  homepage = "http://github.com/ekmett/semigroups/";
  description = "Anything that associates";
  license = stdenv.lib.licenses.bsd3;
}
