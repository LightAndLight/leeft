{ mkDerivation, array, base, bytestring, directory, fetchgit
, filepath, llvm-hs, llvm-hs-pure, mtl, stdenv, tasty, tasty-golden
, tasty-hspec, tasty-hunit, text, transformers, wl-pprint-text
}:
mkDerivation {
  pname = "llvm-hs-pretty";
  version = "0.2.1.1";
  src = fetchgit {
    url = "https://github.com/lightandlight/llvm-hs-pretty.git";
    sha256 = "1lkgv53d4k1aharf7s8xirwxlh187y0lcwh07yhfjsz5754qy47n";
    rev = "7f28d7d3e81549f44699f2283c5c195f69dbd704";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    array base bytestring llvm-hs-pure text wl-pprint-text
  ];
  testHaskellDepends = [
    base directory filepath llvm-hs llvm-hs-pure mtl tasty tasty-golden
    tasty-hspec tasty-hunit text transformers
  ];
  homepage = "https://github.com/llvm-hs/llvm-hs-pretty";
  description = "A pretty printer for LLVM IR";
  license = stdenv.lib.licenses.mit;
}
