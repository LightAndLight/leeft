{ mkDerivation, base, binary, bytestring, deepseq, ghc-prim
, hashable, quickcheck-instances, stdenv, tasty, tasty-hunit
, tasty-quickcheck, text
}:
mkDerivation {
  pname = "text-short";
  version = "0.1.2";
  sha256 = "b3f2b867d14c7c2586ea580028606b6662293ad080726d5241def937e5e31167";
  revision = "1";
  editedCabalFile = "00w77idkh44m88vivkqsys0y1bbxrflh06yq66liq0wgjhhzdppj";
  libraryHaskellDepends = [
    base binary bytestring deepseq ghc-prim hashable text
  ];
  testHaskellDepends = [
    base binary quickcheck-instances tasty tasty-hunit tasty-quickcheck
    text
  ];
  description = "Memory-efficient representation of Unicode text strings";
  license = stdenv.lib.licenses.bsd3;
}
