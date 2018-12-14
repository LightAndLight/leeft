{ mkDerivation, array, base, bytestring, containers, deepseq, dlist
, fmlist, HUnit, QuickCheck, random, stdenv, semigroups, text, utf8-string
, vector
}:
mkDerivation {
  pname = "ListLike";
  version = "4.6";
  sha256 = "c1cdec79a5f585a5839eea26a2afe6a37aab5ed2f402a16e7d59fe9a4e925a9a";
  revision = "2";
  editedCabalFile = "1mca2r4gjznqdh4kck5cjkn53isgkhvkf3ri09qsn7nsssvgki0g";
  libraryHaskellDepends = [
    array base bytestring containers deepseq dlist fmlist semigroups text
    utf8-string vector
  ];
  testHaskellDepends = [
    array base bytestring containers dlist fmlist HUnit QuickCheck
    random semigroups text utf8-string vector
  ];
  homepage = "http://github.com/JohnLato/listlike";
  description = "Generic support for list-like structures";
  license = stdenv.lib.licenses.bsd3;
}
