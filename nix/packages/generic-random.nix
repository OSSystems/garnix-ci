{
  mkDerivation,
  base,
  deepseq,
  lib,
  QuickCheck,
}:
mkDerivation {
  pname = "generic-random";
  version = "1.5.0.1";
  sha256 = "dd3451808788d99211edeac27287db5417e97234ce9221a2eb9ab02e9cfc2c0a";
  libraryHaskellDepends = [
    base
    QuickCheck
  ];
  testHaskellDepends = [
    base
    deepseq
    QuickCheck
  ];
  homepage = "http://github.com/lysxia/generic-random";
  description = "Generic random generators for QuickCheck";
  license = lib.licenses.mit;
}
