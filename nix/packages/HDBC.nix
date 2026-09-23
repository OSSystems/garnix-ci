{
  lib,
  base,
  bytestring,
  containers,
  convertible,
  mkDerivation,
  mtl,
  old-time,
  text,
  time,
  utf8-string,
}:
mkDerivation {
  pname = "HDBC";
  version = "2.4.0.4";
  sha256 = "c93f2d90e1a73be53cab3cfe27352c24383f1eaecd14720d08769799b93690ca";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    bytestring
    containers
    convertible
    mtl
    old-time
    text
    time
    utf8-string
  ];
  homepage = "https://github.com/hdbc/hdbc";
  description = "Haskell Database Connectivity";
  license = lib.licenses.bsd3;
}
