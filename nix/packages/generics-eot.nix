{
  lib,
  QuickCheck,
  base,
  directory,
  doctest,
  fetchgit,
  filepath,
  hspec,
  interpolate,
  markdown-unlit,
  mkDerivation,
  mockery,
  shake,
  silently,
}:
mkDerivation {
  pname = "generics-eot";
  version = "0.0.8";
  src = fetchgit {
    url = "https://github.com/garnix-io/generics-eot";
    sha256 = "sha256-rv/VJv6uOBVeUVevysibnssyFCMMFBScA0br1bY+ljw=";
    rev = "d702b1ea6ff503e015be9b90e2940b61edcfa0b4";
  };
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base
    directory
    doctest
    filepath
    hspec
    interpolate
    markdown-unlit
    mockery
    QuickCheck
    shake
    silently
  ];
  testToolDepends = [ markdown-unlit ];
  homepage = "https://generics-eot.readthedocs.io/";
  description = "A library for generic programming that aims to be easy to understand";
  license = lib.licenses.bsd3;
}
