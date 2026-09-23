{
  mkDerivation,
  base,
  fetchgit,
  lib,
  mtl,
  profunctors,
}:
mkDerivation {
  pname = "iso-deriving";
  version = "0.0.8";
  src = fetchgit {
    url = "https://github.com/hanshoglund/iso-deriving";
    sha256 = "sha256-nST6yuzZTiVbw9IrYbuL2SIZPXZrhj4sbUwL3NMDJLo=";
    rev = "4230bbff15611690c8b004d34ddd438a14efd02d";
  };
  libraryHaskellDepends = [
    base
    mtl
    profunctors
  ];
  testHaskellDepends = [
    base
    mtl
  ];
  description = "Deriving via arbitrary isomorphisms";
  license = lib.licenses.mit;
}
