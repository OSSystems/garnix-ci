{
  mkDerivation,
  aeson,
  base,
  base58-bytestring,
  binary,
  bytestring,
  conduit,
  crypto-api,
  cryptohash-sha256,
  entropy,
  fetchgit,
  http-conduit,
  lib,
  text,
  time,
  unordered-containers,
}:
mkDerivation {
  pname = "oauth2-simple";
  version = "0.1.1.0";
  src = fetchgit {
    url = "https://github.com/garnix-io/oauth2-simple";
    sha256 = "0qdw9qayvi61zdd20x6gmrssn02gb8pa3vjmdi5hikplaigii7w0";
    rev = "aec91d5397e414510cbb37372173af0d84e300b7";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    base58-bytestring
    binary
    bytestring
    conduit
    crypto-api
    cryptohash-sha256
    entropy
    http-conduit
    text
    time
    unordered-containers
  ];
  description = "A simple OAuth2 library";
  license = lib.licenses.bsd3;
}
