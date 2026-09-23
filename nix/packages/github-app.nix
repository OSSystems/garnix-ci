{
  lib,
  aeson,
  base,
  binary,
  bytestring,
  cryptonite,
  deepseq,
  deepseq-generics,
  fetchgit,
  github,
  hpack,
  http-client,
  http-client-tls,
  http-types,
  jwt,
  mkDerivation,
  mtl,
  safe-exceptions,
  tagged,
  text,
  time,
  x509,
  x509-store,
}:
mkDerivation {
  pname = "github-app";
  version = "0.0.1";
  src = fetchgit {
    url = "https://github.com/garnix-io/github-app";
    sha256 = "15iqkhkdzyglwxh27wlrs29458vvb6l1akm3cpv3c2z81144ax7j";
    rev = "03be93acf111282c442e043e1369a8600df61cbb";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson
    base
    binary
    bytestring
    cryptonite
    deepseq
    deepseq-generics
    github
    http-client
    http-client-tls
    http-types
    jwt
    mtl
    safe-exceptions
    tagged
    text
    time
    x509
    x509-store
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/serokell/github-app#readme";
  description = "Authetnicate as a GitHub App";
  license = lib.licenses.mpl20;
}
