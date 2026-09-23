{
  lib,
  aeson,
  base,
  base16-bytestring,
  bytestring,
  cryptonite,
  fetchgit,
  github,
  github-webhooks,
  http-types,
  memory,
  mkDerivation,
  servant,
  servant-server,
  string-conversions,
  text,
  transformers,
  unordered-containers,
  wai,
  warp,
}:
mkDerivation {
  pname = "servant-github-webhook";
  version = "0.4.2.0";
  src = fetchgit {
    url = "https://github.com/tsani/servant-github-webhook";
    sha256 = "1adzyicl8xrxxa0pbxz9d0pvb5pxbqnh8dwlrlgg3gdcjgr71983";
    rev = "4160919432643e7f891dccf1ce5599264fe32795";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson
    base
    base16-bytestring
    bytestring
    cryptonite
    github
    github-webhooks
    http-types
    memory
    servant
    servant-server
    string-conversions
    text
    transformers
    unordered-containers
    wai
  ];
  testHaskellDepends = [
    aeson
    base
    bytestring
    servant-server
    text
    transformers
    wai
    warp
  ];
  homepage = "https://github.com/tsani/servant-github-webhook";
  description = "Servant combinators to facilitate writing GitHub webhooks";
  license = lib.licenses.mit;
}
