# We use our fork until https://github.com/cuedo/github-webhooks/pull/92 has
# been merged.
{
  mkDerivation,
  aeson,
  base,
  base16-bytestring,
  bytestring,
  cryptonite,
  deepseq,
  deepseq-generics,
  hspec,
  memory,
  text,
  time,
  vector,
  fetchgit,
  lib,
}:
mkDerivation {
  pname = "github-webhooks";
  version = "0.18.0";
  src = fetchgit {
    url = "https://github.com/garnix-io/github-webhooks";
    sha256 = "sha256-ntEOKrruC/31tz5clyeycDOc6xd/gQYvjA/dOFGAbOw=";
    rev = "cded146c20abffa9d4f1976d58a66eb805d18dfe";
  };
  libraryHaskellDepends = [
    aeson
    base
    base16-bytestring
    bytestring
    cryptonite
    deepseq
    deepseq-generics
    memory
    text
    time
    vector
  ];
  testHaskellDepends = [
    aeson
    base
    bytestring
    hspec
    text
    vector
  ];
  description = "Aeson instances for GitHub Webhook payloads";
  license = lib.licenses.mit;
}
