{
  description = "A very basic flake";
  inputs.privateDep = {
    url = "github:garnix-testing-org/minimal-collaborators-test";
    flake = false;
  };
  outputs =
    {
      self,
      nixpkgs,
      privateDep,
    }:
    { };
}
