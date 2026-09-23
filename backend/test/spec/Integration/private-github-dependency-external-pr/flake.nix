{
  description = "A very basic flake";
  inputs.privateDep = {
    url = "github:garnix-testing-org/test-repo-private";
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
