{
  description = "A very basic flake";
  inputs.privateDep = {
    url = "github:jkarni/test-repo-private";
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
