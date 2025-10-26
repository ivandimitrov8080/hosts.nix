{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };
  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      packages.${system} = {
        hello = nixpkgs.legacyPackages.x86_64-linux.hello;
        default = self.packages.x86_64-linux.hello;
      };
      checks.${system} = {
        default = pkgs.testers.runNixOSTest {
          name = "test";
          nodes = {
            machine =
              { pkgs, ... }:
              {
                environment.systemPackages = [ pkgs.hello ];
              };
          };
          testScript =
            #py
            ''
              machine.wait_for_unit("multi-user.target");
              machine.succeed("hello");
            '';
        };
      };
    };
}
