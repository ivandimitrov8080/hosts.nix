{
  trivialBuild,
  nixos-options,
  vertico-nixos-options,
  direnv,
  ...
}:
trivialBuild {
  pname = "nix-tooling";
  version = "0.1.0";
  src = ./nix-tooling.el;
  packageRequires = [
    nixos-options
    vertico-nixos-options
    direnv
  ];
}
