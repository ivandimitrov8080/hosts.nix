{
  trivialBuild,
  nixos-options,
  consult,
  ...
}:
(trivialBuild {
  pname = "vertico-nixos-options";
  version = "0.3.0";
  src = ./vertico-nixos-options.el;
  packageRequires = [
    nixos-options
    consult
  ];
})
