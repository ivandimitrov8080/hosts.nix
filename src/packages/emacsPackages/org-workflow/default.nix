{
  trivialBuild,
  org,
  org-modern,
  valign,
  org-appear,
  mixed-pitch,
  olivetti,
  ob-nix,
  markdown-mode,
  htmlize,
  ...
}:
trivialBuild {
  pname = "org-workflow";
  version = "0.1.0";
  src = ./org-workflow.el;
  packageRequires = [
    org
    org-modern
    valign
    org-appear
    mixed-pitch
    olivetti
    ob-nix
    markdown-mode
    htmlize
  ];
}
