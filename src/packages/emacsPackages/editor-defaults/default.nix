{
  trivialBuild,
  ...
}:
trivialBuild {
  pname = "editor-defaults";
  version = "0.1.0";
  src = ./editor-defaults.el;
  packageRequires = [ ];
}
