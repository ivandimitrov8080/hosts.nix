{
  trivialBuild,
  org,
  request,
  xterm-color,
  khalel,
  ...
}:
trivialBuild {
  pname = "utility-commands";
  version = "0.1.0";
  src = ./utility-commands.el;
  packageRequires = [
    org
    request
    xterm-color
    khalel
  ];
}
