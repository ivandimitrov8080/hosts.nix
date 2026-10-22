{
  trivialBuild,
  magit,
  diff-hl,
  ...
}:
trivialBuild {
  pname = "git-workflow";
  version = "0.1.0";
  src = ./git-workflow.el;
  packageRequires = [
    magit
    diff-hl
  ];
}
