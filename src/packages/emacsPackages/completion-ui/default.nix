{
  trivialBuild,
  which-key,
  vertico,
  orderless,
  marginalia,
  consult,
  helpful,
  ...
}:
trivialBuild {
  pname = "completion-ui";
  version = "0.1.0";
  src = ./completion-ui.el;
  packageRequires = [
    which-key
    vertico
    orderless
    marginalia
    consult
    helpful
  ];
}
