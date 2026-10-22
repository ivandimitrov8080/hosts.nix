{
  trivialBuild,
  avy,
  multiple-cursors,
  expand-region,
  smartparens,
  undo-tree,
  rainbow-delimiters,
  aggressive-indent,
  ...
}:
trivialBuild {
  pname = "structural-editing";
  version = "0.1.0";
  src = ./structural-editing.el;
  packageRequires = [
    avy
    multiple-cursors
    expand-region
    smartparens
    undo-tree
    rainbow-delimiters
    aggressive-indent
  ];
}
