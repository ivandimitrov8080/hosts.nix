{
  trivialBuild,
  nix-mode,
  nix-ts-mode,
  rust-mode,
  elm-mode,
  haskell-ts-mode,
  nushell-mode,
  typst-ts-mode,
  markdown-mode,
  yaml-mode,
  eglot,
  company,
  treesit-grammars,
  ...
}:
trivialBuild {
  pname = "language-support";
  version = "0.1.0";
  src = ./language-support.el;
  packageRequires = [
    nix-mode
    nix-ts-mode
    rust-mode
    elm-mode
    haskell-ts-mode
    nushell-mode
    typst-ts-mode
    markdown-mode
    yaml-mode
    eglot
    company
    treesit-grammars.with-all-grammars
  ];
}
