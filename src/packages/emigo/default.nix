{
  lib,
  makeWrapper,
  fetchFromGitHub,
  emacsPackages,
}:
let
  src = fetchFromGitHub {
    owner = "MatthewZMD";
    repo = "emigo";
    rev = "91d122a85cac1965e1a52185ed8711c5ef8f24c9";
    hash = "sha256-wEOlqTNXBViMCTbBkS8jhyyBmD5JmqqqbIi7lrGmt6o=";
  };
in

emacsPackages.trivialBuild {
  pname = "emigo";
  version = "0-unstable-2025-10-13";
  inherit src;

  packageRequires = with emacsPackages; [
    transient
    compat
    markdown-mode
  ];

  nativeBuildInputs = [ makeWrapper ];

  postInstall = ''
    # Install Python backend alongside the .el files so that
    # emigo-python-file resolves correctly via load-file-name.
    cp ${src}/*.py $out/share/emacs/site-lisp/
  '';

  meta = {
    description = "Agentic AI coding assistant for Emacs";
    homepage = "https://github.com/MatthewZMD/emigo";
    license = lib.licenses.asl20;
    maintainers = [ ];
  };
}
