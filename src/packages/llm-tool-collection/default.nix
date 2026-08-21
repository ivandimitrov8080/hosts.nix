{
  lib,
  fetchFromGitHub,
  emacsPackages,
}:
let
  src = fetchFromGitHub {
    owner = "ivandimitrov8080";
    repo = "llm-tool-collection";
    rev = "b9fd45bedf3e0fb07d289730991199ae18785157";
    hash = "sha256-40BSMoM25tdgXeH5+labLYqCPCK4SEuAWovOeJxnzNo=";
  };
in

emacsPackages.trivialBuild {
  pname = "llm-tool-collection";
  version = "1.0";
  inherit src;

  meta = {
    description = "Curated collection of tools for agentic LLMs in Emacs";
    homepage = "https://github.com/ivandimitrov8080/llm-tool-collection";
    license = lib.licenses.gpl3;
    maintainers = [ ];
  };
}
