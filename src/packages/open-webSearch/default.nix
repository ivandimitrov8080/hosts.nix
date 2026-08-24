{
  lib,
  fetchFromGitHub,
  buildNpmPackage,
}:
let
  pname = "open-webSearch";
  version = "2.1.9";
  src = fetchFromGitHub {
    owner = "Aas-ee";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-ZS56Eoy9IePLeyopv4AK6FU8+b1E8r/WPK6RYDvy6yA=";
  };
in
buildNpmPackage {
  inherit pname version src;

  npmDepsHash = "sha256-Ua20YOYr/D06eMQsgBgfN/W7F74wfjjHXL10XIB0nFA=";

  meta = {
    description = "Multi-engine MCP server, CLI, and local daemon for agent web search and content retrieval — skill-guided workflows, no API keys.";
    homepage = "https://github.com/Aas-ee/open-webSearch";
    license = lib.licenses.asl20;
    maintainers = [ ];
  };
}
