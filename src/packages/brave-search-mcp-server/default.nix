{
  lib,
  fetchFromGitHub,
  buildNpmPackage,
}:
let
  pname = "brave-search-mcp-server";
  version = "2.1.3";
  src = fetchFromGitHub {
    owner = "brave";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-AHChu8wy8sp337U5hO+dszy299/U1UDjsAVTc7C7jzY=";
  };
in
buildNpmPackage {
  inherit pname version src;

  npmDepsFetcherVersion = 2;
  npmDepsHash = "sha256-vjRZI/wOSs7I5hVy+AedH0erpzJqvYDsmigjzxCri+k=";

  meta = {
    description = "";
    homepage = "https://github.com/brave/brave-search-mcp-server";
    license = lib.licenses.mit;
    maintainers = [ ];
  };
}
