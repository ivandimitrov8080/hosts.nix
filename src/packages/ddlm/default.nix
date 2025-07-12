{
  fetchFromGitHub,
  rustPlatform,
  ...
}:
rustPlatform.buildRustPackage rec {
  pname = "ddlm";
  version = "1.0";
  src = fetchFromGitHub {
    owner = "ivandimitrov8080";
    repo = "ddlm";
    rev = "5dc510b59179a0f4c0d11d50ad847a7f929466a8";
    hash = "sha256-XTkgT4SbUBQ4goD94/lU5IURmx7pzuHWC2GaGSRL7DM=";
  };
  cargoHash = "sha256-9DltXjD7AxRNCg/TWO4xYnIppJfWLoPgRJ0/u7IA+VM=";

  meta = {
    mainProgram = pname;
  };
}
