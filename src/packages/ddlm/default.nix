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
    rev = "85126cd4674f31854c5537bcc14573b92925917d";
    hash = "sha256-rTNEKsTt7tX0hMf06n8Q1tcBLnB0J8q9rjuFgPBp2rY=";
  };
  cargoHash = "sha256-9DltXjD7AxRNCg/TWO4xYnIppJfWLoPgRJ0/u7IA+VM=";

  meta = {
    mainProgram = pname;
  };
}
