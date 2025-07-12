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
    rev = "b6b02b52a5ff4262931e2b0e89aa362e31465965";
    hash = "sha256-EGbuaFqvzAHnr6M3/92nuXyinfc227BQiU8X3N1I7+U=";
  };
  cargoHash = "sha256-9DltXjD7AxRNCg/TWO4xYnIppJfWLoPgRJ0/u7IA+VM=";

  meta = {
    mainProgram = pname;
  };
}
