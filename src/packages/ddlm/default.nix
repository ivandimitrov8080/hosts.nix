{
  fetchFromSourcehut,
  rustPlatform,
  ...
}:
rustPlatform.buildRustPackage rec {
  pname = "ddlm";
  version = "1.0";
  src = fetchFromSourcehut {
    owner = "kennylevinsen";
    repo = "dlm";
    rev = "6b0e11c4f453b1a4d7a32019227539a980b7ce66";
    hash = "";
  };
  cargoHash = "";

  meta = {
    mainProgram = pname;
  };
}
