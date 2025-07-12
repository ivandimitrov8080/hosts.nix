{
  fetchFromGitHub,
  rustPlatform,
  ...
}:
rustPlatform.buildRustPackage rec {
  pname = "ndlm";
  version = "1.0";
  src = fetchFromGitHub {
    owner = "ivandimitrov8080";
    repo = "ndlm";
    rev = "b99b9226a4be9173ec10292677abe0d60b17744e";
    hash = "sha256-JgEdLFZ/2LL9AnEtOvl1+WuYHHnQaLqgfoE6h3m/2tk=";
  };
  cargoHash = "sha256-8f71QI+TlqZh4Ogx4asjCx8r4dNx2vjF3juuWuFcDys=";

  meta = {
    mainProgram = pname;
  };
}
