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
    rev = "7f83d27dc9dda4be690a577823875bd58917f5b5";
    hash = "sha256-I4IQvEAze0oFlpvC+PdGvRQezdLSytS8xYKUQ83P93A=";
  };
  cargoHash = "sha256-8f71QI+TlqZh4Ogx4asjCx8r4dNx2vjF3juuWuFcDys=";

  meta = {
    mainProgram = pname;
  };
}
