{
  stdenv,
  fetchFromGitLab,
  uutils-coreutils-noprefix,
  ...
}:
stdenv.mkDerivation {
  name = "mobile-config-firefox";
  version = "1";
  src = fetchFromGitLab {
    owner = "postmarketOS";
    repo = "mobile-config-firefox";
    rev = "3fbea365e9ea7bcd317d5d4ef7723f994bbdc381";
    hash = "sha256-pgDMXnEdQg7d1lfGym4V+tsi6JAMgMVGI5ozSu0j0og=";
  };
  nativeBuildInputs = [ uutils-coreutils-noprefix ];

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    cp -r ./* $out
    runHook postInstall
  '';
}
