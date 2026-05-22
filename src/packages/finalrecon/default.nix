{
  lib,
  stdenv,
  fetchFromGitHub,
  python3,
  makeWrapper,
}:

let
  py = python3.withPackages (
    ps: with ps; [
      requests
      beautifulsoup4
      lxml
      dnspython
      aiohttp
      tldextract
      cryptography
    ]
  );
in
stdenv.mkDerivation (finalAttrs: {
  pname = "finalrecon";
  version = "0-unstable-2026-05-22";

  src = fetchFromGitHub {
    owner = "thewhiteh4t";
    repo = "FinalRecon";
    rev = "5b4846ae92c1c94127527ff4da8f52c329b07bf9";
    hash = "sha256-1/3WcuTszM8MMMah59lSv9IYPH0mT1yX7VeRfnbdJ0s=";
  };

  nativeBuildInputs = [ makeWrapper ];

  buildInputs = [ py ];

  dontBuild = true;
  doCheck = false;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/finalrecon $out/bin
    cp -R . $out/lib/finalrecon
    chmod +x $out/lib/finalrecon/finalrecon.py
    makeWrapper ${py}/bin/python3 $out/bin/finalrecon \
      --add-flags "$out/lib/finalrecon/finalrecon.py"
    runHook postInstall
  '';

  meta = {
    description = "All In One Web Recon";
    homepage = "https://github.com/thewhiteh4t/FinalRecon";
    changelog = "https://github.com/thewhiteh4t/FinalRecon/blob/${finalAttrs.src.rev}/CHANGELOG.md";
    license = lib.licenses.mit;
    mainProgram = "finalrecon";
    platforms = lib.platforms.all;
  };
})
