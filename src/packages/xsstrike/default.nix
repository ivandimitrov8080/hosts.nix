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
      tld
      fuzzywuzzy
      requests
    ]
  );
in
stdenv.mkDerivation (finalAttrs: {
  pname = "xsstrike";
  version = "3.1.5";

  src = fetchFromGitHub {
    owner = "s0md3v";
    repo = "XSStrike";
    rev = "ab27955d367432f944d8f29897e09c15356e76f7";
    hash = "sha256-8H6/OZRKrF5dd4NcZ6Km3mxBSK6UkY1LpmgITAXG/AU=";
  };

  nativeBuildInputs = [ makeWrapper ];

  buildInputs = [ py ];

  dontBuild = true;
  doCheck = false;

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/xsstrike $out/bin
    cp -R . $out/lib/xsstrike
    chmod +x $out/lib/xsstrike/xsstrike.py
    makeWrapper ${py}/bin/python3 $out/bin/xsstrike \
      --add-flags "$out/lib/xsstrike/xsstrike.py"
    runHook postInstall
  '';

  meta = {
    description = "Most advanced XSS scanner.";
    homepage = "https://github.com/s0md3v/XSStrike";
    license = lib.licenses.gpl3;
    mainProgram = "xsstrike";
    platforms = lib.platforms.all;
  };
})
