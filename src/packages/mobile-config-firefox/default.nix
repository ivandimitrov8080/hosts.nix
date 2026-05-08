{
  stdenv,
  fetchFromGitLab,
  ...
}:
stdenv.mkDerivation {
  name = "mobile-config-firefox";
  version = "1";
  src = fetchFromGitLab {
    domain = "gitlab.postmarketos.org";
    owner = "postmarketOS";
    repo = "mobile-config-firefox";
    rev = "a54416508bf300fb5190a20a5e265e30f6946cf9";
    hash = "sha256-/3BzFMIxG4dGUBhxkfIV59kKMi05ipX3q9Gs1Jok/6E=";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out
    make DESTDIR="$out" install
    runHook postInstall
  '';
}
