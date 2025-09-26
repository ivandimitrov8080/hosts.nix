{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  udev,
  stdenv,
  darwin,
  ...
}:
rustPlatform.buildRustPackage rec {
  pname = "swhkd";
  version = "main"; # https://github.com/waycrate/swhkd/commits/main/

  # src = fetchFromGitHub {
  #   owner = "waycrate";
  #   repo = pname;
  #   rev = "ae372e0aff2e87fbfed11d79bcd7fd9ef5f68a60";
  #   hash = "sha256-EhbRIlI+RsZjPjbYmgu4WzOHJ8udTtlxgJ2kr9iHyd0=";
  # };

  src = fetchFromGitHub {
    owner = "ivandimitrov8080";
    repo = pname;
    rev = "7ef626a1c37244ad73406c61c27bd4a9d3560dd0";
    hash = "sha256-gE/2HNoQGaRRKqwgA4pEJozvfMxcOQJILLtdsCMZvLM=";
  };

  cargoHash = "sha256-LBbmFyddyw7vV5voctXq3L4U3Ddbh428j5XbI+td/dg=";

  nativeBuildInputs = [
    pkg-config
  ];

  buildInputs = [
    udev
  ]
  ++ lib.optionals stdenv.isDarwin [
    darwin.apple_sdk.frameworks.IOKit
  ];

  meta = {
    description = "Sxhkd clone for Wayland (works on TTY and X11 too)";
    homepage = "https://github.com/waycrate/swhkd/tree/main";
    changelog = "https://github.com/waycrate/swhkd/blob/${src.rev}/CHANGELOG.md";
    license = lib.licenses.bsd2;
    mainProgram = pname;
  };
}
