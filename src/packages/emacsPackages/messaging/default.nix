{
  trivialBuild,
  notmuch,
  telega,
  code-intelligence,
  pass-integration,
  media-playback,
  ...
}:
trivialBuild {
  pname = "messaging";
  version = "0.1.0";
  src = ./messaging.el;
  packageRequires = [
    notmuch
    telega
    code-intelligence
    pass-integration
    media-playback
  ];
}
