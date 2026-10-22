{
  trivialBuild,
  elfeed,
  ...
}:
trivialBuild {
  pname = "rss-feeds";
  version = "0.1.0";
  src = ./rss-feeds.el;
  packageRequires = [ elfeed ];
}
