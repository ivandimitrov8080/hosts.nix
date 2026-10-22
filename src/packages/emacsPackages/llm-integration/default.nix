{
  trivialBuild,
  gptel,
  gptel-agent,
  mcp,
  pass-integration,
  ...
}:
trivialBuild {
  pname = "llm-integration";
  version = "0.1.0";
  src = ./llm-integration.el;
  packageRequires = [
    gptel
    gptel-agent
    mcp
    pass-integration
  ];
}
