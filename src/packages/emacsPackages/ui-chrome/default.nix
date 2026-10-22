{
  trivialBuild,
  catppuccin-theme,
  nerd-icons,
  nerd-icons-xref,
  nerd-icons-dired,
  nerd-icons-completion,
  nerd-icons-ibuffer,
  doom-modeline,
  inhibit-mouse,
  ...
}:
trivialBuild {
  pname = "ui-chrome";
  version = "0.1.0";
  src = ./ui-chrome.el;
  packageRequires = [
    catppuccin-theme
    nerd-icons
    nerd-icons-xref
    nerd-icons-dired
    nerd-icons-completion
    nerd-icons-ibuffer
    doom-modeline
    inhibit-mouse
  ];
}
