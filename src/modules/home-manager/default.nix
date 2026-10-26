{
  inputs,
  pkgs,
  ...
}:
{
  imports = with inputs.configuration.homeManagerModules; [
    default
  ];
  xdg.enable = true;
  xdg.mimeApps.defaultApplications = {
    "text/html" = "org.ladybird.Ladybird.desktop";
    "x-scheme-handler/http" = "org.ladybird.Ladybird.desktop";
    "x-scheme-handler/https" = "org.ladybird.Ladybird.desktop";
    "x-scheme-handler/about" = "org.ladybird.Ladybird.desktop";
    "x-scheme-handler/unknown" = "org.ladybird.Ladybird.desktop";
  };
  gtk = {
    enable = true;
    font = {
      name = "FiraCode Nerd Font Mono";
      package = pkgs.nerd-fonts.fira-code;
    };
  };
  qt = {
    enable = true;
    kvantum.enable = true;
    style.name = "kvantum";
  };
  home.packages = with pkgs; [
    devenv
    vit
    openssl
    speedtest-cli
    uutils-coreutils-noprefix
    xdg-user-dirs
    xdg-utils
  ];
  programs = {
    bash.enable = true;
    bat.enable = true;
    bottom.enable = true;
    browserpass.enable = true;
    delta.enable = true;
    direnv.enable = true;
    eza.enable = true;
    fd.enable = true;
    firefox.enable = true;
    fzf.enable = true;
    gh.enable = true;
    git.enable = true;
    git.settings.core.excludesFile = "/home/ivand/.gitignore";
    gpg.enable = true;
    imv.enable = true;
    kitty.enable = true;
    mpv.enable = true;
    mpv.config = {
      save-position-on-quit = "yes";
    };
    nix-index.enable = true;
    password-store.enable = true;
    rofi.enable = true;
    ssh.enable = true;
    starship.enable = true;
    swaylock.enable = true;
    taskwarrior.enable = true;
    tealdeer.enable = true;
    tmux.enable = true;
    waybar.enable = true;
    waybar.settings.mainBar."custom/weather".exec =
      "${pkgs.wttrbar}/bin/wttrbar --observation-time --location Danang";
    yazi.enable = true;
    zoxide.enable = true;
    nushell.enable = true;
    nushell.settings.edit_mode = "emacs";
    nushell.extraConfig = pkgs.lib.mkAfter ''
      def --wrapped sudo [...rest] { ^sudo -A ...$rest }
    '';
    zsh.enable = true;
  };
  services = {
    gpg-agent.enable = true;
    mako.enable = true;
    clipse.enable = true;
    pass-secret-service.enable = true;
    wpaperd.enable = true;
    gammastep = {
      enable = true;
      latitude = 50.0;
      longitude = 14.41;
    };
    emacs = {
      enable = true;
      package = pkgs.emacs-custom;
      extraOptions = [ "--init-directory=~/.config/emacs" ];
      defaultEditor = true;
      client.enable = true;
      startWithUserSession = "graphical";
    };
  };
  systemd.user.services.emacs.Service.ExecStartPost = [
    "-${pkgs.emacs-custom}/bin/emacsclient --create-frame --no-wait"
  ];
  wayland.windowManager.sway.enable = true;
}
