{ inputs, pkgs }:
{
  imports = with inputs.configuration.homeManagerModules; [
    default
  ];
  xdg.enable = true;
  home.packages = with pkgs; [
    devenv
    vit
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
    gpg.enable = true;
    imv.enable = true;
    kitty.enable = true;
    mpv.enable = true;
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
    yazi.enable = true;
    zoxide.enable = true;
    zsh.enable = true;
    opencode = {
      enable = true;
      agents = {
        "UI/UX" = ''
          ---
          description: Writes consistent and good-lookng web styles
          mode: primary
          temperature: 0.1
          tools:
            write: true
            edit: true
            bash: false
          ---

          You are in UI/UX mode. Focus on:

          - Consistent styles and best practices
          - Responsive design
          - Simplicity over complexity
          - Features over simplicity
          - Beautiful websites

          Write beautiful websites without sacrificing simplicity or features. Write to files without
          asking.
        '';
      };
    };
    nushell = {
      enable = true;
      extraConfig = pkgs.lib.mkAfter ''
        use ${pkgs.xin}/bin/xin
      '';
    };
  };
  services = {
    gpg-agent.enable = true;
    wpaperd.enable = true;
    mako.enable = true;
    gammastep = {
      enable = true;
      latitude = 50.0;
      longitude = 14.41;
    };
  };
  wayland.windowManager.sway.enable = true;
}
