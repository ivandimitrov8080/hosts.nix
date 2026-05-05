{ inputs }:
let
  system = "aarch64-linux";
  pkgs = import inputs.nixpkgs { inherit system; };
  homeDefaults = inputs.configuration.homeManagerModules.default;
in
{
  oneplus6 = inputs.home-manager.lib.homeManagerConfiguration {
    inherit pkgs;
    modules = [
      homeDefaults
      (
        { lib, ... }:
        {
          home = {
            username = "user";
            homeDirectory = "/home/user";
            stateVersion = lib.trivial.release;
          };
          programs = {
            bat.enable = true;
            bottom.enable = true;
            browserpass.enable = true;
            eza.enable = true;
            fd.enable = true;
            firefox.enable = true;
            fzf.enable = true;
            git.enable = true;
            gpg.enable = true;
            kitty.enable = true;
            mpv.enable = true;
            nushell.enable = true;
            password-store.enable = true;
            ssh.enable = true;
            starship.enable = true;
            taskwarrior.enable = true;
            tealdeer.enable = true;
            tmux.enable = true;
            yazi.enable = true;
            zoxide.enable = true;
          };
          services = {
            gpg-agent.enable = true;
          };
        }
      )
    ];
  };
}
