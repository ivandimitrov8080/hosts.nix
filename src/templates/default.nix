{ inputs }:
let
  mkWelcomeText =
    {
      name,
      description,
      path,
    }:
    {
      inherit path;

      description = name;

      welcomeText = ''
        # ${name}
        ${description}
        ## Other tips
        For a quick license setup use license-cli:

        ```
            # SPDX is the license id like MIT or GPL-3.0
            nix shell p#license-cli --command "license text MIT"
        ```
      '';
    };
in
{
  default = mkWelcomeText {
    name = "Default Template";
    description = ''
      A simple flake
    '';
    path = ./default;
  };
}
