{
  pkgs,
  writers,
  ...
}:
writers.writeHaskellBin "twvtodo" {
  libraries = with pkgs.haskellPackages; [
    iCalendar
    aeson
  ];
} (builtins.readFile ./Main.hs)
