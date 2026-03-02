{
  writers,
  ...
}:
let
  inherit (builtins)
    readFile
    ;
in
writers.writeNuBin "xin"
  # nu
  ''
    ${(readFile ./main.nu)}
  ''
