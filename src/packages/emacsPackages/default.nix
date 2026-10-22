{ final, prev, lib }:
lib.recurseIntoAttrs {
  vertico-nixos-options = final.callPackage ./vertico-nixos-options { };
  haskell-ts-mode = final.callPackage ./haskell-ts-mode { inherit prev; };
}
