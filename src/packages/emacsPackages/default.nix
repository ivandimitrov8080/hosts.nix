{ final, prev, lib }:
lib.recurseIntoAttrs {
  vertico-nixos-options = final.callPackage ./vertico-nixos-options { };
  haskell-ts-mode = final.callPackage ./haskell-ts-mode { inherit prev; };

  pass-integration = final.callPackage ./pass-integration { };
  editor-defaults = final.callPackage ./editor-defaults { };
  ui-chrome = final.callPackage ./ui-chrome { };
  completion-ui = final.callPackage ./completion-ui { };
  structural-editing = final.callPackage ./structural-editing { };
  llm-integration = final.callPackage ./llm-integration { };
  code-intelligence = final.callPackage ./code-intelligence { };
  language-support = final.callPackage ./language-support { };
  git-workflow = final.callPackage ./git-workflow { };
  org-workflow = final.callPackage ./org-workflow { };
  messaging = final.callPackage ./messaging { };
  media-playback = final.callPackage ./media-playback { };
  rss-feeds = final.callPackage ./rss-feeds { };
  nix-tooling = final.callPackage ./nix-tooling { };
  utility-commands = final.callPackage ./utility-commands { };
}
