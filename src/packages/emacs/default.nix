{
  emacs-unstable-pgtk,
  emacsWithPackagesFromUsePackage,
  python3,
  python3Packages,
  coreutils,
  typst,
  nixd,
  haskell-language-server,
  typescript-language-server,
  vscode-html-languageserver,
  vscode-css-languageserver,
  tinymist,
  elmPackages,
  mcp-server-fetch,
  mcp-server-filesystem,
  mcp-server-time,
  mcp-server-git,
  open-websearch,
  libwebp,
  curl,
  noto-fonts,
  noto-fonts-color-emoji,
  noto-fonts-lgc-plus,
  pandoc,
  discount,
  tree,
  nixfmt,
  ghc,
  exiftool,
  aspellWithDicts,
  askpass,
  ...
}:
emacsWithPackagesFromUsePackage {
  # Your Emacs config file. Org mode babel files are also
  # supported.
  # NB: Config files cannot contain unicode characters, since
  #     they're being parsed in nix, which lacks unicode
  #     support.
  config = ./emacs.el;

  # Whether to include your config as a default init file.
  # If being bool, the value of config is used.
  # Its value can also be a derivation like this if you want to do some
  # substitution:
  #   defaultInitFile = pkgs.substituteAll {
  #     name = "default.el";
  #     src = ./emacs.el;
  #     inherit (config.xdg) configHome dataHome;
  #   };
  defaultInitFile = true;

  # Package is optional, defaults to pkgs.emacs
  package = emacs-unstable-pgtk;

  # Optionally provide extra packages not in the configuration file.
  # This can also include extra executables to be run by Emacs (linters,
  # language servers, formatters, etc)
  extraEmacsPackages =
    epkgs:
    with epkgs; [
      pass-integration
      editor-defaults
      ui-chrome
      completion-ui
      structural-editing
      llm-integration
      code-intelligence
      language-support
      git-workflow
      org-workflow
      messaging
      media-playback
      rss-feeds
      nix-tooling
      utility-commands
      # Runtime executables and non-Elisp dependencies.
      haskell-language-server
      elmPackages.elm-language-server
      nixd
      typescript-language-server
      vscode-html-languageserver
      vscode-css-languageserver
      tinymist
      coreutils
      typst
      python3Packages.python-lsp-server
      (python3.withPackages (
        ps: with ps; [
          epc
          networkx
          pygments
          grep-ast
          diskcache
          tiktoken
          tqdm
          gitignore-parser
          scipy
          litellm
          orjson
        ]
      ))
      (ghc.withPackages (
        hp: with hp; [
          notmuch
          mime
          hp.pandoc
          zlib
          Glob
          extra
          tar
        ]
      ))
      mcp-server-fetch
      mcp-server-filesystem
      mcp-server-time
      mcp-server-git
      open-websearch
      libwebp
      curl
      noto-fonts
      noto-fonts-color-emoji
      noto-fonts-lgc-plus
      pandoc
      discount
      tree
      nixfmt
      exiftool
      askpass
      (aspellWithDicts (
        d: with d; [
          en
          bg
        ]
      ))
    ];
}
