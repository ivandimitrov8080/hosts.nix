{
  stdenv,
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
  emacs-overlay,
  docs-hm,
  docs-nixos,
  writeText,
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
  ...
}:
let
  system = stdenv.hostPlatform.system;
  emacs-unstable-pgtk = emacs-overlay.packages.${system}.emacs-unstable-pgtk;
  emacsWithPackagesFromUsePackage = emacs-overlay.lib.${system}.emacsWithPackagesFromUsePackage;
  optionsJsonHm = "${docs-hm}/share/doc/home-manager/options.json";
  optionsJsonNixos = "${docs-nixos}/share/doc/nixos/options.json";

in
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
  defaultInitFile = writeText "default.el" (
    builtins.replaceStrings [ "@nixos-options@" "@hm-options@" ] [ optionsJsonNixos optionsJsonHm ] (
      builtins.readFile ./emacs.el
    )
  );

  # Package is optional, defaults to pkgs.emacs
  package = emacs-unstable-pgtk;

  # By default emacsWithPackagesFromUsePackage will only pull in
  # packages with `:ensure`, `:ensure t` or `:ensure <package name>`.
  # Setting `alwaysEnsure` to `true` emulates `use-package-always-ensure`
  # and pulls in all use-package references not explicitly disabled via
  # `:ensure nil` or `:disabled`.
  # Note that this is NOT recommended unless you've actually set
  # `use-package-always-ensure` to `t` in your config.
  alwaysEnsure = true;

  # For Org mode babel files, by default only code blocks with
  # `:tangle yes` are considered. Setting `alwaysTangle` to `true`
  # will include all code blocks missing the `:tangle` argument,
  # defaulting it to `yes`.
  # Note that this is NOT recommended unless you have something like
  # `#+PROPERTY: header-args:emacs-lisp :tangle yes` in your config,
  # which defaults `:tangle` to `yes`.
  alwaysTangle = true;

  # Optionally provide extra packages not in the configuration file.
  # This can also include extra executables to be run by Emacs (linters,
  # language servers, formatters, etc)
  extraEmacsPackages =
    epkgs: with epkgs; [
      (trivialBuild {
        pname = "vertico-nixos-options";
        version = "0.3.0";
        src = ./vertico-nixos-options.el;
        packageRequires = [
          nixos-options
          consult
        ];
      })
      elm-mode
      haskell-ts-mode
      rust-mode
      nix-mode
      nix-ts-mode
      nixos-options
      nushell-mode
      typst-ts-mode
      projectile
      magit
      flycheck
      flycheck-aspell
      company
      eglot
      org
      org-modern
      org-appear
      mixed-pitch
      olivetti
      ob-nix
      tree-sitter
      treesit-grammars.with-all-grammars
      markdown-mode
      yaml-mode
      catppuccin-theme
      which-key
      vertico
      orderless
      marginalia
      helpful
      consult
      nerd-icons
      nerd-icons-xref
      nerd-icons-dired
      nerd-icons-completion
      nerd-icons-ibuffer
      avy
      multiple-cursors
      expand-region
      doom-modeline
      rainbow-delimiters
      vterm
      smartparens
      undo-tree
      dired-quick-sort
      notmuch
      request
      xterm-color
      gptel
      elfeed
      khalel
      htmlize
      emms
      mcp
      telega
      transmission
      aggressive-indent
      pass
      direnv
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
      (aspellWithDicts (
        d: with d; [
          en
          bg
        ]
      ))
    ];
}
