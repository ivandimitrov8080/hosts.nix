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
    epkgs: with epkgs; [
      vertico-nixos-options
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
      diff-hl
      flycheck
      flycheck-aspell
      company
      fancy-compilation
      eglot
      org
      org-modern
      valign
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
      gptel-agent
      elfeed
      inhibit-mouse
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
      askpass
      (aspellWithDicts (
        d: with d; [
          en
          bg
        ]
      ))
    ];
}
