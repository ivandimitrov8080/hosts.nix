{ prev, ... }:
prev.haskell-ts-mode.overrideAttrs (old: {
  dontUnpack = false;
  postPatch = (old.postPatch or "") + ''
    substituteInPlace haskell-ts-mode.el \
      --replace-fail type_synomym type_synonym

    pushd ..
    src="$PWD/$ename-$version.tar"
    tar --create --file="$src" "$ename-$version"
    popd
  '';
})
