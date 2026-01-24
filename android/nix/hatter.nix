# Hatter, and the nixpkgs it is built and tested with. Our fixes to it are in
# the workspace's clone until they are merged.
let
  src = builtins.path {
    name = "hatter";
    path = ../../../third_party/hatter;
    filter = path: _: baseNameOf path != ".git";
  };
  sources = import "${src}/npins";
in
{
  inherit src sources;
  pkgs = import sources.nixpkgs { };
  # The app's Haskell package, without the files only nix and Android read.
  app = pkgs: pkgs.lib.fileset.toSource {
    root = ../.;
    fileset = pkgs.lib.fileset.unions [
      ../toxcore-android.cabal
      ../src
      ../test
    ];
  };
}
