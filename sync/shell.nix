# Builds and tests the library with the Haskell packages hTox uses:
# `nix-shell --run 'cabal test'`.
let
  hatter = import ../android/nix/hatter.nix;
  inherit (hatter) pkgs;
  hpkgs = (import "${hatter.src}/nix/hpkgs.nix" { inherit pkgs; }).extend (self: super: {
    toxcore-sync = self.callCabal2nix "toxcore-sync" ./. { };
  });
in
hpkgs.shellFor {
  packages = ps: [ ps.toxcore-sync ];
  nativeBuildInputs = [ pkgs.cabal-install ];
}
