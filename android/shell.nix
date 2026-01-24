# Builds and tests the app on the desktop, where Hatter's platform calls are
# stubs: `nix-shell --run 'cabal test'`.
let
  hatter = import ./nix/hatter.nix;
  inherit (hatter) pkgs;
  hpkgs = (import "${hatter.src}/nix/hpkgs.nix" { inherit pkgs; }).extend
    (pkgs.lib.composeExtensions (import ./nix/packages.nix pkgs) (self: super: {
      hatter = pkgs.haskell.lib.dontHaddock
        (pkgs.haskell.lib.dontCheck super.hatter-project);
      toxcore-android = self.callCabal2nix "toxcore-android" (hatter.app pkgs) { };
    }));
in
hpkgs.shellFor {
  packages = ps: [ ps.toxcore-android ];
  nativeBuildInputs = [ pkgs.cabal-install ];
}
