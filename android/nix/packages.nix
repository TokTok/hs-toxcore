# The Haskell packages the app needs that nixpkgs has broken, too old or
# lacks: hs-toxcore with its sync library, hs-toxav and hs-toxcore's msgpack
# packages from the workspace's checkouts, and qrcode-core with its version
# bounds lifted.
pkgs: self: super:
let
  inherit (pkgs.lib) fileset;
  inherit (pkgs.haskell.lib) doJailbreak markUnbroken overrideCabal;

  # Only the library: its tools and tests have dependencies the app doesn't
  # need, and some can't be built for Android.
  libraryWith = args: name: dir: overrideCabal
    (self.callCabal2nix name
      (fileset.toSource {
        root = dir;
        fileset = fileset.unions [
          (dir + "/${name}.cabal")
          (dir + "/LICENSE")
          (dir + "/src")
          (fileset.maybeMissing (dir + "/cbits"))
        ];
      })
      args)
    (old: {
      postPatch = (old.postPatch or "") + ''
        sed -i '/^\(executable\|test-suite\|benchmark\) /,$d' ${name}.cabal
      '';
      executableHaskellDepends = [ ];
      testHaskellDepends = [ ];
      benchmarkHaskellDepends = [ ];
      doCheck = false;
      doHaddock = false;
      enableLibraryProfiling = false;
    });
  library = libraryWith { };
in
{
  msgpack-types = library "msgpack-types" ../../../hs-msgpack-types;
  msgpack-binary = library "msgpack-binary" ../../../hs-msgpack-binary;
  msgpack-arbitrary = library "msgpack-arbitrary" ../../../hs-msgpack-arbitrary;
  toxcore = doJailbreak (library "toxcore" ../..);
  toxcore-sync = library "toxcore-sync" ../../sync;
  toxav = libraryWith
    {
      libavcodec = pkgs.ffmpeg;
      ffmpeg = pkgs.ffmpeg;
      opus = pkgs.libopus;
    } "toxav" ../../../hs-toxav;
  qrcode-core = doJailbreak (markUnbroken super.qrcode-core);
}
