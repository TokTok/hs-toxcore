# The app's APK, for arm64 phones.
let
  hatter = import ./hatter.nix;
  lib = import "${hatter.src}/nix/lib.nix" { inherit (hatter) sources; };
in
lib.mkApk {
  sharedLibs = [{ lib = import ./android.nix { }; abiDir = "arm64-v8a"; }];
  androidSrc = ../platform;
  baseJavaSrc = "${hatter.src}/android/java";
  # A debug key, the same for every build, so each installs over the last.
  keystore = ./debug.keystore;
  apkName = "toxcore-android.apk";
  name = "toxcore-android-apk";
}
