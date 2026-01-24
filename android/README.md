# hTox

hTox is a Tox client for Android, written in Haskell with
[Hatter](https://github.com/jappeace/hatter) on hs-toxcore, laid out like bTox.

- `src/`: the app. `Model` is the state, how events change it and what it asks
  of Tox, `Network` the Tox that does it and reports back, `Files` the files
  going to and from friends, `Store` what is kept between runs in SQLite (Tox
  gets only our secret key from it, and the schema moves on by migrations), `View` the
  widgets of each screen, `App` the wiring between them and Hatter. `Theme` has bTox's
  light and dark colours. `Identicon` draws keys as bTox and qTox do, and
  `QrCode` Tox IDs as they do, as PNGs from `Png`.
- `app/Main.hs`: the entry point Hatter's Android build runs.
- `platform/`: the Android manifest, theme, icons, activity, and the
  foreground service that keeps Tox running while no screen is open.
- `nix/`: the Hatter pin, the Android builds, and the Haskell packages nixpkgs
  lacks. Hatter builds from the workspace's `third_party/hatter`, which has our
  changes to it, and hs-toxcore and its msgpack packages from their checkouts
  here.

Build the APK, for arm64 phones:

```sh
nix-build nix/apk.nix
adb install result/toxcore-android.apk
```

Every build is signed with `nix/debug.keystore`, so it installs over the last.

Run the tests on the desktop:

```sh
nix-shell --run 'cabal test'
```

The first Android build compiles a cross GHC, which takes a while.
