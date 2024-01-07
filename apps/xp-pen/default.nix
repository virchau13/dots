{ pkgs, qt5, callPackage }:
callPackage ./fhsenv.nix { xp-pentablet-unwrapped = qt5.callPackage ./unwrapped.nix {}; }
