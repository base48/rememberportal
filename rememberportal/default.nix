{ pkgs ? import <nixpkgs> {}, compiler ? "ghc864" }:
# FIXME (pkgs.haskell.packages.${compiler}.callPackage ./rememberportal.nix {})
pkgs.haskell.lib.dontHaddock (pkgs.haskell.packages.${compiler}.callPackage ./rememberportal.nix {})
