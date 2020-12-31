{ pkgs ? import <nixpkgs> {}, compiler ? "ghc882" }:
# https://romanofskiat.wordpress.com/2019/02/07/ghc-cant-find-a-package-database/
# FIXME (pkgs.haskell.packages.${compiler}.callPackage ./rememberportal.nix {})
pkgs.haskell.lib.dontHaddock (pkgs.haskell.packages.${compiler}.callPackage ./rememberportal.nix {})
