{ pkgs ? import <nixpkgs> {}, compiler ? "ghc984" }:  # needs to match the default version of ghc in your nixos release
# https://romanofskiat.wordpress.com/2019/02/07/ghc-cant-find-a-package-database/
# FIXME (pkgs.haskell.packages.${compiler}.callPackage ./rememberportal.nix {})
pkgs.haskell.lib.dontHaddock (pkgs.haskell.packages.${compiler}.callPackage ./rememberportal.nix {})
