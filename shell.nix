{ pkgs ? import <nixpkgs> {}, compiler ? "ghc8104" }:  # needs to match the default version of ghc in your nixos release
(import ./default.nix { inherit pkgs compiler; }).env
