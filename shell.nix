{ pkgs ? import <nixpkgs> {}, compiler ? "ghc882" }:
(import ./default.nix { inherit pkgs compiler; }).env
