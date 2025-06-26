# default.nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.callCabal2nix "ghcitui" ./. {}
