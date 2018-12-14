{ nixpkgs ? import <nixpkgs> {} }:

(import ./default.nix { inherit nixpkgs; hoogle = true; }).env
