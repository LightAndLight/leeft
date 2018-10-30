{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  subst = haskellPackages.callPackage (import ./nix/subst.nix) {};

  drv = variant (haskellPackages.callPackage (import ./leeft.nix) { inherit subst; });

in

  if pkgs.lib.inNixShell then drv.env else drv
