{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  # subst = haskellPackages.callPackage (import ./nix/subst.nix) {};
  # subst =
    # haskellPackages.callPackage
    # (pkgs.fetchFromGitHub {
      # owner = "lightandlight";
      # repo = "subst";
      # rev = "10445211a173633955a1498df4a2e010e228fca8";
      # sha256 = "0rk4k9h0l045v6ba9ar8bs9yh8s6bnchqgzifsblh20p17qiv9if";
    # }) {};
  # subst =
    # pkgs.fetchFromGitHub {
      # owner = "lightandlight";
      # repo = "subst";
      # rev = "10445211a173633955a1498df4a2e010e228fca8";
      # sha256 = "0rk4k9h0l045v6ba9ar8bs9yh8s6bnchqgzifsblh20p17qiv9if";
    # };
  subst =
    import (fetchTarball "https://github.com/LightAndLight/subst/archive/0.0.1.tar.gz") { inherit nixpkgs compiler; };

  drv = variant (haskellPackages.callPackage (import ./leeft.nix) { inherit subst; });

in

  if pkgs.lib.inNixShell then drv.env else drv
