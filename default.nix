{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", hoogle ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./leeft.nix;

  hp =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  hp' =
    hp.override {
      overrides = self: super: {
        grin = hp'.callPackage (import ./grin/grin) {};
        functor-infix = self.callPackage (import ./nix/functor-infix.nix) {};
        llvm-hs = self.callPackage (import ./nix/llvm-hs.nix) { llvm-config = pkgs.llvm_7; };
        llvm-hs-pure = self.callPackage (import ./nix/llvm-hs-pure.nix) {};
        llvm-hs-pretty = self.callPackage (import ./nix/llvm-hs-pretty.nix) {};
      } //
      (if hoogle
       then {
         ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
         ghcWithPackages = self.ghc.withPackages;
       }
       else {});
    };

  llc-7 =
    pkgs.runCommand "llc-7"
      { buildInputs = [pkgs.llvm_7]; }
      ''
        mkdir -p $out/bin
        cd $out/bin
        ln -s ${pkgs.llvm_7}/bin/llc llc-7
        ln -s ${pkgs.llvm_7}/bin/opt opt-7
      '';

  drv = hp'.callPackage f { inherit llc-7; };

in

  drv
