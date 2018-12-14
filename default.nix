{ nixpkgs ? import <nixpkgs> {}, hoogle ? false }:

let

  inherit (nixpkgs) pkgs;

  f = import ./leeft.nix;

  # we need to use ghc822 for llvm-hs 5
  hp = pkgs.haskell.packages.ghc822;

  hp' =
    if hoogle
    then
      hp.override {
        overrides = self: super: {
          temporary = self.callPackage (import ./nix/temporary.nix) {};
          ListLike = self.callPackage (import ./nix/ListLike.nix) {};
          functor-infix = self.callPackage (import ./nix/functor-infix.nix) {};
          llvm-hs = self.callPackage (import ./nix/llvm-hs.nix) { llvm-config = pkgs.llvm; };
          llvm-hs-pure = self.callPackage (import ./nix/llvm-hs-pure.nix) {};
          llvm-hs-pretty =
            # failing tests
            # this might be a mistake
            pkgs.haskell.lib.dontCheck
            (self.callPackage (import ./nix/llvm-hs-pretty.nix) {});
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
        };
      }
    else hp;

  grin = hp'.callPackage (import ./grin/grin) {};

  drv = hp'.callPackage f { inherit grin; };

in

  drv
