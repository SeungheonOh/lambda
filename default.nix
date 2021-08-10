{ pkgs ? import <nixpkgs> {}, withHoogle ? false}:

let
  haskellPackages' = pkgs.haskellPackages;
  haskellPackages = (
    if withHoogle
    then haskellPackages'.override {
      overrides = (self: super:
        {
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          ghcWithPackages = self.ghc.withPackages;
        }
      );
    }
    else haskellPackages'
  );

  drv = haskellPackages.callCabal2nix (builtins.baseNameOf (builtins.toString ./.)) ./. {};
in
  {
    drv = drv;
    shell = haskellPackages.shellFor {
      packages = p: [drv];
      buildInputs = with pkgs; [ 
        # here you can add developmental tools you need on nix-shell
        cabal-install 
        hlint
        pkgs.haskellPackages.hoogle
        pkgs.haskellPackages.hindent
        ghcid 
        haskell-language-server
      ];
    };
  }
