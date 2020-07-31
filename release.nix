let
  pkgs = import <nixpkgs> { };

in
  { model = pkgs.haskellPackages.callPackage ./model.nix { };
  }
