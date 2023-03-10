let
  nixpkgsPin = {
    url = https://github.com/NixOS/nixpkgs/archive/c95bf18beba4290af25c60cbaaceea1110d0f727.tar.gz;
  };
  pkgs = import (builtins.fetchTarball nixpkgsPin) {};
in

pkgs.stdenv.mkDerivation rec {
  name = "SIMPL_example";
  src = ./.;
  nativeBuildInputs = with pkgs; with haskellPackages; [
    souffle
  ];
  buildInputs = with pkgs; with haskellPackages; [
    haskell.compiler.ghc8107
    cabal-install
  ];
}
