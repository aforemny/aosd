{ pkgs ? import <nixpkgs> { } }:
pkgs.mkShell {
  buildInputs = [
    pkgs.cabal-install
    pkgs.ormolu
    (pkgs.ghc.withPackages (pkgs: [
      pkgs.optparse-applicative
      pkgs.X11
    ]))
  ];
}
