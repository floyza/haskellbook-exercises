{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [
      hoogle
      haskell-language-server
    ]))
  ];
}
