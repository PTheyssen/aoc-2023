let 
  myNixPkgs = import <nixpkgs> {};
in
myNixPkgs.mkShell {
  nativeBuildInputs = with myNixPkgs; [
    (ghc.withPackages (pkgs: with pkgs; [ split matrix groupBy Stack ]))
    pkgs.haskell-language-server 
  ];
}

