let
  rev = "39cd40f7bea40116ecb756d46a687bfd0d2e550e";
  pkgs = import (builtins.fetchTarball "github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {};
in (pkgs.haskell.packages.ghc802.override {
   overrides = self: super: {
     prettyprinter = self.callPackage ./nix/prettyprinter.nix {};
     dhall = self.callCabal2nix "dhall" (pkgs.fetchFromGitHub {
         owner = "ramirez7";
         repo = "dhall-haskell";
         rev = "2e303da45fc15edfef9127b59bf3b24a4a48e718";
         sha256 = "1zwsrmga1z7bji75y6jshnq663ackkky025ck46cb8ainigl3hwy";
       }) {};

   };
}).callCabal2nix "dalek" ./. {}
