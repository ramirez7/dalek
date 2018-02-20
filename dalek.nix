let
  rev = "39cd40f7bea40116ecb756d46a687bfd0d2e550e";
  pkgs = import (builtins.fetchTarball "github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {};
in (pkgs.haskell.packages.ghc802.override {
   overrides = self: super: {
     dhall = self.callCabal2nix "dhall" (pkgs.fetchFromGitHub {
         owner = "ramirez7";
         repo = "dhall-haskell";
         rev = "cfbff072501b36cdf9747bdcbb691fc2cf45c956";
         sha256 = "14qaaysgw6j1ha5r7k5n5iwxms7xkp3665j4gl1m36n1f2fd091r";
       }) {};

   };
}).callCabal2nix "dalek" ./. {}
