let
  rev = "39cd40f7bea40116ecb756d46a687bfd0d2e550e";
  pkgs = import (builtins.fetchTarball "github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {};
in (pkgs.haskell.packages.ghc802.override {
   overrides = self: super: {
     dhall = self.callCabal2nix "dhall" (pkgs.fetchFromGitHub {
         owner = "ramirez7";
         repo = "dhall-haskell";
         rev = "b47dedfbd4408a2dc07d4df08483947a3fb8b354";
         sha256 = "13zjysrb1park6aj25sjfpbz1fiml7sam5mdymcmb7gx4xrysg64";
       }) {};

   };
}).callCabal2nix "dalek" ./. {}
