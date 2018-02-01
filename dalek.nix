let
  rev = "39cd40f7bea40116ecb756d46a687bfd0d2e550e";
  pkgs = import (builtins.fetchTarball "github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {};
in (pkgs.haskell.packages.ghc802.override {
   overrides = self: super: {
     dhall = self.callCabal2nix "dhall" (pkgs.fetchFromGitHub {
         owner = "ramirez7";
         repo = "dhall-haskell";
         rev = "1210edef7db68a1f289917714a6d8906ecd45035";
         sha256 = "1ds5b6d6fw9sazg2xl3kqc6dqcwf7csbw7fhy90mjlgspf0dj8yf";
       }) {};

   };
}).callCabal2nix "dalek" ./. {}
