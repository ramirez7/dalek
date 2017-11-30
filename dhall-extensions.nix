let
  rev = "39cd40f7bea40116ecb756d46a687bfd0d2e550e";
  pkgs = import (builtins.fetchTarball "github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {};
in (pkgs.haskell.packages.ghc802.override {
   overrides = self: super: {
     dhall = self.callCabal2nix "dhall" (pkgs.fetchFromGitHub {
         owner = "ramirez7";
         repo = "dhall-haskell";
         rev = "ffd294b68789540722f47ebc41e4294c158c8dd1";
         sha256 = "0nl1jpafcyj0nl1x75v4svqfhz2a474i9i64xjs6vlfgcy6k4w2n";
       }) {};

   };
}).callCabal2nix "dhall-extensions" ./. {}
