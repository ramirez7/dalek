let
  rev = "39cd40f7bea40116ecb756d46a687bfd0d2e550e";
  pkgs = import (builtins.fetchTarball "github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {};
in (pkgs.haskell.packages.ghc802.override {
   overrides = self: super: {
     dhall = self.callCabal2nix "dhall" (pkgs.fetchFromGitHub {
         owner = "ramirez7";
         repo = "dhall-haskell";
         rev = "292134b5034bd39ebf0353e1e8aacc7935fe0a6b";
         sha256 = "1jmvwhxqn2cwz318gi2jsbi5fwcl8hi3xicrslbsiqp8lq6s204c";
       }) {};

   };
}).callCabal2nix "dalek" ./. {}
