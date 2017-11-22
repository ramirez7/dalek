let
  rev = "39cd40f7bea40116ecb756d46a687bfd0d2e550e";
  pkgs = import (builtins.fetchTarball "github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {};
in (pkgs.haskell.packages.ghc802.override {
   overrides = self: super: {
     dhall = self.callCabal2nix "dhall" (pkgs.fetchFromGitHub {
         owner = "dhall-lang";
         repo = "dhall-haskell";
         rev = "be10607f96f694ad3092eff40ef74c9c5d227bcd";
         sha256 = "067n0sc22nkaciq7v1zw58ikya4wkw73p7igd4crfi2igzrlghpb";
       }) {};

   };
}).callCabal2nix "dhall-extensions" ./. {}
