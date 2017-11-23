let
  rev = "39cd40f7bea40116ecb756d46a687bfd0d2e550e";
  pkgs = import (builtins.fetchTarball "github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {};
in (pkgs.haskell.packages.ghc802.override {
   overrides = self: super: {
     dhall = self.callCabal2nix "dhall" (pkgs.fetchFromGitHub {
         owner = "ramirez7";
         repo = "dhall-haskell";
         rev = "8cdb0f08c72f43b4f7006ef393fc3ec94ea3844e";
         sha256 = "1w0q1f5sd6yj0zpyjl75ws31rkw4cg9p1dq5kncnc54kifirx349";
       }) {};

   };
}).callCabal2nix "dhall-extensions" ./. {}
