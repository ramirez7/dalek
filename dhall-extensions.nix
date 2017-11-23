let
  rev = "39cd40f7bea40116ecb756d46a687bfd0d2e550e";
  pkgs = import (builtins.fetchTarball "github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {};
in (pkgs.haskell.packages.ghc802.override {
   overrides = self: super: {
     dhall = self.callCabal2nix "dhall" (pkgs.fetchFromGitHub {
         owner = "dhall-lang";
         repo = "dhall-haskell";
         rev = "755f91b6e0ed3a2c87c434a2fb2d927039a3ffdc";
         sha256 = "0g91gv8dd73715mcn7zlmckvlx9yccg8nbr7kxy2qnbvdfdjdj7w";
       }) {};

   };
}).callCabal2nix "dhall-extensions" ./. {}
