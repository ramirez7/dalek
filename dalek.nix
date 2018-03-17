let
  rev = "39cd40f7bea40116ecb756d46a687bfd0d2e550e";
  pkgs = import (builtins.fetchTarball "github.com/NixOS/nixpkgs/archive/${rev}.tar.gz") {};
in (pkgs.haskell.packages.ghc802.override {
   overrides = self: super: {
     prettyprinter = self.callPackage ./nix/prettyprinter.nix {};
     dhall = self.callCabal2nix "dhall" (pkgs.fetchFromGitHub {
         owner = "ramirez7";
         repo = "dhall-haskell";
         rev = "e9abddb164a280f07774e283589df912aadfcb80";
         sha256 = "0qwcwvss0i4k0p28qb0xdnj6c48mxw437i5bc1y4gh75pgf3fydz";
       }) {};

   };
}).callCabal2nix "dalek" ./. {}
