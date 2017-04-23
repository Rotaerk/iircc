{ refsWithLocalSource ? [] }:

let
  project = "iircc";
  inherit (import ./refs.nix { inherit refsWithLocalSource; }) sourceImports;
  reflex-platform = sourceImports.reflex-platform {};
  pkgs = reflex-platform.nixpkgs;
  haskellPackages =
    reflex-platform.ghc.override {
      overrides = self: super:
        {
          runCabal2Nix = import ./runCabal2Nix.nix { compilerName = self.ghc.name; inherit pkgs; };
        };
    };
in
  pkgs.haskell.lib.overrideCabal
    (haskellPackages.callPackage (haskellPackages.runCabal2Nix.forLocalPath "${project}" ./.) {})
    (drv: {
      src = builtins.filterSource (path: type: baseNameOf path != ".git") drv.src;
    })
