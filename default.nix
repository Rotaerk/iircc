{ refsWithLocalSource ? [] }:

let
  project = "iircc";
  inherit (import ./refs.nix { inherit refsWithLocalSource; }) sourceImports c2nResultsWith;
  reflex-platform = sourceImports.reflex-platform {};
  pkgs = reflex-platform.nixpkgs;
  haskellPackages =
    reflex-platform.ghc.override {
      overrides = self: super:
        let
          c2n = c2nResultsWith self.runCabal2Nix;
        in {
          mkDerivation = args: super.mkDerivation (args // {
            enableLibraryProfiling = true;
            # enableExecutableProfiling = true;
          });

          runCabal2Nix = import ./runCabal2Nix.nix { compilerName = super.ghc.name; cabal2nix = super.cabal2nix; inherit pkgs; };
        };
    };
in
  pkgs.haskell.lib.overrideCabal
    (haskellPackages.callPackage (haskellPackages.runCabal2Nix.forLocalPath "${project}" ./.) {})
    (drv: {
      src = builtins.filterSource (path: type: baseNameOf path != ".git") drv.src;
    })
