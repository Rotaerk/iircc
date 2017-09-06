{ refsWithLocalSource ? [] }:

let
  project = "iircc";
  inherit (import ./refs.nix { inherit refsWithLocalSource; }) sources sourceImports;
  reflex-platform = sourceImports.reflex-platform {};
  pkgs = reflex-platform.nixpkgs;
  haskellPackages =
    reflex-platform.ghc.override {
      overrides = self: super: {
        mkDerivation = args: super.mkDerivation (args // {
          # enableLibraryProfiling = true;
          # enableExecutableProfiling = true;
        });

        cborg = self.callCabal2nix "cborg" (sources.cborg + /cborg) {};
        serialise = self.callCabal2nix "serialise" (sources.cborg + /serialise) {};
      };
    };
in
  pkgs.haskell.lib.overrideCabal
    (haskellPackages.callCabal2nix "${project}" ./. {})
    (drv: {
      src = builtins.filterSource (path: type: baseNameOf path != ".git") drv.src;
    })
