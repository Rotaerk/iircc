{ refsWithLocalSource ? [] }:

let
  project = "iircc";
  inherit (import ./refs.nix { inherit refsWithLocalSource; }) sourceImports relSourceOverrides c2nResultsWith;
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
            enableExecutableProfiling = true;
          });

          runCabal2Nix = import ./runCabal2Nix.nix { compilerName = self.ghc.name; inherit pkgs; };

          irc-core = self.callPackage (c2n.relSourceImports.irc-core "irc-core" "lib") {};
          # irc-core = relSourceOverrides.irc-core "lib" "2.2.1" super.irc-core;
        };
    };
in
  pkgs.haskell.lib.overrideCabal
    (haskellPackages.callPackage (haskellPackages.runCabal2Nix.forLocalPath "${project}" ./.) {})
    (drv: {
      src = builtins.filterSource (path: type: baseNameOf path != ".git") drv.src;
    })
