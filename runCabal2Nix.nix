{
  compilerName,
  pkgs ? import <nixpkgs> {},
  system ? pkgs.stdenv.system
}:

let
  haskellPackages =
    pkgs.haskell.packages.ghc801.override {
      overrides = self: super: {
        cabal2nix =
          pkgs.haskell.lib.overrideCabal
            (self.callPackage (import ./cabal2nix.cabal.nix) {})
            (drv: {
              isLibrary = false;
              enableSharedExecutables = false;
              executableToolDepends = [ pkgs.makeWrapper ];
              postInstall = ''
                exe=$out/libexec/${drv.pname}-${drv.version}/${drv.pname}
                install -D $out/bin/${drv.pname} $exe
                rm -rf $out/{bin,lib,share}
                makeWrapper $exe $out/bin/${drv.pname} --prefix PATH ":" "${pkgs.nix-prefetch-scripts}/bin"
                mkdir -p $out/share/bash-completion/completions
                $exe --bash-completion-script $exe >$out/share/bash-completion/completions/${drv.pname}
              '';
            });
      };
    };
  cabal2nix = haskellPackages.cabal2nix;
in {

  forLocalPath =
    resultNamePrefix:
    localPath:

      pkgs.runCommand "${resultNamePrefix}.c2n" {
        buildInputs = [ cabal2nix ];
      } ''
        mkdir -p "$out"
        cabal2nix --compiler=${compilerName} --system=${system} file://"${localPath}" >"$out/default.nix"
      '';

  forHackagePackages =
    resultNamePrefix:
    packageSpecs: # list of attrsets, each containing a packageId and sha256

      let cabalConfig =
        builtins.toFile "cabal.config" ''
          repository hackage
            url: http://hackage.haskell.org/
          remote-repo-cache: .
        '';
      in
        pkgs.runCommand "${resultNamePrefix}.c2n" {
          buildInputs = [ cabal2nix pkgs.cabal-install ];
          packageSpecs = map (spec: spec.packageId + ":" + spec.sha256) packageSpecs;
        } ''
          mkdir -p "$out" "$out/.home" "$out/.hackageCache"
          cd "$out/.hackageCache"
          cabal --config-file="${cabalConfig}" update
          cd "$out"
          for spec in ''${packageSpecs[*]}
          do
            IFS=":" read -ra fields <<< "$spec"
            packageId=''${fields[0]}
            sha256=''${fields[1]}
            mkdir -p "$out/$packageId"
            HOME="$out/.home" cabal2nix --compiler=${compilerName} --system=${system} --hackage-db="$out/.hackageCache/hackage/00-index.tar" --sha256="$sha256" "cabal://$packageId" >"$out/$packageId/default.nix"
          done
          rm -rf "$out/.home" "$out/.hackageCache"
        '';
}
