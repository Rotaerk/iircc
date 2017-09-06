{
  ghc,
  cabal2nix,
  pkgs ? import <nixpkgs> {},
  system ? pkgs.stdenv.system
}:

{
  forLocalPath =
    resultNamePrefix:
    localPath:

      pkgs.runCommand "${resultNamePrefix}.c2n" {
        buildInputs = [ cabal2nix ];
      } ''
        mkdir -p "$out"
        cabal2nix --compiler=${ghc.name} --system=${system} file://"${localPath}" >"$out/default.nix"
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
            HOME="$out/.home" cabal2nix --compiler=${ghc.name} --system=${system} --hackage-db="$out/.hackageCache/hackage/00-index.tar" --sha256="$sha256" "cabal://$packageId" >"$out/$packageId/default.nix"
          done
          rm -rf "$out/.home" "$out/.hackageCache"
        '';
}
