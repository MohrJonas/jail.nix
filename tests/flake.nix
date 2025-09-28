{
  # This is a separate flake file from the one at the root because we don't
  # want to depend on nixpkgs
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  outputs =
    { nixpkgs, ... }:
    let
      pkgs = import nixpkgs { system = "x86_64-linux"; };
      lib = pkgs.lib;
      haskellPackages = pkgs.haskell.packages.ghc984;
      cabal2nix = haskellPackages.callCabal2nix "jail-nix-tests" ./. { };
      testDependencies = [
        (haskellPackages.ghc.withPackages (p: cabal2nix.buildInputs))
        pkgs.cabal-install
      ];
      mkApp = name: runtimeInputs: script: {
        type = "app";
        program = lib.getExe (
          pkgs.writeShellApplication {
            inherit name runtimeInputs;
            text = script;
          }
        );
      };
    in
    {
      apps.x86_64-linux.runTests = mkApp "run-all-jail-nix-tests" testDependencies ''
        ${
          lib.pipe
            {
              jail-nix = ../.;
              nixpkgs = nixpkgs;
            }
            [
              (lib.mapAttrsToList (name: path: "${name}=${path}"))
              (lib.concatStringsSep " ")
              (lib.toShellVar "NIX_PATH")
            ]
        } cabal test
      '';

      apps.x86_64-linux.checkFmt = mkApp "check-fmt" [ pkgs.nixfmt-tree ] "treefmt --ci";

      apps.x86_64-linux.fmt = mkApp "fmt" [ pkgs.nixfmt-tree ] "treefmt";

      devShells.x86_64-linux.default = pkgs.mkShell {
        buildInputs = testDependencies ++ [
          haskellPackages.haskell-language-server
          pkgs.ghcid
          pkgs.hpack
        ];
      };
    };
}
