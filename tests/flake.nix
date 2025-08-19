{
  # This is a separate flake file from the one at the root because we don't
  # want to depend on nixpkgs
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  outputs = { nixpkgs, ... }: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    lib = pkgs.lib;
    haskellPackages = pkgs.haskellPackages.callCabal2nix "jail-nix-tests" ./. { };
    testDependencies = [
      (pkgs.haskellPackages.ghc.withPackages (p: haskellPackages.buildInputs))
      pkgs.cabal-install
    ];
  in {
    apps.x86_64-linux.runChecks = {
      type = "app";
      program = lib.getExe (pkgs.writeShellApplication {
        name = "run-all-jail-nix-tests";
        runtimeInputs = testDependencies;
        text = ''
          ${lib.pipe {
            jail-nix = ../.;
            nixpkgs = nixpkgs;
          } [
            (lib.mapAttrsToList (name: path: "${name}=${path}"))
            (lib.concatStringsSep " ")
            (lib.toShellVar "NIX_PATH")
          ]} cabal test
        '';
      });
    };

    devShells.x86_64-linux.default = pkgs.mkShell {
      buildInputs = testDependencies ++ [
        (pkgs.haskell-language-server.override { dynamic = true; })
        pkgs.ghcid
        pkgs.hpack
      ];
    };
  };
}
