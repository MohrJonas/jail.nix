{
  # This is a separate flake file from the one at the root because we don't
  # want to depend on nixpkgs
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  outputs = { nixpkgs, ... }: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    lib = pkgs.lib;
    jail = import ../lib.nix { inherit pkgs; };

    ansi = clrCode: "[${toString clrCode}m";
    red = ansi 31;
    reset = ansi 0;

    test = {
      assertStdout = actual: expected: ''
        ACTUAL=$(${lib.getExe actual})
        if test "$ACTUAL" != ${lib.escapeShellArg expected}; then
          printf '${red}Expected "%s" to equal "%s".${reset}' "$ACTUAL" ${lib.escapeShellArg expected}
          exit 1
        fi
      '';

      assertFileDoesNotExist = fileName: ''
        if test -e ${lib.escapeShellArg fileName}; then
          printf '${red}Expected "%s" to not exist.${reset}' ${lib.escapeShellArg fileName}
          exit 1
        fi
      '';
    };

    normalizeTestBody = testBody:
      let type = builtins.typeOf testBody; in
      if type == "string" then testBody else
      if type == "list" then lib.concatStringsSep "\n" (map normalizeTestBody testBody) else
      if lib.isDerivation testBody then lib.getExe testBody else
      throw "Unknown test body type ${type}";
  in {
    checks.x86_64-linux = lib.mapAttrs (name: testBody:
      pkgs.runCommand (lib.strings.sanitizeDerivationName name) {} ''
        export HOME=$(pwd)
        export LANG=
        ${normalizeTestBody testBody}
        touch $out
      ''
    ) (import ./test-cases.nix { inherit pkgs lib test jail; });
  };
}
