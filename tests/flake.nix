{
  # This is a separate flake file from the one at the root because we don't
  # want to depend on nixpkgs
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
  outputs = { nixpkgs, ... }: let
    pkgs = import nixpkgs { system = "x86_64-linux"; };
    lib = pkgs.lib;
    jail-nix.lib = import ../lib;

    ansi = clrCode: "[${toString clrCode}m";
    green = ansi 32;
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

    testCases = import ./test-cases.nix { inherit pkgs lib test jail-nix; };
  in {
    apps.x86_64-linux.runChecks = {
      type = "app";
      program = lib.getExe (pkgs.writeShellApplication {
        name = "run-all-jail-nix-tests";
        text = ''
          printf 'Running tests...\n'
          TESTS=(${lib.pipe testCases [
            lib.attrNames
            (map lib.escapeShellArg)
            (lib.concatStringsSep " ")
          ]})
          TEST_FAIL_LOG=$(mktemp)
          trap 'rm $TEST_FAIL_LOG' EXIT
          (
            for TEST_NAME in "''${TESTS[@]}"; do
              (
                if TEST_OUTPUT=$(nix build -L .#checks.x86_64-linux."$TEST_NAME" 2>&1); then
                  printf '  %s [${green}✔${reset}]\n' "$TEST_NAME"
                else
                  printf '  %s [${red}✖${reset}]\n' "$TEST_NAME"
                  printf '${red}✖ %s${reset}\n%s\n\n\n' "$TEST_NAME" "$TEST_OUTPUT" >> "$TEST_FAIL_LOG"
                fi
              ) &
            done
          ) | sort
          wait
          FAILURES=$(cat "$TEST_FAIL_LOG")
          if [ "$FAILURES" != "" ]; then
            printf '\n${red}Failing tests:${reset}\n\n%s' "$FAILURES"
            exit 1
          fi
        '';
      });
    };

    checks.x86_64-linux = lib.mapAttrs (name: testBody:
      pkgs.runCommand (lib.strings.sanitizeDerivationName name) {} ''
        export HOME=$(pwd)
        export LANG=
        ${normalizeTestBody testBody}
        touch $out
      ''
    ) testCases;
  };
}
