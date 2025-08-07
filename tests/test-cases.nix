{ pkgs, lib, test, jail-nix }: with test; let
  sh = script: (pkgs.writeShellApplication { name = "test-script"; text = script; });
  defaultPath = "${pkgs.coreutils}/bin";
  jail = jail-nix.lib.extend { inherit pkgs; };
in {
  "simple jail works" = assertStdout
    (jail "hello" pkgs.hello [])
    "Hello, world!";

  "it allows overriding base permissions shared across all jails" = let
    jail' = jail-nix.lib.extend {
      inherit pkgs;
      basePermissions = c: [
        (c.readonly "/build/secret")
        (c.readonly "/nix/store")
      ];
    };
   in [
     "echo hunter2 > /build/secret"
     (assertStdout (jail' "hello" (sh "cat /build/secret") []) "hunter2")
   ];

  "it sets a sane default path" = assertStdout
    (jail "test" (sh ''echo "$PATH"'') [])
    defaultPath;

  "it does not forward environment variables by default" = [
    "export MY_ENV_VAR=secret"
    (assertStdout
      (jail "test" (sh ''echo "''${MY_ENV_VAR-unset}"'') [])
      "unset"
    )
  ];

  "combinators/add-cleanup" = [
    (assertStdout
      (jail "test" (sh "cat /foo-in-jail") (c: [
        (c.ro-bind "foo" "/foo-in-jail")
        (c.add-runtime "echo hello > foo")
        (c.add-cleanup "rm foo")
      ]))
      "hello"
    )
    (assertFileDoesNotExist "foo")
  ];

  "combinators/add-path" = assertStdout
    (jail "test" (sh ''echo "$PATH"'') (c: [
      (c.add-path "/some/new/path")
    ]))
    "${defaultPath}:/some/new/path";

  "combinators/add-pkg-deps" = assertStdout
    (jail "test" (sh ''echo "$PATH"'') (c: [
      (c.add-pkg-deps [ pkgs.hello ])
    ]))
    "${defaultPath}:${pkgs.hello}/bin";

  "combinators/bind-pkg" = assertStdout
    (jail "test" (sh "cat /foo") (c: [
      (c.bind-pkg "/foo" (pkgs.writeText "foo" "bar"))
    ]))
    "bar";

  "combinators/fwd-env" = [
    "export MY_ENV_VAR=secret"
    (assertStdout
      (jail "test" (sh ''echo "''${MY_ENV_VAR-unset}"'') (c: [
        (c.fwd-env "MY_ENV_VAR")
      ]))
      "secret"
    )
  ];

  "combinators/readonly-from-path-var" = let
    separator = " ";
    mkTestPath = path: "mkdir -p /build/${path} && touch /build/${path}/file";
  in [
    (mkTestPath "some/path")
    (mkTestPath "some/other/path")
    (mkTestPath "some/not/in/var")
    "export TEST_PATHS=${lib.escapeShellArg (lib.concatStringsSep separator [
      "some/path"
      "some/non/existant/path"
      "some/other/path"
      "some/other/non/existant/path"
    ])}"
    (assertStdout
      (jail "test" (sh "${lib.getExe pkgs.tree} /build/some") (c: [
        (c.readonly-paths-from-var "TEST_PATHS" separator)
      ]))
      (lib.trim ''
        /build/some
        |-- other
        |   `-- path
        |       `-- file
        `-- path
            `-- file

        4 directories, 2 files
      '')
    )
  ];

  "combinators/readonly-from-path-var doesn't require the var to be set" =
    jail "test" (sh "true") (c: [
      (c.readonly-paths-from-var "THIS_VAR_IS_NOT_SET" ":")
    ]);

  "combinators/set-argv" = assertStdout
    (jail "test" (sh "printf '1=%s,2=%s,3=%s' \"$@\"") (c: [
      (c.set-argv [ "foo" "bar baz" "foo>'bar'" ])
    ]))
    "1=foo,2=bar baz,3=foo>'bar'";

  "combinators/set-env" = assertStdout
    (jail "test" (sh ''echo "''${MY_ENV_VAR-unset}"'') (c: [
      (c.set-env "MY_ENV_VAR" "some-value")
    ]))
    "some-value";
}
