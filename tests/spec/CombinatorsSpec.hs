module CombinatorsSpec (spec) where

import Data.List.Extra
import Data.String.Interpolate.Util
import System.Directory.Extra (createDirectoryIfMissing, doesFileExist)
import Test.Hspec
import TestPrelude

spec :: Spec
spec = parallel $ inTestM $ do
  describe "add-cleanup" $ do
    it "runs arbitrary logic after the jail exits" $ do
      tmpDir <- getTestDir
      [i|
        jail "test" (sh "cat /foo-in-jail") (c: [
          (c.ro-bind "foo" "/foo-in-jail")
          (c.add-runtime "echo hello > foo")
          (c.add-cleanup "rm foo")
        ])
      |]
        `shouldOutput` "hello\n"
      liftIO $ doesFileExist (tmpDir </> "foo") `shouldReturn` False

  describe "add-path" $ do
    it "prepends the passed path to $PATH" $ do
      output <-
        runNixDrv
          [i|
            jail "test" (sh ''echo "$PATH"'') (c: [
              (c.add-path "/some/new/path")
            ])
          |]
      liftIO $ output `shouldStartWith` "/some/new/path:"

  describe "add-pkg-deps" $ do
    it "sets the path" $ do
      helloPath <- evalNixExpr "toString pkgs.hello"
      output <-
        runNixDrv
          [i|
            jail "test" (sh ''echo "$PATH"'') (c: [
              (c.add-pkg-deps [ pkgs.hello ])
            ])
          |]
      liftIO $ output `shouldStartWith` helloPath <> "/bin:"

  describe "bind-pkg" $ do
    it "binds the passed nix package at the specified location" $ do
      [i|
        jail "test" (sh "cat /foo") (c: [
          (c.bind-pkg "/foo" (pkgs.writeText "foo" "bar\n"))
        ])
      |]
        `shouldOutput` "bar\n"

  describe "fwd-env" $ do
    it "forwards the passed environment variable" $ do
      withEnv "MY_ENV_VAR" "secret" $ do
        [i|
          jail "test" (sh ''echo "''${MY_ENV_VAR-unset}"'') (c: [
            (c.fwd-env "MY_ENV_VAR")
          ])
        |]
          `shouldOutput` "secret\n"

  describe "readonly-from-path-var" $ do
    forM_ [":", " "] $ \separator ->
      describe ("with separator \"" <> separator <> "\"") $ do
        it "binds all paths in the specified environment variable as read only" $ do
          tmpDir <- getTestDir
          let separator = " "
          let mkTestPath path = liftIO $ createDirectoryIfMissing True (tmpDir </> path) >> writeFile (tmpDir </> path </> "file") ""
          mkTestPath "some/path"
          mkTestPath "some/other/path"
          mkTestPath "some/not/in/var"
          let testPaths =
                intercalate
                  separator
                  $ map
                    (tmpDir </>)
                    [ "some/path",
                      "some/non/existant/path",
                      "some/other/path",
                      "some/other/non/existant/path"
                    ]
          withEnv "TEST_PATHS" testPaths $ do
            [i|
              jail "test" (sh "${lib.getExe pkgs.tree} ${#{toNixString (tmpDir </> "some")}}") (c: [
                (c.readonly-paths-from-var "TEST_PATHS" #{toNixString separator})
              ])
            |]
              `shouldOutput` unindent
                [i|
                  #{tmpDir </> "some"}
                  |-- other
                  |   `-- path
                  |       `-- file
                  `-- path
                      `-- file

                  4 directories, 2 files
                |]

        it "doesn't require the var to be set" $ do
          void $
            runNixDrv
              [i|
                jail "test" (sh "true") (c: [
                  (c.readonly-paths-from-var "THIS_VAR_IS_NOT_SET" #{toNixString separator})
                ])
              |]

  describe "set-argv" $ do
    it "overrides argv passed to the jailed program" $ do
      [i|
        jail "test" (sh "printf '1=%s,2=%s,3=%s' \\"$@\\"") (c: [
          (c.set-argv [ "foo" "bar baz" "foo>'bar'" ])
        ])
      |]
        `shouldOutput` "1=foo,2=bar baz,3=foo>'bar'"

  describe "set-env" $ do
    it "allows setting arbitrary environment variables" $ do
      [i|
        jail "test" (sh ''echo -n "''${MY_ENV_VAR-unset}"'') (c: [
          (c.set-env "MY_ENV_VAR" "some-value")
        ])
      |]
        `shouldOutput` "some-value"
