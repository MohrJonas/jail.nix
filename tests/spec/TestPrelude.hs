module TestPrelude
  ( (</>),
    TestM,
    buildNixDrvPath,
    evalNixExpr,
    forM,
    forM_,
    getTestDir,
    i,
    inTestM,
    liftIO,
    runNixDrv,
    shouldOutput,
    toNixString,
    void,
    withEnv,
    lifted,
  )
where

import Control.Monad
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, ask, asks, local, runReaderT)
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.List.Extra
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.String.Conversions
import Data.String.Interpolate (i)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process qualified as Process
import Test.Hspec
import Test.Hspec.Core.Spec qualified

data Context = Context
  { tmpDir :: FilePath,
    env :: Map.Map String String
  }

newtype TestM a = TestM {testMReaderT :: ReaderT Context IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Context
    )

lifted :: ((a -> IO b) -> IO c) -> (a -> TestM b) -> TestM c
lifted io action = do
  ctx <- ask
  liftIO $ io $ flip runReaderT ctx . testMReaderT . action

instance Example (TestM ()) where
  type Arg (TestM ()) = Context
  evaluateExample test _ hook _ = do
    hook $ runReaderT $ testMReaderT test
    pure $ Test.Hspec.Core.Spec.Result "" Test.Hspec.Core.Spec.Success

getTestDir :: TestM FilePath
getTestDir = asks tmpDir

inTestM :: SpecWith Context -> SpecWith ()
inTestM = aroundWith $ \action () -> do
  withSystemTempDirectory "jail-nix-test" $ \tmpDir -> do
    baseEnv <-
      Map.fromList
        <$> mapMaybeM
          (\(name, getVal) -> ((name,) <$>) <$> getVal)
          [ ("HOME", lookupEnv "HOME"),
            ("LANG", pure $ Just "en_US.UTF-8"),
            ("DBUS_SESSION_BUS_ADDRESS", lookupEnv "DBUS_SESSION_BUS_ADDRESS"),
            ("XDG_RUNTIME_DIR", lookupEnv "XDG_RUNTIME_DIR")
          ]
    action $
      Context
        { tmpDir,
          env = baseEnv
        }

toNixString :: String -> String
toNixString s =
  s
    & replace "\\" "\\\\"
    & replace "\"" "\\\""
    & replace "$" "\\$"
    & ("\"" <>)
    & (<> "\"")

withEnv :: String -> String -> TestM () -> TestM ()
withEnv name value = local $ \ctx -> ctx {env = Map.insert name value (env ctx)}

runNixDrv :: String -> TestM String
runNixDrv nixDrvExpr = do
  (exe :: String, drvPath :: FilePath) <-
    evalNixExpr
      [i| let d = #{nixDrvExpr}; in [(lib.getExe d) d.drvPath] |]
  void $ buildNixDrvPath drvPath
  env <- asks env
  let p = (Process.proc exe []) {Process.env = Just $ Map.toList env}
  liftIO $ Process.readCreateProcess p ""

buildNixDrvPath :: FilePath -> TestM FilePath
buildNixDrvPath drvPath = do
  liftIO $
    Process.readProcess
      "nix"
      [ "build",
        "--no-link",
        "--print-out-paths",
        drvPath <> "^*"
      ]
      ""

evalNixExpr :: (Aeson.FromJSON a) => String -> TestM a
evalNixExpr nixExpr = do
  tmpDir <- getTestDir
  liftIO $
    writeFile
      (tmpDir </> "test.nix")
      [i|
        let
          pkgs = import <nixpkgs> {};
          lib = pkgs.lib;
          sh = script: (pkgs.writeShellApplication { name = "test-script"; text = script; });
          jail-nix = import <jail-nix> {};
          jail = jail-nix.extend { inherit pkgs; };
        in #{nixExpr}
      |]
  liftIO $
    fromJust . Aeson.decode' . cs
      <$> Process.readProcess
        "nix"
        [ "eval",
          "--json",
          "--file",
          tmpDir </> "test.nix"
        ]
        ""

shouldOutput :: String -> String -> TestM ()
shouldOutput nixDrvExpr expectedOutput = do
  out <- runNixDrv nixDrvExpr
  liftIO $ out `shouldBe` expectedOutput
