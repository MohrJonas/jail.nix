module DBusCombinatorsSpec (spec) where

import Control.Concurrent (modifyMVar_, newMVar, readMVar)
import Control.Exception (bracket)
import DBus qualified
import DBus.Client qualified as DBus
import Test.Hspec
import TestPrelude

spec :: Spec
spec = inTestM $ do
  let mockBusName = "foo.bar"
  let mockMethod =
        MockDBusMethod
          { objPath = "/foo/bar/baz",
            ifaceName = "foo.bar.Iface",
            methodName = "Demo"
          }

  describe "unsafe-dbus" $ do
    it "gives unfiltered access to DBus method calls" $ do
      (sendDBusOutput, methodCalls) <- withDBusClient mockBusName $ \client -> do
        withMockDBusMethodHandler client mockMethod "some return value" $ do
          runNixDrv
            [i|
              jail "send-dbus" (#{callDBusMethodNixDrv mockBusName mockMethod "some arg"}) (c: [
                c.unsafe-dbus
              ])
            |]
      liftIO $ do
        sendDBusOutput `shouldBe` "   some return value"
        methodCalls `shouldBe` ["some arg"]

    it "gives unfiltered access to DBus signals" $ do
      (_, signals) <- withDBusClient mockBusName $ \client -> do
        withMockDBusSignalHandler client mockMethod $ do
          runNixDrv
            [i|
              jail "send-dbus" (#{sendDBusSignalNixDrv mockBusName mockMethod "some message"}) (c: [
                c.unsafe-dbus
              ])
            |]
      liftIO $ signals `shouldBe` ["some message"]

  describe "dbus" $ do
    describe "sending signals from within the jail" $ do
      it "filters all signals if there are no rules" $ do
        (_, signals) <- withDBusClient mockBusName $ \client -> do
          withMockDBusSignalHandler client mockMethod $ do
            runNixDrv
              [i|
                jail "send-dbus" (#{sendDBusSignalNixDrv mockBusName mockMethod "some arg"}) (c: [
                  (c.dbus {})
                ])
              |]
        liftIO $ signals `shouldBe` []

    describe "calling methods from within the jail" $ do
      it "allows method calls that match the call filter rules" $ do
        (sendDBusOutput, methodCalls) <- withDBusClient mockBusName $ \client -> do
          withMockDBusMethodHandler client mockMethod "some return value" $ do
            runNixDrv
              [i|
                jail "send-dbus" (#{callDBusMethodNixDrv mockBusName mockMethod "some arg"}) (c: [
                  (c.dbus {
                    call = [ #{toNixString $ toProxyRule mockBusName mockMethod} ];
                  })
                ])
              |]
        liftIO $ do
          sendDBusOutput `shouldBe` "   some return value"
          methodCalls `shouldBe` ["some arg"]

      it "allows method calls if the receiving bus has `own` privileges" $ do
        (sendDBusOutput, methodCalls) <- withDBusClient mockBusName $ \client -> do
          withMockDBusMethodHandler client mockMethod "some return value" $ do
            runNixDrv
              [i|
                jail "send-dbus" (#{callDBusMethodNixDrv mockBusName mockMethod "some arg"}) (c: [
                  (c.dbus {
                    own = [ #{toNixString $ DBus.formatBusName mockBusName} ];
                  })
                ])
              |]
        liftIO $ do
          sendDBusOutput `shouldBe` "   some return value"
          methodCalls `shouldBe` ["some arg"]

      it "allows method calls if the receiving bus has `talk` privileges" $ do
        (sendDBusOutput, methodCalls) <- withDBusClient mockBusName $ \client -> do
          withMockDBusMethodHandler client mockMethod "some return value" $ do
            runNixDrv
              [i|
                jail "send-dbus" (#{callDBusMethodNixDrv mockBusName mockMethod "some arg"}) (c: [
                  (c.dbus {
                    talk = [ #{toNixString $ DBus.formatBusName mockBusName} ];
                  })
                ])
              |]
        liftIO $ do
          sendDBusOutput `shouldBe` "   some return value"
          methodCalls `shouldBe` ["some arg"]

      it "filters method calls if the receiving bus only has `see` privileges" $ do
        (sendDBusOutput, methodCalls) <- withDBusClient mockBusName $ \client -> do
          withMockDBusMethodHandler client mockMethod "some return value" $ do
            runNixDrv
              [i|
                jail "send-dbus" (#{callDBusMethodNixDrv mockBusName mockMethod "some arg"}) (c: [
                  (c.dbus {
                    see = [ #{toNixString $ DBus.formatBusName mockBusName} ];
                  })
                ])
              |]
        liftIO $ do
          sendDBusOutput `shouldBe` "Failed to call dbus method\n"
          methodCalls `shouldBe` []

      it "filters all method calls if there are no rules" $ do
        (sendDBusOutput, methodCalls) <- withDBusClient mockBusName $ \client -> do
          withMockDBusMethodHandler client mockMethod "some return value" $ do
            runNixDrv
              [i|
                jail "send-dbus" (#{callDBusMethodNixDrv mockBusName mockMethod "some arg"}) (c: [
                  (c.dbus { })
                ])
              |]
        liftIO $ do
          sendDBusOutput `shouldBe` "Failed to call dbus method\n"
          methodCalls `shouldBe` []

      it "filters method calls if the rules do not match" $ do
        (sendDBusOutput, methodCalls) <- withDBusClient mockBusName $ \client -> do
          withMockDBusMethodHandler client mockMethod "some return value" $ do
            let otherMockMethod = mockMethod {objPath = "/some/unrelated/object/path"}
            runNixDrv
              [i|
                jail "send-dbus" (#{callDBusMethodNixDrv mockBusName mockMethod "some arg"}) (c: [
                  (c.dbus {
                    call = [ #{toNixString $ toProxyRule mockBusName otherMockMethod} ];
                  })
                ])
              |]
        liftIO $ do
          sendDBusOutput `shouldBe` "Failed to call dbus method\n"
          methodCalls `shouldBe` []

-- * DBus test helpers

withDBusClient :: DBus.BusName -> (DBus.Client -> TestM a) -> TestM a
withDBusClient busName action = do
  let setup = do
        client <- liftIO DBus.connectSession
        DBus.requestName client busName [DBus.nameDoNotQueue] >>= \case
          DBus.NamePrimaryOwner -> pure client
          reply -> error $ "Got unexpected requestName reply: " <> show reply
  lifted (bracket setup DBus.disconnect) action

data MockDBusMethod = MockDBusMethod
  { objPath :: DBus.ObjectPath,
    ifaceName :: DBus.InterfaceName,
    methodName :: DBus.MemberName
  }

-- | Returns a RULE that can be passed into `--call` or `--broadcast` that
-- matches the given MockDBusMethod. See XDG-DBUS-PROXY(1).
toProxyRule :: DBus.BusName -> MockDBusMethod -> String
toProxyRule busName mockMethod =
  DBus.formatBusName busName
    <> "="
    <> DBus.formatInterfaceName (ifaceName mockMethod)
    <> "."
    <> DBus.formatMemberName (methodName mockMethod)
    <> "@"
    <> DBus.formatObjectPath (objPath mockMethod)

withMockDBusMethodHandler :: DBus.Client -> MockDBusMethod -> String -> TestM a -> TestM (a, [String])
withMockDBusMethodHandler client (MockDBusMethod {objPath, ifaceName, methodName}) returnValue action = do
  recordedMethodCalls <- liftIO $ newMVar []
  let mockMethod :: String -> IO String
      mockMethod arg = do
        modifyMVar_ recordedMethodCalls (pure . (<> [arg]))
        pure returnValue
  liftIO
    $ DBus.export
      client
      objPath
    $ DBus.defaultInterface
      { DBus.interfaceName = ifaceName,
        DBus.interfaceMethods = [DBus.autoMethod methodName mockMethod]
      }
  (,) <$> action <*> liftIO (readMVar recordedMethodCalls)

withMockDBusSignalHandler :: DBus.Client -> MockDBusMethod -> TestM a -> TestM (a, [String])
withMockDBusSignalHandler client (MockDBusMethod {objPath}) action = do
  recordedSignals <- liftIO $ newMVar []
  let onSignal sig = do
        let body = case DBus.fromVariant <$> DBus.signalBody sig of
              [Just s] -> s
              _ -> error $ "Expected string signal body, got: " <> show (DBus.signalBody sig)
        modifyMVar_ recordedSignals (pure . (<> [body]))
  let matchRule = DBus.matchAny {DBus.matchPath = Just objPath}
  void $ liftIO $ DBus.addMatch client matchRule onSignal
  (,) <$> action <*> liftIO (readMVar recordedSignals)

-- | Returns a nix expression that calls the specified (String -> String) DBus method
callDBusMethodNixDrv :: DBus.BusName -> MockDBusMethod -> String -> String
callDBusMethodNixDrv busName (MockDBusMethod {objPath, ifaceName, methodName}) arg =
  [i|
    sh ''
      if ! \\
        ${pkgs.dbus}/bin/dbus-send \\
          --type=method_call \\
          --print-reply=literal \\
          --dest=#{toNixShellArg $ DBus.formatBusName busName} \\
          #{toNixShellArg $ DBus.formatObjectPath objPath} \\
          #{toNixShellArg $ DBus.formatInterfaceName ifaceName}.#{toNixShellArg $ DBus.formatMemberName methodName} \\
          string:#{toNixShellArg arg} \\
      ; then
        echo "Failed to call dbus method"
      fi
    ''
  |]
  where
    toNixShellArg :: String -> String
    toNixShellArg s = "${lib.escapeShellArg " <> toNixString s <> "}"

sendDBusSignalNixDrv :: DBus.BusName -> MockDBusMethod -> String -> String
sendDBusSignalNixDrv busName (MockDBusMethod {objPath, ifaceName}) msg = do
  [i|
    sh ''
      if ! \\
        ${pkgs.dbus}/bin/dbus-send \\
          --type=signal \\
          --dest=#{toNixShellArg $ DBus.formatBusName busName} \\
          #{toNixShellArg $ DBus.formatObjectPath objPath} \\
          #{toNixShellArg $ DBus.formatInterfaceName ifaceName} \\
          string:#{toNixShellArg msg} \\
      ; then
        echo "Failed to call dbus method"
      fi
    ''
  |]
  where
    toNixShellArg :: String -> String
    toNixShellArg s = "${lib.escapeShellArg " <> toNixString s <> "}"
