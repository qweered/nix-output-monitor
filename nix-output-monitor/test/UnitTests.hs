import Control.Monad.Trans.Writer.CPS (runWriterT)
import Data.ByteString.Char8 qualified as ByteString
import Data.Map.Strict qualified as Map
import Data.Set (singleton)
import Data.Set qualified as Set
import Data.Strict qualified as Strict
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Encoding
import Data.Time (UTCTime (..))
import Data.Time.Calendar (Day (..))
import NOM.Builds
import NOM.Derivation qualified as NomDrv
import NOM.Error (NOMError (..))
import NOM.NixMessage.OldStyle (NixOldStyleMessage (..))
import NOM.Parser
import NOM.State (DerivationId, EvalInfo (..), NOMState (..), ProgressState (..), getDerivationId, getDerivationInfos)
import NOM.State.CacheId.Set qualified as CSet
import NOM.Update (insertDerivation, lookupDerivation)
import NOM.Update.Monad
import Optics (view)
import NOM.Util (parseOne)
import Relude
import Relude.Unsafe qualified as Unsafe
import System.IO.Error qualified as IOError
import Test.HUnit hiding (State)

assertOldStyleParse :: ByteString -> IO (ByteString, NixOldStyleMessage)
assertOldStyleParse input = do
  let res = parseOne parser input
  assertBool "parsing succeeds" (isJust res)
  let (t, res') = Unsafe.fromJust res
  assertBool "parsing succeeds with an actual match" (isJust res')
  pure (t, Unsafe.fromJust res')

{- | Pure stub for the 'UpdateMonad' constraints, allowing tests to drive
'insertDerivation' and 'lookupDerivation' without touching the real Nix
store: derivation file reads answer from the environment.
-}
newtype TestM a = TestM (ReaderT (Either NOMError NomDrv.Derivation) (State NOMState) a)
  deriving newtype (Functor, Applicative, Monad, MonadState NOMState, MonadReader (Either NOMError NomDrv.Derivation))

runTestM :: Either NOMError NomDrv.Derivation -> TestM a -> State NOMState a
runTestM stub (TestM action) = runReaderT action stub

instance MonadNow TestM where
  getNow = pure 0
  getUTC = pure (UTCTime (ModifiedJulianDay 0) 0)

instance MonadReadDerivation TestM where
  getDerivation _ = ask

instance MonadCacheBuildReports TestM where
  getCachedBuildReports = pure mempty
  updateBuildReports updateFunc = pure (updateFunc mempty)

instance MonadCheckStorePath TestM where
  subscribeStorePath _ _ = pure ()
  foundStorePaths = pure []

-- | Derivation file contents for tests, parameterised over derivation inputs.
testDerivation :: Map FilePath (Set Text) -> NomDrv.Derivation
testDerivation deps =
  NomDrv.Derivation
    { NomDrv.outputs = mempty
    , NomDrv.inputDrvs = deps
    , NomDrv.inputSrcs = mempty
    , NomDrv.platform = ""
    , NomDrv.builder = ""
    , NomDrv.args = mempty
    , NomDrv.env = mempty
    }

emptyTestState :: NOMState
emptyTestState =
  MkNOMState
    { derivationInfos = mempty
    , storePathInfos = mempty
    , fullSummary = mempty
    , forestRoots = mempty
    , buildReports = mempty
    , startTime = 0
    , progressState = JustStarted
    , storePathIds = mempty
    , derivationIds = mempty
    , touchedIds = mempty
    , activities = mempty
    , nixErrors = mempty
    , nixTraces = mempty
    , buildPlatform = Strict.Nothing
    , interestingActivities = mempty
    , evaluationState = MkEvalInfo{count = 0, at = 0, lastFileName = Strict.Nothing}
    }

-- | Register a derivation (with the given file contents) and return its id.
insertTestDerivation :: NomDrv.Derivation -> Derivation -> TestM DerivationId
insertTestDerivation parsed drv = do
  drvId <- getDerivationId drv
  void (runWriterT (insertDerivation parsed drvId))
  pure drvId

-- | The stub answer for successful derivation reads.
okStub :: Either NOMError NomDrv.Derivation
okStub = Right (testDerivation mempty)

evalTest :: Either NOMError NomDrv.Derivation -> TestM a -> a
evalTest stub action = evalState (runTestM stub action) emptyTestState

execTest :: Either NOMError NomDrv.Derivation -> TestM a -> NOMState
execTest stub action = execState (runTestM stub action) emptyTestState

runTestOn :: NOMState -> TestM a -> (a, NOMState)
runTestOn testState action = runState (runTestM okStub action) testState

-- | Roots of the dependency forest in a finished test state.
rootsOf :: NOMState -> [DerivationId]
rootsOf testState = CSet.toList testState.forestRoots

rootDrv :: Derivation
rootDrv = Derivation (StorePath "cccccccccccccccccccccccccccccccc" "root")

parentDrv :: Derivation
parentDrv = Derivation (StorePath "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" "parent")

childDrvPath :: FilePath
childDrvPath = "/nix/store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-child.drv"

utf8DrvText :: Text
utf8DrvText = "Derive([(\"out\",\"/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo\",\"\",\"\")],[],[],\"x86_64-linux\",\"/bin/bash\",[],[(\"PS1\",\"─ \")])"

utf8RawBytes :: ByteString
utf8RawBytes = Encoding.encodeUtf8 "Derive([(\"out\",\"/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo\",\"\",\"\")],[],[],\"x86_64-linux\",\"/bin/bash\",[],[(\"X\",\"a" <> ByteString.singleton '\xff' <> "b\")])"

main :: IO ()
main = do
  counts <-
    runTestTT
      $ test
        [ "Parse Plan" ~: do
            (rest, result) <-
              assertOldStyleParse
                "these derivations will be built:\n  /nix/store/7n05q79qhrgvnfmvv2v3cnj3yqf4d1hf-haskell-language-server-0.4.0.0.drv\nthese paths will be fetched (134.19 MiB download, 1863.82 MiB unpacked):\n  /nix/store/60zb5dndaw1fzir3s69sy3xhy19gll1p-ghc-8.8.2\ngarbage"
            assertEqual
              "result matches"
              ( PlanBuilds
                  ( singleton
                      ( Derivation
                          $ StorePath
                            "7n05q79qhrgvnfmvv2v3cnj3yqf4d1hf"
                            "haskell-language-server-0.4.0.0"
                      )
                  )
                  ( Derivation
                      $ StorePath
                        "7n05q79qhrgvnfmvv2v3cnj3yqf4d1hf"
                        "haskell-language-server-0.4.0.0"
                  )
              )
              result
            (rest2, result2) <- assertOldStyleParse rest
            assertEqual
              "result matches"
              ( PlanDownloads
                  (134.19 * 1024 ** 2)
                  (1863.82 * 1024 ** 2)
                  (singleton (StorePath "60zb5dndaw1fzir3s69sy3xhy19gll1p" "ghc-8.8.2"))
              )
              result2
            assertEqual "rest is okay" "garbage" rest2
        , "Parse Downloading" ~: do
            (rest, result) <-
              assertOldStyleParse
                "copying path '/nix/store/yk1164s4bkj6p3s4mzxm5fc4qn38cnmf-ghc-8.8.2-doc' from 'https://cache.nixos.org'...\n"
            assertEqual
              "result matches"
              ( Downloading
                  (StorePath "yk1164s4bkj6p3s4mzxm5fc4qn38cnmf" "ghc-8.8.2-doc")
                  (Host (Just "https") Nothing "cache.nixos.org")
              )
              result
            assertEqual "no rest" "" rest
        , "Parse local building" ~: do
            (rest, result) <-
              assertOldStyleParse
                "building '/nix/store/dpqlnrbvzhjxp06d1mc3ksf2w8m2ldms-aeson-1.5.2.0.drv'...\n"
            assertEqual
              "result matches"
              ( Build
                  (Derivation $ StorePath "dpqlnrbvzhjxp06d1mc3ksf2w8m2ldms" "aeson-1.5.2.0")
                  Localhost
              )
              result
            assertEqual "no rest" "" rest
        , "Parse remote building" ~: do
            (rest, result) <-
              assertOldStyleParse
                "building '/nix/store/63jjdifv1x1nymjxdwla603xy1sggakk-hoogle-local-0.1.drv' on 'ssh://maralorn@example.com'...\n"
            assertEqual
              "result matches"
              ( Build
                  (Derivation $ StorePath "63jjdifv1x1nymjxdwla603xy1sggakk" "hoogle-local-0.1")
                  (Host (Just "ssh") (Just "maralorn") "example.com")
              )
              result
            assertEqual "no rest" "" rest
        , "Parse failed build" ~: do
            (rest, result) <-
              assertOldStyleParse
                "builder for '/nix/store/fbpdwqrfwr18nn504kb5jqx7s06l1mar-regex-base-0.94.0.1.drv' failed with exit code 1\n"
            assertEqual
              "result matches"
              (Failed (Derivation $ StorePath "fbpdwqrfwr18nn504kb5jqx7s06l1mar" "regex-base-0.94.0.1") (ExitCode 1))
              result
            assertEqual "no rest" "" rest
        , "Parse failed build for nix 2.4" ~: do
            (rest, result) <-
              assertOldStyleParse
                "error: builder for '/nix/store/dylih0mw8yisn6nrjc3qlf51knmdkrq1-local-build-3.drv' failed with exit code 1;\n"
            assertEqual
              "result matches"
              (Failed (Derivation $ StorePath "dylih0mw8yisn6nrjc3qlf51knmdkrq1" "local-build-3") (ExitCode 1))
              result
            assertEqual "no rest" "" rest
        , "Parse failed build for nix >= 2.29" ~: do
            (_, result) <-
              assertOldStyleParse
                $ ByteString.unlines
                  [ "error: Cannot build '/nix/store/d055cqki6z1vll144kvj496cknwvwi44-build-fail.drv'."
                  , "       Reason: builder failed with exit code 1."
                  , "       Output paths:"
                  , "          /nix/store/pvf324ikpfb9nhyszmc4zz5g9y8by0f6-build-fail"
                  ]
            assertEqual
              "result matches"
              (Failed (Derivation $ StorePath "d055cqki6z1vll144kvj496cknwvwi44" "build-fail") (ExitCode 1))
              result
        , "Forest roots deduplicate reinserted derivations" ~: do
            let expected = evalTest okStub (getDerivationId rootDrv)
                finalRoots = rootsOf (execTest okStub (insertTestDerivation (testDerivation mempty) rootDrv >> insertTestDerivation (testDerivation mempty) rootDrv))
            assertEqual "reinserting a root keeps a single entry" [expected] finalRoots
        , "Forest roots keep only the topmost derivation" ~: do
            let parentWithChild = testDerivation (Map.singleton childDrvPath (singleton "out"))
                expected = evalTest okStub (getDerivationId parentDrv)
                finalRoots = rootsOf (execTest okStub (insertTestDerivation parentWithChild parentDrv))
            assertEqual "the child is rewired below its parent" [expected] finalRoots
        , "Parse input-addressed derivation" ~: do
            assertEqual
              "result matches"
              ( Right
                  NomDrv.Derivation
                    { NomDrv.outputs = Map.singleton "out" (NomDrv.DerivationOutput (Just "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo") "" "")
                    , NomDrv.inputDrvs = mempty
                    , NomDrv.inputSrcs = mempty
                    , NomDrv.platform = "x86_64-linux"
                    , NomDrv.builder = "/bin/bash"
                    , NomDrv.args = mempty
                    , NomDrv.env = mempty
                    }
              )
              (NomDrv.parseDerivationText "Derive([(\"out\",\"/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo\",\"\",\"\")],[],[],\"x86_64-linux\",\"/bin/bash\",[],[])")
        , "Parse floating CA derivation" ~: do
            assertEqual
              "empty path parses to Nothing"
              ( Right
                  NomDrv.Derivation
                    { NomDrv.outputs = Map.singleton "out" (NomDrv.DerivationOutput Nothing "r:sha256" "")
                    , NomDrv.inputDrvs = mempty
                    , NomDrv.inputSrcs = mempty
                    , NomDrv.platform = "x86_64-linux"
                    , NomDrv.builder = "/bin/bash"
                    , NomDrv.args = mempty
                    , NomDrv.env = mempty
                    }
              )
              (NomDrv.parseDerivationText "Derive([(\"out\",\"\",\"r:sha256\",\"\")],[],[],\"x86_64-linux\",\"/bin/bash\",[],[])")
        , "Parse impure derivation" ~: do
            Right parsed <- pure (NomDrv.parseDerivationText "Derive([(\"out\",\"\",\"r:sha256\",\"impure\")],[],[],\"x86_64-linux\",\"/bin/bash\",[],[])")
            assertEqual "impure outputs have no known path" Nothing (NomDrv.outputPath =<< Map.lookup "out" parsed.outputs)
        , "Parse DrvWithVersion with dynamic inputDrvs" ~: do
            assertEqual
              "nested uses flatten into a single set"
              ( Right
                  NomDrv.Derivation
                    { NomDrv.outputs = Map.singleton "out" (NomDrv.DerivationOutput (Just "/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-top") "" "")
                    , NomDrv.inputDrvs = Map.singleton "/nix/store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-dep.drv" (Set.fromList ["bar", "foo", "out"])
                    , NomDrv.inputSrcs = mempty
                    , NomDrv.platform = "x86_64-linux"
                    , NomDrv.builder = "/bin/bash"
                    , NomDrv.args = mempty
                    , NomDrv.env = mempty
                    }
              )
              (NomDrv.parseDerivationText "DrvWithVersion(\"xp-dyn-drv\",[(\"out\",\"/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-top\",\"\",\"\")],[(\"/nix/store/bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb-dep.drv\",([\"out\"],[(\"foo\",[\"bar\"])]))],[],\"x86_64-linux\",\"/bin/bash\",[],[])")
        , "Parse derivation escapes" ~: do
            Right parsed <- pure (NomDrv.parseDerivationText "Derive([(\"out\",\"/nix/store/aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-foo\",\"\",\"\")],[],[],\"x86_64-linux\",\"/bin/bash\",[],[(\"GREETING\",\"hello\\nworld\")])")
            assertEqual "escapes decode" (Map.singleton "GREETING" "hello\nworld") parsed.env
        , "Parse daemon store host as local" ~: do
            assertEqual "empty host is local" Localhost (parseHost "")
            assertEqual "daemon store is local" Localhost (parseHost "daemon")
            assertEqual
              "remote hosts keep parsing"
              (Host (Just "ssh-ng") Nothing "example.com")
              (parseHost "ssh-ng://example.com")
        , "Missing remote derivations become quiet leaves" ~: do
            let enoent = IOError.mkIOError IOError.doesNotExistErrorType "openFile" Nothing Nothing
                remoteDrv = Derivation (StorePath "dddddddddddddddddddddddddddddddd" "remote")
                missingStub = Left (DerivationReadError enoent)
                both = runWriterT (lookupDerivation remoteDrv >> lookupDerivation remoteDrv)
                (remoteId, logged) = evalTest missingStub both
                finalState = execTest missingStub both
                (cachedFlag, _) = runTestOn finalState (view #cached <$> getDerivationInfos remoteId)
            assertEqual "no errors are emitted" [] logged
            assertEqual "the missing derivation is cached" True cachedFlag
            assertEqual "the missing derivation is a root leaf" [remoteId] (rootsOf finalState)
        , "Parse derivation with UTF-8 content" ~: do
            Right parsed <- pure (NomDrv.parseDerivationText (decodeUtf8With lenientDecode (Encoding.encodeUtf8 utf8DrvText)))
            assertEqual "non-ASCII content survives" (Map.singleton "PS1" "─ ") parsed.env
        , "Parse derivation with invalid bytes leniently" ~: do
            Right parsed <- pure (NomDrv.parseDerivationText (decodeUtf8With lenientDecode utf8RawBytes))
            assertEqual "invalid bytes become replacement chars" (Map.singleton "X" ("a" <> Text.singleton '\xFFFD' <> "b")) parsed.env
        ]
  if errors counts + failures counts == 0 then exitSuccess else exitFailure
