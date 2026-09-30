module NOM.Derivation (
  Derivation (..),
  DerivationOutput (..),
  parseDerivation,
  parseDerivationText,
  outputPath,
) where

import Data.Attoparsec.Text qualified as AT
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Relude

-- | A single output of a Nix derivation.
--
-- Nix serialises derivations on disk in ATerm format, e.g.
--
-- > Derive([("out","/nix/store/...","","")], [...], [...], ...)
--
-- For input-addressed derivations every output has a known store path.
-- For floating content-addressed derivations (experimental @ca-derivations@),
-- deferred outputs and impure derivations (experimental @impure-derivations@)
-- the path is empty (@""@):
--
-- > Derive([("out","","r:sha256","")], [...], [...], ...)
--
-- See @src/libstore/derivations.cc:parseDerivationOutput@ in the Nix source
-- and https://github.com/maralorn/nix-output-monitor/issues/167.
data DerivationOutput = DerivationOutput
  { path :: Maybe FilePath
  -- ^ Known output path, if any. 'Nothing' for floating CA, deferred and
  -- impure outputs.
  , hashAlgo :: Text
  , hash :: Text
  }
  deriving stock (Show, Eq, Ord)

-- | Minimal view of a Nix derivation. Only the fields NOM needs.
data Derivation = Derivation
  { outputs :: Map Text DerivationOutput
  , inputDrvs :: Map FilePath (Set Text)
  , inputSrcs :: Set FilePath
  , platform :: Text
  , builder :: Text
  , args :: [Text]
  , env :: Map Text Text
  }
  deriving stock (Show, Eq)

-- | Project the known output path, if any.
outputPath :: DerivationOutput -> Maybe FilePath
outputPath output = output.path

-- | Parse a derivation from strict 'Text'. Accepts both @Derive(...)@ and
-- @DrvWithVersion("xp-dyn-drv", ...)@ (experimental @dynamic-derivations@).
parseDerivationText :: Text -> Either String Derivation
parseDerivationText = AT.parseOnly (parseDerivation <* AT.endOfInput)

-- | Attoparsec parser for the ATerm format (strict 'Text' version).
parseDerivation :: AT.Parser Derivation
parseDerivation = do
  void parseHeader
  outputs <- mapOf parseOutputEntry
  void (AT.char ',')
  inputDrvs <- mapOf parseInputDrvEntry
  void (AT.char ',')
  inputSrcs <- Set.fromList . map toString <$> listOf textParser
  void (AT.char ',')
  platform <- textParser
  void (AT.char ',')
  builder <- textParser
  void (AT.char ',')
  args <- listOf textParser
  void (AT.char ',')
  env <- mapOf parseEnvEntry
  void (AT.char ')')
  pure Derivation{outputs, inputDrvs, inputSrcs, platform, builder, args, env}
 where
  parseHeader :: AT.Parser ()
  parseHeader =
    void (AT.string "Derive(")
      <|> void (AT.string "DrvWithVersion(" *> textParser *> AT.char ',')

  parseOutputEntry :: AT.Parser (Text, DerivationOutput)
  parseOutputEntry = do
    void (AT.char '(')
    key <- textParser
    void (AT.char ',')
    pathText <- textParser
    void (AT.char ',')
    hashAlgo <- textParser
    void (AT.char ',')
    hash <- textParser
    void (AT.char ')')
    let path = if Text.null pathText then Nothing else Just (toString pathText)
    pure (key, DerivationOutput{path, hashAlgo, hash})

  parseInputDrvEntry :: AT.Parser (FilePath, Set Text)
  parseInputDrvEntry = do
    void (AT.char '(')
    key <- toString <$> textParser
    void (AT.char ',')
    value <- parseNodeOutputs
    void (AT.char ')')
    pure (key, value)

  parseEnvEntry :: AT.Parser (Text, Text)
  parseEnvEntry = do
    void (AT.char '(')
    key <- textParser
    void (AT.char ',')
    value <- textParser
    void (AT.char ')')
    pure (key, value)

-- | Parse the set of output names an input derivation is used by.
--
-- Old (stable) form is a plain list: @["out", "bin"]@.
-- New (experimental @dynamic-derivations@) form is a pair of a shallow set
-- and a map of nested uses: @(["out"], [("foo", ["bar"])])@.
-- We flatten everything into a single set, which is a safe over-approximation
-- for dependency tracking.
parseNodeOutputs :: AT.Parser (Set Text)
parseNodeOutputs =
  Set.fromList <$> listOf textParser
    <|> do
      void (AT.char '(')
      shallow <- Set.fromList <$> listOf textParser
      void (AT.char ',')
      childMap <- mapOf parseChildEntry
      void (AT.char ')')
      let childKeys = Set.fromList (Map.keys childMap)
          childValues = mconcat (Map.elems childMap)
      pure (shallow <> childKeys <> childValues)
 where
  parseChildEntry :: AT.Parser (Text, Set Text)
  parseChildEntry = do
    void (AT.char '(')
    key <- textParser
    void (AT.char ',')
    value <- parseNodeOutputs
    void (AT.char ')')
    pure (key, value)

-- | Parse a @\"..."@ string with @\\n@, @\\r@, @\\t@, @\\\\@ and @\\"@
-- escapes, matching Nix\'s ATerm printer.
textParser :: AT.Parser Text
textParser = do
  void (AT.char '"')
  chunks <- go
  pure (Text.concat chunks)
 where
  go :: AT.Parser [Text]
  go = do
    prefix <- AT.takeWhile (\c -> c /= '"' && c /= '\\')
    next <- AT.anyChar
    case next of
      '"' -> pure [prefix]
      '\\' -> do
        escaped <- AT.anyChar
        let decoded = case escaped of
              'n' -> '\n'
              'r' -> '\r'
              't' -> '\t'
              c -> c
        rest <- go
        pure (prefix : Text.singleton decoded : rest)
      _ -> pure [prefix]

listOf :: AT.Parser a -> AT.Parser [a]
listOf element = do
  void (AT.char '[')
  AT.sepBy element (AT.char ',') <* AT.char ']'

mapOf :: (Ord k) => AT.Parser (k, v) -> AT.Parser (Map k v)
mapOf entry = Map.fromList <$> listOf entry
