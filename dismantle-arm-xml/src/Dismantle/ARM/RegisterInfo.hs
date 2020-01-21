{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Dismantle.ARM.RegisterInfo
  ( registerInfoParser
  , RegisterInfo(..)
  , RegisterDirection(..)
  , RegisterKind(..)
  , RegisterUsage(..)
  , RegisterIndexMode(..)
  ) where

import           Control.Applicative ( (<|>) )
import           Control.Monad ( void, foldM, unless, when )
import qualified Control.Monad.State as MS
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import           Data.Char ( toUpper, toLower )
import           Data.Void (Void)
import           Data.Maybe ( fromMaybe )

import           Debug.Trace
-- | Parsing the "explanation" section of the ARM xml specification. Each
-- section formally describes how logical operands are derived
-- from opcode fields (i.e. SIMD register Dm = Vm:M). However, the classification
-- of this operand is only present in an english-language description section.
-- This english has a (fairly) consistent structure that this parser exploits.
--
-- The parser 'registerInfoParser' derives a 'RegisterInfo' for a given logical
-- operand, when it determines that a section is describing an operand that
-- represents a register (either general-purpose or SIMD/vector).
--
-- Much of the content of the explanation is left uninterpreted, since it is ultimately
-- described formally in the ASL for the instruction. This parser
-- is meant to derive just enough information to assign the operand a type, so the
-- encoding will have a signature that can be matched with its ASL translation.
--
-- TODO: currently lists of registers are left completely uninterpreted.


data RegisterInfoParserError =
    IncompatibleRegisterIndexModes RegisterIndexMode RegisterIndexMode
  | IncompatibleRegisterUsages RegisterUsage RegisterUsage
  | IncompatibleRegisterKinds RegisterKind RegisterKind
  | IncompatibleRegisterDirections RegisterDirection RegisterDirection
  | IncompatibleRegisterEncodings String String
  | IncompatibleRegisterNames String String
  | NoRegisterKeyword IRegisterInfo
  | UnrecognizedTrailer IRegisterInfo String
  deriving (Ord, Eq, Show)


instance P.ShowErrorComponent RegisterInfoParserError where
  showErrorComponent = show


data RegisterDirection = Input | Output | InputOutput
  deriving (Ord, Eq, Show)

data RegisterKind = GPR | SIMDandFP
  deriving (Ord, Eq, Show)

-- | Currently not interpreted by dismantle. Classifies how a register is used.
data RegisterUsage = Data | Accumulator | Base | Index | Banked | NoUsage
  deriving (Ord, Eq, Show)

-- | How the register index is derived from the opcode fields
data RegisterIndexMode =
    IndexSimple
    -- ^ the source fields are a literal register index
  | IndexRegisterOffset String
    -- ^ this register is actually just the index of the given register (by name), plus one
    -- in this case we derive multiple logical operands from the same set of opcode fields
  | IndexVector
    -- ^ a special-case for indexing vector (SIMD) registers where the mapping is a non-trivial
    -- computation over the given fields
  | IndexMul2
    -- ^ the source fields are a literal register index, but multiplied by two (i.e. D:Vd = Qd * 2)
  deriving (Ord, Eq, Show)

data RegisterInfo a =
  RegisterInfo { regKind :: RegisterKind
               , regUsage :: RegisterUsage
               , regDirection :: RegisterDirection
               , regIndexMode :: RegisterIndexMode
               , regEncodedIn :: a
               , regSymbolName :: String
               , regSourceText :: String
               }
  deriving (Ord, Eq, Show)

-- | Intermediate register info during parsing
data IRegisterInfo =
  IRegisterInfo { iregKind :: Maybe RegisterKind
                , iregUsage :: Maybe RegisterUsage
                , iregDirection :: Maybe RegisterDirection
                , iregIndexMode :: Maybe RegisterIndexMode
                , iregEncodedIn :: Maybe String
                , iregSymbolName :: Maybe String
                , iregKeyword :: Bool
                  -- ^ tripped when the word "register" has been seen
                }
  deriving (Ord, Eq, Show)

noRegisterInfo :: IRegisterInfo
noRegisterInfo = IRegisterInfo Nothing Nothing Nothing Nothing Nothing Nothing False

mergeIRegisterInfos :: IRegisterInfo -> IRegisterInfo -> Either RegisterInfoParserError IRegisterInfo
mergeIRegisterInfos (IRegisterInfo mkind musage mdir mimode encin symnm rkw) (IRegisterInfo mkind' musage' mdir' mimode' encin' symnm' rkw') = do
  mkind'' <- collapseMaybes IncompatibleRegisterKinds mkind mkind'
  musage'' <- collapseMaybes IncompatibleRegisterUsages musage musage'
  mdir'' <- collapseMaybes IncompatibleRegisterDirections mdir mdir'
  mimode'' <- collapseMaybes IncompatibleRegisterIndexModes mimode mimode'
  encin'' <- collapseMaybes IncompatibleRegisterEncodings encin encin'
  symnm'' <- collapseMaybes IncompatibleRegisterNames symnm symnm'
  let rkw'' = rkw || rkw'
  return $ IRegisterInfo mkind'' musage'' mdir'' mimode'' encin'' symnm'' rkw''

type Parser = P.Parsec RegisterInfoParserError String

type RegisterInfoParser = MS.StateT IRegisterInfo Parser

-- | Parse an "explanation" section from the ARM xml. Returns 'Nothing' if the
-- section has an expected form that is known not to describe a register operand.
registerInfoParser :: String -> (Maybe String) -> Parser (Maybe (RegisterInfo String))
registerInfoParser symbolName encodedin = do
  P.optional $ encodingPreamble
  ws
  dropWords ["is", "an", "the", "optional", "a ", "specifies", "if present,", "arm"]
  ws
  (const Nothing <$> knownPrefixParser
    <|> Just <$> innerRegisterInfoParser symbolName encodedin)

-- FIXME: debugging
traceState :: String -> RegisterInfoParser ()
traceState msg = do
  st <- MS.lift P.getParserState
  irinfo <- MS.get
  traceM msg
  traceShowM st
  traceShowM irinfo


innerRegisterInfoParser :: String -> (Maybe String) -> Parser (RegisterInfo String)
innerRegisterInfoParser symbolName encodedin = do
  P.optional $ englishNumber
  ws
  P.optional $ bitSize
  ws
  P.optional $ P.chunk "name of the"
  ws
  P.optional $ englishNumber
  ws
  MS.evalStateT realRegInfoParser initInfo
 where
   initInfo = ni { iregEncodedIn = encodedin
                 , iregSymbolName = Just symbolName
                 }

   ni = noRegisterInfo

   ws' :: RegisterInfoParser ()
   ws' = MS.lift ws
   -- FIXME: lenses?
   parseUsage = liftParser (\x -> ni { iregUsage = x }) usageParser
   parseKind = liftParser (\x -> ni { iregKind = x }) kindParser
   parseMode = liftParser (fromMaybe ni) indexModeParser
   parseDirection = liftParser (\x -> ni { iregDirection = x }) directionParser
   parseEncoding = liftParser (\x -> ni { iregEncodedIn = x }) encodingParser

   -- The actual register info is assembled in a fairly arbitrary order, so this
   -- stateful parser collects key phrases that indicate pieces of the register info
   -- and assembles them into a final register info
   realRegInfoParser :: RegisterInfoParser (RegisterInfo String)
   realRegInfoParser = do
     sourceTxt <- MS.lift  P.getInput
     parseDirection
     ws'
     parseUsage
     ws'
     parseKind
     ws'
     registerKW
     ws'
     parseUsage
     ws'
     parseDirection
     ws'
     registerKW
     ws'
     registerContentInterpParser
     ws'
     parseEncoding
     ws'
     parseMode
     ws'
     trailerParser

     assembleRegisterInfo sourceTxt

-- | Information about how the register contents are interpreted. Unused currently.
registerContentInterpParser :: RegisterInfoParser ()
registerContentInterpParser = do
   void $ MS.lift $ P.optional $ alternatives $
     [ str "into which the status result of the store exclusive is written"
     , str "holding the lower 32 bits of the addend, and the destination register for the lower 32 bits of the result"
     , str "holding the upper 32 bits of the addend, and the destination register for the upper 32 bits of the result"
     , str "or {value{APSR_nzcv}} (encoded as {binarynumber{0b1111}})"
     , str "or APSR_nzcv (encoded as 0b1111)"
     , str "into which the status result of store exclusive is written"
     , str "containing an offset applied after the access"
     , str "holding address to be branched to"
     , str "holding the address to be branched to"
     , str "holding accumulate vector"
     , str "holding the accumulate vector"
     , str "holding indices"
     , str "holding the indices"
     , str "holding a shift amount in its bottom 8 bits"
     , str "holding lower 32 bits of addend"
     , str "holding upper 32 bits of addend"
     , str "holding addend"
     , str "holding the addend"
     , str "holding the scalar"
     , str "holding scalar"
     ]


-- | We need to ensure that all the input has been consumed by the various parsers, so we need to
-- recognize the trailer of the description.
trailerParser :: RegisterInfoParser ()
trailerParser = do
  irinfo <- MS.get
  sourceTxt <- P.getInput
  result <- MS.lift $ P.observing $ do
    alternatives $
      [ (P.char '.' >> ws >> P.eof)
      , (P.char ',' >> ws >> P.eof)
      , (void $ P.chunk ". The PC can be used")
      , (void $ P.chunk ". If the PC is used")
      , (void $ P.chunk ". For PC use see")
      , (void $ P.chunk ". If APSR_nzcv is used,")
      , (void $ P.chunk ". The value returned is")
      , (void $ P.chunk ". If <dt> is S32,")
      , (void $ P.chunk ". If writeback is not specified, the PC can be used.")
      , (void $ P.chunk ". If omitted, this register is the same as <Rn>.")
      , (void $ P.chunk ". Arm deprecates using the PC")
      , (void $ P.chunk ". It must be encoded with an identical value")
      , P.eof
      ]
  case result of
    Left err -> P.customFailure $ UnrecognizedTrailer irinfo sourceTxt
    Right _ -> return ()


registerKW :: RegisterInfoParser ()
registerKW = do
  kw <- MS.gets iregKeyword
  kw' <- MS.lift ((str "register" >> return True) <|> return False)
  MS.modify' $ \st -> st { iregKeyword = kw || kw' }


liftParser :: (Maybe a -> IRegisterInfo) -> Parser (Maybe a) -> RegisterInfoParser ()
liftParser f p = do
  st <- MS.get
  result <- f <$> MS.lift (P.try p)
  case mergeIRegisterInfos st result of
    Left err -> MS.lift $ P.customFailure err
    Right a -> MS.put a

assembleRegisterInfo :: String -> RegisterInfoParser (RegisterInfo String)
assembleRegisterInfo sourceTxt = do
  irinfo <- MS.get
  let
    kind = fromMaybe GPR $ iregKind irinfo
    direction = fromMaybe InputOutput $ iregDirection irinfo
    usage = fromMaybe NoUsage $ iregUsage irinfo
    mode = fromMaybe IndexSimple $ iregIndexMode irinfo
    rkw = iregKeyword irinfo
    symbolName = fromMaybe (error "Invalid empty symbol name") $ iregSymbolName irinfo
    encodedin = fromMaybe symbolName $ iregEncodedIn irinfo

  unless rkw $
    MS.lift $ P.customFailure $ NoRegisterKeyword irinfo

  return $ RegisterInfo kind usage direction mode encodedin symbolName sourceTxt


bitSize :: Parser String
bitSize = P.choice (map P.chunk bitSizeStrings)

bitSizeStrings :: [String]
bitSizeStrings =
  [ "3-bit", "4-bit", "5-bit", "6-bit", "8-bit", "12-bit", "16-bit", "24-bit", "32-bit", "64-bit", "128-bit" ]

dropWords :: [String] -> Parser ()
dropWords words =
  void $ P.optional $ P.many $
    (P.choice $ map (\w -> str w >> return ()) words) <|> (P.char ' ' >> return ())

encodingPreamble :: Parser ()
encodingPreamble = do
  P.chunk "For"
  P.many $ P.choice $ map P.chunk
    [ "half-precision"
    , "single-precision"
    , "double-precision"
    , "64-bit SIMD"
    , "128-bit SIMD"
    , "offset"
    , "or"
    , "the"
    , "vector"
    , "scalar"
    , "post-indexed"
    , "pre-indexed"
    , "encoding"
    , "variant"
    , "variants"
    , " "
    ]

  P.takeWhileP Nothing (/= ':')
  P.char ':'
  return ()

alternatives :: [Parser a] -> Parser a
alternatives ps = P.choice (map P.try ps)

ws :: Parser ()
ws = P.takeWhileP Nothing (== ' ') >> return ()

englishNumber :: Parser Integer
englishNumber =
  P.choice
    [ str "first" >> return 1
    , str "second" >> return 2
    , str "third" >> return 3
    , str "fourth" >> return 4
    , str "fifth" >> return 5
    ]

str :: String -> Parser String
str string@(s : str) = P.try $ do
  (P.char s <|> P.char (toUpper s) <|> P.char (toLower s))
  P.chunk str
  return string
str "" = P.chunk ""

kindParser :: Parser (Maybe RegisterKind)
kindParser =
  P.optional $ alternatives
    [ str "general-purpose" >> return GPR
    , str "SIMD&FP" >> return SIMDandFP
    ]

usageParser :: Parser (Maybe RegisterUsage)
usageParser =
  P.optional $ alternatives
    [ str "data" >> return Data
    , str "accumulator" >> return Accumulator
    , str "base" >> return Base
    , str "index" >> return Index
    , str "banked" >> return Banked
    ]

directionParser :: Parser (Maybe RegisterDirection)
directionParser = do
  P.optional $ alternatives $
      [ P.chunk "destination and source" >> return InputOutput
      -- TODO: "to or from" is (usually) concretized per-encoding based on additional information
      -- sitting elsewhere in the XML
      , P.chunk "that " >> innerIdent >> P.chunk " will be transferred to or from" >> return InputOutput
      , P.chunk "that is transferred into" >> return Output
      , P.chunk "destination and second source" >> return InputOutput
      , P.chunk "to be transferred to or from" >> return InputOutput
      , P.chunk "to be transferred" >> return Output
      , P.chunk "source register and the destination" >> return InputOutput
      , P.chunk "source and destination" >> return InputOutput
      , P.chunk "to be loaded" >> return Input
      , P.chunk "to be stored" >> return Output
      , P.chunk "to be accessed" >> return Input
      , str "destination" >> return Output
      , P.chunk "arm source" >> return Input
      , P.chunk "source" >> return Input
      , P.chunk "input" >> return Input
      , P.chunk "output" >> return Output
      ]
  where
    innerIdent :: Parser ()
    innerIdent = void $ alternatives $
      [ bracketed ('{','}') >> P.char '}' >> bracketed ('[',']')
      , bracketed ('{','}') >> P.char '}' >> return ""
      , bracketed ('<','>') >> bracketed ('[',']')
      , bracketed ('{','}')
      , bracketed ('<','>')
      ]

bracketed :: (Char, Char) -> Parser String
bracketed (lbrak, rbrak) = do
  P.char lbrak
  nm <- P.takeWhile1P Nothing (/= rbrak)
  P.char rbrak
  return $ nm

encodingParser :: Parser (Maybe String)
encodingParser = P.optional $ alternatives $
  [ encodedIn
  , bracketedField
  ]

encodedIn :: Parser String
encodedIn = do
  str ", encoded in the "
  nm <- bracketed ('"','"')
  str " field"
  P.notFollowedBy (P.chunk " when")
  return $ nm



bracketedField :: Parser String
bracketedField = do
  str "(field "
  nm <- bracketed ('"','"')
  str ")"
  return $ nm


indexModeParser :: Parser (Maybe IRegisterInfo)
indexModeParser = P.optional $ alternatives
  [ multwoIndexModeParser
  , encodedInConditionally
  , vectorIndexModeParser
  , oneOffsetIndexModeParser
  ]

multwoIndexModeParser :: Parser IRegisterInfo
multwoIndexModeParser = do
  str "as "
  nm <- bracketed ('<','>')
  str "*2"
  return $ noRegisterInfo { iregIndexMode = Just IndexMul2, iregSymbolName = Just nm  }

oneOffsetIndexModeParser :: Parser IRegisterInfo
oneOffsetIndexModeParser = do
  P.chunk ". This is the next SIMD&FP register after "
  nm <- bracketed ('<','>')
  return $ noRegisterInfo { iregIndexMode = Just $ IndexRegisterOffset nm }
  <|> do
  P.choice
    [ P.chunk ". <Rt2> must be <R(t+1)>."
    , P.chunk ". This register must be <R(t+1)>."
    ]
  return $ noRegisterInfo { iregIndexMode = Just $ IndexRegisterOffset "Rt", iregSymbolName = Just "Rt2" }


-- TODO: This is still a bit weird
vectorIndexModeParser :: Parser IRegisterInfo
vectorIndexModeParser = do
  P.chunk ". If <dt> is "
  P.takeWhileP Nothing (/= ',')
  P.chunk ", Dm is restricted to D0-D7. Dm is encoded in \"Vm<2:0>\", and x is encoded in \"M:Vm<3>\". If <dt> is "
  P.takeWhileP Nothing (/= ',')
  P.chunk ", Dm is restricted to D0-D15. Dm is encoded in \"Vm\", and x is encoded in \"M\"."
  return $ noRegisterInfo { iregIndexMode = Just IndexVector, iregEncodedIn = Just "size:M:Vm" }


-- FIXME: this is not quite right, but there are inconsistencies in the XML
encodedInConditionally :: Parser IRegisterInfo
encodedInConditionally = do
  P.chunk ", encoded in the \"Vm<2:0>\" field when <dt> is S16, otherwise the \"Vm\" field."
  return $ noRegisterInfo { iregIndexMode = Just IndexSimple, iregEncodedIn = Just "Vm<2:0>" }

collapseMaybes :: Eq a => (a -> a -> b) -> Maybe a -> Maybe a -> Either b (Maybe a)
collapseMaybes err ma1 ma2 = case (ma1, ma2) of
  (Just a1, Just a2) -> if a1 == a2 then Right (Just a1) else Left $ err a1 a2
  (Nothing, _) -> Right ma2
  (_, Nothing) -> Right ma1


-- | Parser that only succeeds when the input has a known form indicating
-- that this text is not describing a register operand
knownPrefixParser :: Parser ()
knownPrefixParser = P.try $
  (void $ P.choice (map str knownPrefixes))
  <|> literalPrefixParser

literalPrefixParser :: Parser ()
literalPrefixParser = do
  P.optional $ englishNumber
  ws
  P.optional $ bitSize
  ws
  P.optional $ P.chunk "unsigned"
  ws
  void $ P.chunk "immediate"


knownPrefixes :: [String]
knownPrefixes =
  [ "see {"
  , "offset is added"
  , "type of shift applied"
  , "type of shift to be applied"
  , "constant of specified type"
  , "immediate value"
  , "rotation to be applied"
  ]
  ++
  [ "shift amount", "one of:", "label of", "bit number", "width of the"
  , "shift to apply", "number of the", "destination vector for a doubleword operation"
  , "sequence of one or more of the following", "When <size> ==", "data type"
  , "constant of the specified type"
  , "location of the extracted result in the concatenation of the operands"
  , "alignment", "suffix"
  , "see Standard assembler syntax fields"
  , "base register writeback"
  , "limitation on the barrier operation"
  , "stack pointer"
  , "index register is added to or subtracted from the base register"
  , "index register is added to the base register"
  , "offset is added to the base register"
  , "positive unsigned immediate byte offset"
  , "program label"
  , "condition"
  , "size of the"
  ]
  ++
  [ "least significant", "number of bits", "rotate amount", "bit position"
  , "optional shift amount", "address adjusted", "optional suffix"
  , "mode whose banked sp", "rotation applied to elements"
  , "element index"
  , "immediate offset", "number of fraction bits in"
  , "signed floating-point constant", "data size", "scalar"
  , "endianness to be selected", "number of mode to change to"
  , "sequence of one or more of following, specifying which interrupt mask bits are affected"
  , "unsigned immediate"
  -- fixme: these seem unused in arch32?
  , "system register encoding space"
  , "opc1 parameter within the System register encoding space"
  , "opc2 parameter within the System register encoding space"
  , "CRm parameter within the System register encoding space"
  , "CRn parameter within the System register encoding space"
  -- fixme: vector instructions
  , "vectors containing the table"
  , "destination vector for a quadword operation"
  , "list of one or more registers"
  , "list of consecutively numbered 32-bit SIMD&FP registers to be transferred"
  , "list of consecutively numbered 64-bit SIMD&FP registers to be transferred"
  , "list of two or more registers to be loaded"
  , "list of two or more registers to be stored"
  , "list containing the 64-bit names of SIMD&FP registers"
  , "list containing the 64-bit names of the SIMD&FP registers"
  , "list containing the 64-bit names of two SIMD&FP registers"
  , "list containing the 64-bit names of three SIMD&FP registers"
  , "list containing the 64-bit names of four SIMD&FP registers"
  , "list containing the single 64-bit name of the SIMD&FP register holding the element"
  , "list containing the 64-bit names of the two SIMD&FP registers holding the element"
  , "list containing the 64-bit names of the three SIMD&FP registers holding the element"
  , "list containing the 64-bit names of the four SIMD&FP registers holding the element"
  -- fixme: special system register accesses are unintepreted
  , "special register to be accessed"
  ]
