module Dismantle.ARM.RegisterInfo
  ( registerInfoParser
  , RegisterInfo(..)
  , RegisterDirection(..)
  , RegisterKind(..)
  , RegisterUsage(..)
  , RegisterIndexMode(..)
  ) where

import           Control.Applicative ( (<|>) )
import           Control.Monad ( void )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import           Data.Void (Void)
import           Data.Maybe ( fromMaybe )

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
  deriving (Ord, Eq, Show)

type Parser = P.Parsec RegisterInfoParserError String

data RegisterDirection = Input | Output | InputOutput
  deriving (Ord, Eq, Show)

data RegisterKind = GPR | SIMDandFP
  deriving (Ord, Eq, Show)

-- | Currently not interpreted by dismantle. Classifies how a register is used.
data RegisterUsage = Data | Accumulator | Base | Index | NoUsage
  deriving (Ord, Eq, Show)

-- | How the register index is derived from the opcode fields
data RegisterIndexMode =
    IndexFromXML
    -- ^ the fields (specified in the XML) should be interepreted as a literal index
  | IndexImplicit
    -- ^ this register index is derived from a single field with the same name
  | IndexExplicit String
    -- ^ the encoding is explicit in the explanation (e.g. "encoded in <Rd>")
  | IndexRegisterOffset String
    -- ^ this register is actually just the index of the given register (by name), plus one
    -- in this case we derive multiple logical operands from the same set of opcode fields
  | IndexVector [String]
    -- ^ a special-case for indexing vector (SIMD) registers where the mapping is a non-trivial
    -- computation over the given fields
  deriving (Ord, Eq, Show)

data RegisterInfo =
  RegisterInfo { regKind :: RegisterKind
               , regUsage :: RegisterUsage
               , regDirection :: RegisterDirection
               , regIndexMode :: RegisterIndexMode
               , regSourceText :: String
               }
  deriving (Ord, Eq, Show)



-- | Parse an "explanation" section from the ARM xml. Returns 'Nothing' if the
-- section has an expected form that is known not to describe a register operand.
registerInfoParser :: Parser (Maybe RegisterInfo)
registerInfoParser = do
  P.optional $ encodingPreamble
  ws
  dropWords ["Is", "is", "An", "an", "The", "the", "optional", "a ", "Specifies", "If present,"]
  ws
  (const Nothing <$> knownPrefixParser <|> Just <$> innerRegisterInfoParser)


innerRegisterInfoParser :: Parser (RegisterInfo)
innerRegisterInfoParser = do
  sourceTxt <- P.stateInput <$> P.getParserState
  P.optional $ englishNumber
  ws
  P.optional $ bitSize
  ws
  P.optional $ P.chunk "name of the"
  ws
  P.optional $ englishNumber
  ws
  (rkind, impliedDirection, mrmode) <- kindParser
  ws
  rusage <- usageParser
  ws
  (rdir, mrmode') <- directionParser impliedDirection
  ws
  mrmode'' <- combineIndexModes mrmode mrmode'
  indexMode <- indexModeParser mrmode''
  return (RegisterInfo rkind rusage rdir indexMode sourceTxt)

bitSize :: Parser String
bitSize = P.choice (map P.chunk bitSizeStrings)

bitSizeStrings :: [String]
bitSizeStrings =
  [ "3-bit", "4-bit", "5-bit", "6-bit", "8-bit", "12-bit", "16-bit", "24-bit", "32-bit", "64-bit", "128-bit" ]

dropWords :: [String] -> Parser ()
dropWords words =
  void $ P.optional $ P.many $
    (P.choice $ map (\w -> P.chunk w >> return ()) words) <|> (P.char ' ' >> return ())

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


ws :: Parser ()
ws = P.takeWhileP Nothing (== ' ') >> return ()

englishNumber :: Parser Integer
englishNumber =
  P.choice
    [ P.chunk "first" >> return 1
    , P.chunk "second" >> return 2
    , P.chunk "third" >> return 3
    , P.chunk "fourth" >> return 4
    , P.chunk "fifth" >> return 5
    ]

kindParser :: Parser (RegisterKind, Maybe RegisterDirection, Maybe RegisterIndexMode)
kindParser =
  P.choice
    [ P.chunk "general-purpose" >> return (GPR, Nothing, Nothing)
    , P.chunk "Arm source" >> return (GPR, Just Input, Just $ IndexImplicit)
    , P.chunk "source general-purpose" >> return (GPR, Just Input, Just $ IndexImplicit)
    , P.chunk "destination general-purpose" >> return (GPR, Just Output, Just $ IndexImplicit)
    , P.chunk "SIMD&FP" >> return (SIMDandFP, Nothing, Nothing)
    ]

usageParser :: Parser RegisterUsage
usageParser =
  P.choice
    [ P.chunk "data" >> return Data
    , P.chunk "accumulator" >> return Accumulator
    , P.chunk "base" >> return Base
    , P.chunk "index" >> return Index
    , return NoUsage
    ]


directionParser :: Maybe RegisterDirection -> Parser (RegisterDirection, Maybe RegisterIndexMode)
directionParser (Just impliedDirection) = P.chunk "register" >> return (impliedDirection, Nothing)
directionParser Nothing = do
  (directionWithName >>= \(dir, nm) -> return (dir, Just (IndexExplicit nm)))
  <|> (direction >>= \dir -> return (dir, Nothing))
  where
    directionWithName :: Parser (RegisterDirection, String)
    directionWithName = do
      P.chunk "register <"
      nm <- P.takeWhile1P Nothing (/= '>')
      P.chunk "> to be "
      direction <- (P.chunk "loaded" >> return Input) <|> (P.chunk "stored" >> return Output)
      return (direction, nm)

    direction :: Parser RegisterDirection
    direction =
      P.choice
          [ P.chunk "destination register" >> return Output
          , P.chunk "register to be transferred" >> return Input
          , P.chunk "source register" >> return Input
          , P.chunk "input register" >> return Input
          , P.chunk "output register" >> return Output
          , P.chunk "register holding address to be branched to" >> return Input
          , P.chunk "register to be accessed" >> return Input
          , P.chunk "register into which the status result of store exclusive is written" >> return Output
          , P.chunk "destination and source register" >> return InputOutput
          , P.chunk "destination and second source register" >> return InputOutput
          , P.chunk "source and destination register" >> return InputOutput
          , P.chunk "register" >> return InputOutput
          ]

combineIndexModes :: Maybe RegisterIndexMode -> Maybe RegisterIndexMode -> Parser (Maybe RegisterIndexMode)
combineIndexModes mode mode' = case (mode, mode') of
  (Just i, Just i') ->
    if i /= i' then
      P.customFailure $ IncompatibleRegisterIndexModes i i'
    else return mode
  (Nothing, _) -> return mode'
  (_, Nothing) -> return mode


indexModeParser :: Maybe RegisterIndexMode -> Parser RegisterIndexMode
indexModeParser mrindex = do
  P.takeWhileP Nothing (/= '.')
  mrindex' <- P.optional $ vectorIndexModeParser <|> oneOffsetIndexModeParser
  fromMaybe IndexFromXML <$> combineIndexModes mrindex mrindex'

oneOffsetIndexModeParser :: Parser RegisterIndexMode
oneOffsetIndexModeParser = do
  P.chunk ". This is the next SIMD&FP register after <"
  nm <- P.takeWhile1P Nothing (/= '>')
  return $ IndexRegisterOffset nm
  <|> do
  P.choice
    [ P.chunk ". <Rt2> must be <R(t+1)>."
    , P.chunk ". This register must be <R(t+1)>."
    ]
  return $ IndexRegisterOffset "Rt"

vectorIndexModeParser :: Parser RegisterIndexMode
vectorIndexModeParser = do
  P.chunk ". If <dt> is "
  P.takeWhileP Nothing (/= ',')
  P.chunk ", Dm is restricted to D0-D7. Dm is encoded in \"Vm<2:0>\", and x is encoded in \"M:Vm<3>\". If <dt> is "
  P.takeWhileP Nothing (/= ',')
  P.chunk ", Dm is restricted to D0-D15. Dm is encoded in \"Vm\", and x is encoded in \"M\"."
  return $ IndexVector ["size", "M", "Vm"]


-- | Parser that only succeeds when the input has a known form indicating
-- that this text is not describing a register operand
knownPrefixParser :: Parser ()
knownPrefixParser = P.try $
  (void $ P.choice (map (\s -> P.chunk s) knownPrefixes))
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
  [ "shift amount", "one of:", "label of", "bit number", "width of the"
  , "shift to apply", "number of the", "destination vector for a doubleword operation"
  , "sequence of one or more of the following", "When <size> ==", "data type"
  , "constant of the specified type"
  , "location of the extracted result in the concatenation of the operands"
  , "alignment", "suffix"
  , "See Standard assembler syntax fields"
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
  , "list containing the 64-bit names of the SIMD&FP registers"
  , "list containing the 64-bit names of two SIMD&FP registers"
  , "list containing the 64-bit names of three SIMD&FP registers"
  , "list containing the 64-bit names of four SIMD&FP registers"
  , "list containing the single 64-bit name of the SIMD&FP register holding the element"
  , "list containing the 64-bit names of the two SIMD&FP registers holding the element"
  , "list containing the 64-bit names of the three SIMD&FP registers holding the element"
  , "list containing the 64-bit names of the four SIMD&FP registers holding the element"
  ]
