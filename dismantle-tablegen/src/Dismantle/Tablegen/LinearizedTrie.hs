{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MagicHash #-}
module Dismantle.Tablegen.LinearizedTrie (
  LinearizedTrie(..),
  -- * Serialization
  serialize,
  deserialize,
  -- * Unsafe
  unsafeFromAddr,
  unsafeLinearizedTrieParseTableBytes,
  unsafeLinearizedTriePayloads
  ) where

import           Control.Monad ( replicateM, when )
import qualified Control.Monad.Fail as MF
import qualified Data.Binary as DB
import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Digest.Pure.SHA as DPS
import qualified Data.Foldable as F
import           Data.Int ( Int32 )
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import           Data.Word ( Word8 )
import qualified GHC.ForeignPtr as GFP
import qualified GHC.Prim as GPr
import qualified GHC.Ptr as GPtr
import qualified System.IO.Unsafe as SIU


-- | A data type mapping sequences of bytes to elements of type @a@
data LinearizedTrie a =
  LinearizedTrie { ltPayloads :: V.Vector a
                 -- ^ Payloads of the parser; looking up a byte sequence in the trie
                 -- will yield an element in this vector.
                 --
                 -- Note that the element at index 0 is undefined and unused, as the
                 -- indices into the payloads table from the parse tables start at 1
                 -- (since those values are stored as negatives).  The item at index 1
                 -- is the "default" element returned when nothing else matches.
                 , ltParseTables :: SV.Vector Int32
                 -- ^ The parse tables are linearized into an array of Int32.  To look
                 -- up a byte, add the byte to the 'ltStartIndex' and use the result
                 -- to index into 'ltParseTables'.  If the result negative, it is a
                 -- (negated) index into 'ltPayloads'.  Otherwise, it is the next
                 -- 'ltStartIndex' to use.
                 , ltStartIndex :: {-# UNPACK #-} !Int
                 -- ^ The table index to start traversing from.
                 }
  deriving (Functor)


-- | Serialize the contents of a 'LinearizedTrie' to a 'LBS.ByteString'
--
-- Beyond flattening the structure to a bytestring, this function includes a
-- hash of all of the inputs used to generate the trie that is checked when
-- loading the trie from disk.
--
-- The intent is that this function, along with the deserializer, can allow
-- parse tables to be computed offline and checked into the repository to speed
-- build times for client code.
serialize :: (a -> BP.Put)
          -- ^ A function to serialize the payload type
          -> [LBS.ByteString]
          -- ^ The inputs used to construct the trie (to be hashed for consistency checks)
          -> LinearizedTrie a
          -> LBS.ByteString
serialize putA inputs lt = BP.runPut $ do
  DB.put (computeHash inputs)

  BP.putInt64le (fromIntegral (V.length (ltPayloads lt)))
  F.mapM_ putA (ltPayloads lt)
  BP.putInt64le (fromIntegral (SV.length (ltParseTables lt)))
  SV.mapM_ BP.putInt32le (ltParseTables lt)
  BP.putInt64le (fromIntegral (ltStartIndex lt))

computeHash :: [LBS.ByteString] -> DPS.Digest DPS.SHA1State
computeHash inputs =
  DPS.completeSha1Incremental (F.foldl' BG.pushChunks DPS.sha1Incremental inputs) (fromIntegral len)
  where
    len = sum (fmap LBS.length inputs)

deserialize :: (BG.Get a)
            -> DPS.Digest DPS.SHA1State
            -> LBS.ByteString
            -> Either String (LinearizedTrie a)
deserialize getA expectedHash bytes =
  case BG.runGetOrFail getTrie bytes of
    Left (_, _, msg) -> Left msg
    Right (_, _, lt) -> Right lt

  where
    getTrie = do
      serializedHash <- DB.get
      when (serializedHash /= expectedHash) $ do
        MF.fail "Hash mismatch while deserializing LinearizedTrie"
      nElts <- BG.getInt64le
      elts <- replicateM (fromIntegral nElts) getA
      nTblElts <- BG.getInt64le
      tblElts <- replicateM (fromIntegral nTblElts) BG.getInt32le
      startIdx <- BG.getInt64le
      return LinearizedTrie { ltPayloads = V.fromList elts
                            , ltParseTables = SV.fromList tblElts
                            , ltStartIndex = fromIntegral startIdx
                            }

-- Unsafe things

-- | This constructor is designed for use in Template Haskell-generated code so
-- that the parsing tables can be encoded as an 'Addr#' and turned into a
-- 'LinearizedTrie' in constant time.
--
-- It is suggested that this is only used with values generated from a
-- safely-constructed 'LinearizedTrie'
unsafeFromAddr :: [a]
               -- ^ The payloads of the 'LinearizedTrie'.  Note that this list only
               -- contains the *defined* values.  There is an implicit undefined
               -- value stored at index 0.
               -> GPr.Addr#
               -- ^ The linearized parsing tables (probably stored in the read-only data section)
               -> Int
               -- ^ The number of 'Int32' entries in the parsing tables
               -> Int
               -- ^ The index to start parsing with
               -> LinearizedTrie a
unsafeFromAddr payloads addr nElts ix0 = SIU.unsafePerformIO $ do
  fp <- GFP.newForeignPtr_ (GPtr.Ptr addr)
  return $! LinearizedTrie { ltPayloads = V.fromList (undefined : payloads)
                           , ltParseTables = SV.unsafeFromForeignPtr0 fp nElts
                           , ltStartIndex = ix0
                           }
{-# NOINLINE unsafeFromAddr #-}

-- | Extract the parse tables of a 'LinearizedTrie' as a list of 'Word8' values
-- suitable for embedding in TH as an 'Addr#'
--
-- The first 'Int' is the number of 'Int32' entries in the table.
--
-- The second 'Int' is the starting index (i.e., the index to start using the
-- parse tables from)
unsafeLinearizedTrieParseTableBytes :: LinearizedTrie a -> ([Word8], Int, Int)
unsafeLinearizedTrieParseTableBytes lt =
  (LBS.unpack (BP.runPut (SV.mapM_ BP.putInt32host tbls)), SV.length tbls, ltStartIndex lt)
  where
    tbls = ltParseTables lt

-- | Extract the payloads from a 'LinearizedTrie'
--
-- The list will only contain the values in the real payloads table starting at
-- index 1, as index 0 is undefined and unused.
unsafeLinearizedTriePayloads :: LinearizedTrie a -> [a]
unsafeLinearizedTriePayloads lt =
  case V.null (ltPayloads lt) of
    True -> []
    False -> tail (V.toList (ltPayloads lt))
