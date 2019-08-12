{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module WordIndexed ( wordIndexedTests ) where

import Control.Exception
import GHC.Exception ( ErrorCall(..) )
import           Data.Bits
import           Data.Monoid ( (<>) )
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Text.PrettyPrint.HughesPJClass as PP

import           Data.Word.Indexed
import qualified Data.Parameterized.NatRepr as NR

wordIndexedTests :: IO TestTree
wordIndexedTests =
    return $ testGroup "Data.Word.Indexed" $
    let r = NR.knownNat :: NR.NatRepr 5
        tstOrd a b ex = compare a b == ex @?
                        "compare " <> show a <> " to " <> show b <>
                        " got " <> (show $ compare a b) <> ", expected " <> show ex
        tstNEq a b = (a /= b) @?
                        "unexpected equality of " <> show a <> " against " <> show b
        tstOverflow v = catch (evaluate (fromInteger v :: W 5) >>=
                              \wv -> "expected overflow exception" @=? "success with " <> show wv)
                              (\(ErrorCallWithLocation e _l) ->
                                   "Value " <> show v <> " too large for Word.Indexed of size 5" @=? e)
    in
    [ let wi = (w 65535 :: W 5) in testCase "w constructor" $ do
                 31 @=? unW wi
                 5  @=? width wi
                 "(31 :: W 5)" @=? show wi  -- diagnostic
                 "31" @=? (show $ PP.pPrint wi)  -- used for disassembly (pPrint)

    , let wi = (wRep r 65535) in testCase "wRep constructor" $ do
                 31 @=? unW wi
                 5  @=? width wi
                 "(31 :: W 5)" @=? show wi
                 "31" @=? (show $ PP.pPrint wi)

    , let wi = (fromInteger 31 :: W 5) in testCase "fromInteger constructor" $ do
                 31 @=? unW wi
                 5  @=? width wi
                 "(31 :: W 5)" @=? show wi
                 "31" @=? (show $ PP.pPrint wi)

    , testCase "fromInteger constructor, overflow" $ tstOverflow 65535

    , let wi = (fromInteger 31 :: W 5) in testCase "fromInteger constructor, smaller overflow" $ do
                 31 @=? unW wi
                 5  @=? width wi
                 "(31 :: W 5)" @=? show wi
                 "31" @=? (show $ PP.pPrint wi)

    , testCase "fromInteger constructor, negative overflow" $ tstOverflow (-65535)

    , let wi = (w (-65535) :: W 5) in testCase "w constructor, negative overflow" $ do
                 -- n.b.  65535 is 0b111111111111111., the 2's complement (invert and add 1) is
                 -- 0b000000000000001, masked as 0b00001 or 1.
                 1  @=? unW wi
                 5  @=? width wi
                 "(1 :: W 5)" @=? show wi
                 "1" @=? (show $ PP.pPrint wi)

    , testCase "fromInteger constructor, small negative" $ tstOverflow (-4) -- because how big is -4 :: Integer?

    , let wi = (w (-4) :: W 5) in testCase "fromInteger constructor, negative" $ do
                 -- n.b. 4 is 0b11111100, the 2's complement (invert and
                 -- add 1) is 0b11111100, which masked and interpreted
                 -- as unsigned is 0b11100 or 28.
                 28 @=? unW wi
                 5  @=? width wi
                 "(28 :: W 5)" @=? show wi
                 "28" @=? (show $ PP.pPrint wi)

    , let wi1 = w 65535 :: W 5
          wi2 = w 65535 :: W 4
          wi3 = w 32767 :: W 4
          wi4 = w 30 :: W 5
          wi5 = w 0 :: W 5
      in testCase "Eq comparisons" $ do
        -- n.b. can only compare W instances with the same width,
        -- (e.g. wi1 == wi2 is a compile error).  Note that in
        -- comparing two Num instances, a fromInteger will be
        -- performed to convert from one to the other before
        -- comparison (invoking instance (W n) fromInteger).
        wi2 @=? wi3
        wi3 @=? wi2
        tstNEq wi1 wi4
        tstNEq wi4 wi1
        tstNEq wi1 wi5
        tstNEq wi5 wi1
        tstNEq wi4 wi5
        tstNEq wi5 wi4
        wi1 @=? wi1
        wi2 @=? wi2
        wi5 @=? wi5

        catch (evaluate (wi1 == 65530) >>=
               \_ -> "expected overflow exception" @=? "success with wi1 == 65530")
              (\(ErrorCallWithLocation e _l) -> "Value 65530 too large for Word.Indexed of size 5" @=? e)
        tstNEq wi1 (w 65530 :: W 5)

        wi1 @=? (w 65535 :: W 5)
        wi1 @=? (w 32767 :: W 5)
        catch (evaluate (65530 == wi1) >>=
               \_ -> "expected overflow exception" @=? "success with 65530 == wi1")
              (\(ErrorCallWithLocation e _l) -> "Value 65530 too large for Word.Indexed of size 5" @=? e)
        tstNEq (w 65530 :: W 5) wi1
        (w 65535 :: W 5) @=? wi1
        (w 32767 :: W 5) @=? wi1

        catch (evaluate (wi2 == 31) >>=
               \_ -> "expected overflow exception" @=? "success with wi2 == 31")
              (\(ErrorCallWithLocation e _l) -> "Value 31 too large for Word.Indexed of size 4" @=? e)

    , let wi1 = w 65535 :: W 5
          wi2 = w 65535 :: W 4
          wi3 = w 32767 :: W 4
          wi4 = w 30 :: W 5
          wi5 = w 0 :: W 5
      in testCase "Ord comparisons" $ do
        -- n.b. can only compare W instances with the same width,
        -- (e.g. compare wi1 wi2 is a compile error)
        tstOrd wi2 wi3 EQ
        tstOrd wi3 wi2 EQ
        tstOrd wi1 wi4 GT
        tstOrd wi4 wi1 LT
        tstOrd wi1 wi5 GT
        tstOrd wi5 wi1 LT
        tstOrd wi4 wi5 GT
        tstOrd wi5 wi4 LT
        tstOrd wi1 wi1 EQ
        tstOrd wi2 wi2 EQ
        tstOrd wi5 wi5 EQ

        tstOrd wi1 31 EQ
        catch (evaluate (compare wi1 65530) >>=
               \_ -> "expected overflow exception" @=? "success with compare 65530 wi1 = GT")
              (\(ErrorCallWithLocation e _l) -> "Value 65530 too large for Word.Indexed of size 5" @=? e)
        tstOrd wi1 (w 65530 :: W 5) GT  -- hi bits of 65530 are dropped
        tstOrd (w 0xffff01a :: W 5) (w 0x51f :: W 5) LT -- only low bits count

    , testCase "Bounded operations" $ do
        31 @=? (maxBound :: W 5)
        1  @=? (maxBound :: W 1)
        65535 @=? (maxBound :: W 16)

        0 @=? (minBound :: W 1)
        0 @=? (minBound :: W 5)
        0 @=? (minBound :: W 16)

    , testCase "Enum operations" $ do
        31 @=? ((toEnum (31 :: Int)) :: W 5)
        1 @=? ((toEnum (1 :: Int)) :: W 1)
        0 @=? ((toEnum (0 :: Int)) :: W 5)

        31 @=? fromEnum ((toEnum (31 :: Int)) :: W 5)
        1 @=? fromEnum ((toEnum (1 :: Int)) :: W 1)
        0 @=? fromEnum ((toEnum (0 :: Int)) :: W 5)

    , let wi1 = w 65535 :: W 5
          wi2 = w 65535 :: W 4
          wi3 = w 32767 :: W 4
          wi4 = w 30 :: W 5
          wi5 = w 0 :: W 5
          wi6 = w 6 :: W 5
          wi7 = w 8 :: W 5
      in testCase "Bits operations" $ do
        30 @=? wi1 .&. wi4
        30 @=? wi4 .&. wi1
        0  @=? wi1 .&. wi5
        0  @=? wi5 .&. wi1
        15 @=? wi2 .&. wi3
        15 @=? wi3 .&. wi2
        31 @=? wi1 .&. wi1
        0  @=? wi5 .&. wi5
        6  @=? wi4 .&. wi6
        0  @=? wi6 .&. wi7

        31 @=? wi1 .|. wi4
        31 @=? wi1 .|. wi5
        31 @=? wi1 .|. wi6
        6  @=? wi5 .|. wi6
        6  @=? wi6 .|. wi5
        30 @=? wi4 .|. wi6
        14 @=? wi6 .|. wi7

        24 @=? wi4 `xor` wi6
        31 @=? wi1 `xor` wi5

        0  @=? complement wi1
        0  @=? complement wi2
        31 @=? complement wi5
        23 @=? complement wi7

        8  @=? shiftL wi7 0
        16 @=? shiftL wi7 1
        0  @=? shiftL wi7 2
        30 @=? shiftL wi1 1
        16 @=? shiftL wi1 4
        0  @=? shiftL wi5 2
        0  @=? shiftL wi5 20

        8  @=? shiftR wi7 0
        4  @=? shiftR wi7 1
        0  @=? shiftR wi7 4
        15 @=? shiftR wi1 1
        3  @=? shiftR wi1 3
        0  @=? shiftR wi5 2
        0  @=? shiftR wi5 20

        Just 5 @=? bitSizeMaybe wi1
        Just 4 @=? bitSizeMaybe wi2
        Just 4 @=? bitSizeMaybe wi3
        Just 5 @=? bitSizeMaybe wi4
        Just 5 @=? bitSizeMaybe wi5
        Just 5 @=? bitSizeMaybe wi6
        Just 5 @=? bitSizeMaybe wi7

        False @=? testBit wi6 0
        True  @=? testBit wi6 1
        True  @=? testBit wi6 2
        False @=? testBit wi6 3
        False @=? testBit wi6 4
        False @=? testBit wi6 5
        False @=? testBit wi6 99

        False @=? isSigned wi1
        False @=? isSigned wi2
        False @=? isSigned wi3
        False @=? isSigned wi4
        False @=? isSigned wi5
        False @=? isSigned wi6
        False @=? isSigned wi7

        5 @=? popCount wi1
        4 @=? popCount wi2
        4 @=? popCount wi3
        4 @=? popCount wi4
        0 @=? popCount wi5
        2 @=? popCount wi6
        1 @=? popCount wi7

        31 @=? rotate wi1 0
        31 @=? rotate wi1 1
        31 @=? rotate wi1 (-1)
        31 @=? rotate wi1 3
        31 @=? rotate wi1 (-3)
        31 @=? rotate wi1 5
        31 @=? rotate wi1 (-5)
        31 @=? rotate wi1 55
        31 @=? rotate wi1 (-55)
        0  @=? rotate wi5 0
        0  @=? rotate wi5 1
        0  @=? rotate wi5 3
        0  @=? rotate wi5 5
        0  @=? rotate wi5 55
        6  @=? rotate wi6 0
        3  @=? rotate wi6 (-1)
        12 @=? rotate wi6 1
        24 @=? rotate wi6 2
        17 @=? rotate wi6 3
        17 @=? rotate wi6 13
        17 @=? rotate wi6 (-2)
        24 @=? rotate wi6 (-3)
        24 @=? rotate wi6 (-13)

    , let wi1 = w 65535 :: W 5
          wi2 = w 65535 :: W 4
          wi3 = w 32767 :: W 4
          wi4 = w 30 :: W 5
          wi5 = w 0 :: W 5
          wi6 = w 6 :: W 5
          wi7 = w 8 :: W 5
      in testCase "Num operations" $ do
        29 @=? wi1 + wi4
        31 @=? wi1 + wi5
        30 @=? wi1 + wi1
        14 @=? wi2 + wi3
        14 @=? wi6 + wi7
        14 @=? wi7 + wi6
        6  @=? wi6 + wi7 + wi7 + wi7 + wi7

        0  @=? wi1 * wi5
        0  @=? wi5 * wi1
        24 @=? wi7 * (shiftR wi6 1)
        16 @=? wi6 * wi7
        1  @=? wi1 * wi1

        1  @=? negate wi1
        1  @=? negate wi2
        1  @=? negate wi3
        2  @=? negate wi4
        0  @=? negate wi5
        26 @=? negate wi6
        24 @=? negate wi7

        31 @=? abs wi1
        15 @=? abs wi2
        15 @=? abs wi3
        30 @=? abs wi4
        0  @=? abs wi5
        6  @=? abs wi6
        8  @=? abs wi7

        1 @=? signum wi1
        1 @=? signum wi2
        1 @=? signum wi3
        1 @=? signum wi4
        0 @=? signum wi5
        1 @=? signum wi6
        1 @=? signum wi7
    ]
