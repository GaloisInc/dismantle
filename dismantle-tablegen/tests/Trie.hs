{-# LANGUAGE OverloadedStrings #-}
module Trie ( trieTests ) where

import qualified Data.ByteString as BS
import           Data.Either ( fromRight )
import           GHC.Word ( Word8 )
import           Numeric (showHex)
import qualified Test.Tasty as T
import           Test.Tasty.HUnit

import           Dismantle.Tablegen.ByteTrie ( byteTrie, lookupByte )
import           Dismantle.Tablegen.LinearizedTrie as LT


hexStr :: (Integral a, Show a) => a -> String -> String
hexStr v = (<>) "0x" . showHex v

testLookup :: (Eq a, Show a) => LT.LinearizedTrie a -> Word8 -> a -> T.TestTree
testLookup t b expected = testCase (hexStr b " lookup")
                          (let Right v = lookupByte t b in v @?= expected)


trieTests :: IO T.TestTree
trieTests = return $
  T.testGroup "ByteTrie"
  [ T.testGroup "empty" $
    let Right t = byteTrie (Nothing :: Maybe String) []
    in
      [
        -- length is an nonsense operation on a ByteTrie
        -- , testCase "empty length" $
        --   let Right t = byteTrie (Nothing :: Maybe String) []
        --   in length t @?= 0
        testLookup t 0 Nothing
      , testLookup t 1 Nothing
      , testLookup t 10 Nothing
      , testLookup t 0x3d Nothing
      , testLookup t 255 Nothing
      ]

  ----------------------------------------------------------------------

  , T.testGroup "single entry, single level trie"
    [
      T.testGroup "ff req mask and 1 true mask" $
      let Right t = byteTrie Nothing [("SOLO",
                                       BS.pack [0xff],  -- which bits are important
                                       BS.pack [0x1],   -- values of the important bits
                                       [],      -- no contra-indicators
                                       Just ("solo one" :: String))]
      in
        [
          testLookup t 0    Nothing
        , testLookup t 1  $ Just "solo one"
        , testLookup t 10   Nothing
        , testLookup t 0xfe Nothing
        , testLookup t 0xff Nothing
        ]

    , T.testGroup "ff req mask and d1 true mask" $
      let Right t = byteTrie Nothing [("SOLO",
                                       BS.pack [0xff],  -- which bits are important
                                       BS.pack [0xd1],  -- values of the important bits
                                       -- "\x01",  -- must match this value
                                       -- "\xff",  -- all bits are checked
                                       [],      -- no contra-indicators
                                       Just ("solo one" :: String))]
      in
        [
          testLookup t 0    $ Nothing
        , testLookup t 1    $ Nothing
        , testLookup t 10   $ Nothing
        , testLookup t 0xd1 $ Just "solo one"
        , testLookup t 0xfe $ Nothing
        , testLookup t 0xff $ Nothing
        ]

    , T.testGroup "d3 req mask and 1 true mask" $
      let Right t = byteTrie Nothing [("SOLO",
                                       BS.pack [0xd3],  -- which bits are important
                                       BS.pack [0x01],   -- values of the important bits
                                       [],      -- no contra-indicators
                                       Just ("solo one" :: String))]
          matchSOLO = [ 0x01, 0x05, 0x09, 0x0d
                      , 0x21, 0x25, 0x29, 0x2d
                        ]
      in
        [ testLookup t n (if n `elem` matchSOLO
                           then Just "solo one"
                           else Nothing)
          | n <- [0..255] ]

    , T.testGroup "0x83 req mask and 9 true mask" $
      -- This demonstrates that setting a bit in the true mask that
      -- doesn't exist in the req mask will result in an invalid
      -- pattern that will never be added to the ByteTrie.
      --
      -- QUESTIONABLE: should this throw an error when building the
      -- Trie instead of silently ignoring the entry?
      let Right t = byteTrie Nothing [("SOLO",
                                       BS.pack [0x83],  -- which bits are important
                                       BS.pack [0x09],   -- values of the important bits
                                       [],      -- no contra-indicators
                                       Just ("solo one" :: String))]
      in
        [ testLookup t n Nothing | n <- [0..255] ]
        -- ++ [ testLookup t 0 $ Just "should throw an error instead of silently ignoring the input?" ]

    , T.testGroup "83 req mask and 1 true mask with negatives" $
      -- all negatives are applied when the table is built, not during lookups
      let Right t = byteTrie Nothing [("SOLO",
                                       BS.pack [0x83],  -- which bits are important
                                       BS.pack [0x01],   -- values of the important bits
                                       [ (BS.pack [0x80], BS.pack [0x00])
                                       , (BS.pack [0x70], BS.pack [0x30])  -- should eliminate 0x3d
                                       , (BS.pack [0x18], BS.pack [0xc1])
                                       ],      -- no contra-indicators
                                       Just ("solo one" :: String))]
      in
          [ testLookup t 0    $ Nothing
          , testLookup t 1    $ Just "solo one"
          , testLookup t 10   $ Nothing
          , testLookup t 0xd  $ Just "solo one"
          , testLookup t 0x3d $ Just "solo one"  -- TODO: should have been eliminated?!
          , testLookup t 0x7d $ Just "solo one"
          , testLookup t 0x8d $ Nothing
          , testLookup t 0xfe $ Nothing
          , testLookup t 0xff $ Nothing
          ]

    , T.testGroup "0 req mask and 0x31 true mask (ignored)" $
      let Right t = byteTrie Nothing [("SOLO",
                                       BS.pack [0x0],  -- which bits are important
                                       BS.pack [0x31],   -- values of the important bits
                                       [],      -- no contra-indicators
                                       Just ("solo one" :: String))]
      in
        [ testLookup t 0    $ Nothing
        , testLookup t 1    $ Nothing
        , testLookup t 10   $ Nothing
        , testLookup t 0xd  $ Nothing
        , testLookup t 0x3d $ Nothing
        , testLookup t 0x7d $ Nothing
        , testLookup t 0x8d $ Nothing
        , testLookup t 0xfe $ Nothing
        , testLookup t 0xff $ Nothing
        ]

    , T.testGroup "0 req mask and 0 true mask (Broken?)" $
      let Right t = byteTrie Nothing [("SOLO",
                                       BS.pack [0x0],  -- which bits are important
                                       BS.pack [0x0],   -- values of the important bits
                                       [],      -- no contra-indicators
                                       Just ("solo one" :: String))]
      in
        -- QUESTIONABLE: This answers ALL the questions!
        --
        -- All lookups here return "Just "solo one", which is probably
        -- not what is wanted, but this insert should probably have
        -- been rejected (a requireMask should not be == 0), so
        -- leaving this test as broken to determine what the desired
        -- behavior is.
        [ testLookup t 0    $ Just "solo one"
        , testLookup t 1    $ Just "solo one"
        , testLookup t 10   $ Just "solo one"
        , testLookup t 0xd  $ Just "solo one"
        , testLookup t 0x3d $ Just "solo one"
        , testLookup t 0x7d $ Just "solo one"
        , testLookup t 0x8d $ Just "solo one"
        , testLookup t 0xfe $ Just "solo one"
        , testLookup t 0xff $ Just "solo one"
        ]

    ----------------------------------------------------------------------

    , T.testGroup "0xd3/0x41 req mask/true mask" $
      -- establish baseline for next test
      let Right t = byteTrie Nothing
            [("ONE",
               BS.pack [0xd3],  -- which bits are important
               BS.pack [0x41],  -- values of the important bits
               [
                 -- (BS.pack [0x69], BS.pack [0x41])  -- disallows 0x41 and 0x45 values
               ],
               Just ("val one" :: String))
            ]
          matchONE = [ 0x41, 0x45, 0x49, 0x4d, 0x61, 0x65, 0x69, 0x6d
                     ]
        in
        [ testLookup t n (Just "val one") | n <- matchONE ]
        ++ [ testLookup t n Nothing | n <- [0..255], not (n `elem` matchONE) ]
    ]

    , T.testGroup "0xd3/0x41 req mask/true mask negative" $
      -- Demonstrates that a negative match does nothing if there is
      -- nothing else to match against.  This same negative match will
      -- be used in "0xd3/0x41 and 0x83/0x01 req mask/true mask,
      -- negatives" below.
      let Right t = byteTrie Nothing
            [("ONE",
               BS.pack [0xd3],  -- which bits are important
               BS.pack [0x41],  -- values of the important bits
               [
                 (BS.pack [0x69], BS.pack [0x41])  -- disallows 0x41 and 0x45 values
               ],
               Just ("val one" :: String))
            ]
          matchONE = [ 0x41, 0x45, 0x49, 0x4d, 0x61, 0x65, 0x69, 0x6d
                     ]
        in
        [ testLookup t n (Just "val one") | n <- matchONE ]
        ++ [ testLookup t n Nothing | n <- [0..255], not (n `elem` matchONE) ]

  ----------------------------------------------------------------------

  , T.testGroup "multi-entry, single level trie"
    -- Establish baseline results for two entries
    [
      T.testGroup "0xd3/0x41 and 0x83/0x01 req mask/true mask" $
      let Right t = byteTrie Nothing
            [("ONE",
               BS.pack [0xd3],  -- which bits are important
               BS.pack [0x41],  -- values of the important bits
               [],      -- no contra-indicators
               Just ("val one" :: String))
            , ("TWO", BS.pack[0x83], BS.pack [0x1],
                [
                ],
                Just ("val two"))
            ]
          matchONE = [ 0x41, 0x45, 0x49, 0x4d
                     , 0x61, 0x65, 0x69, 0x6d
                     ]
          matchTWO = [ 0x01, 0x05, 0x09, 0x0d
                     , 0x11, 0x15, 0x19, 0x1d
                     , 0x21, 0x25, 0x29, 0x2d
                     , 0x31, 0x35, 0x39, 0x3d
                     , 0x51, 0x55, 0x59, 0x5d
                     , 0x71, 0x75, 0x79, 0x7d
                     ]
        in
        [ testLookup t n (Just "val one") | n <- matchONE ]
        ++ [ testLookup t n (Just "val two") | n <- matchTWO ]
        ++ [ testLookup t n Nothing | n <- [0..255], not (n `elem` (matchONE ++ matchTWO)) ]

    , T.testGroup "0xd3/0x41 and 0x83/0x01 req mask/true mask null negatives" $
      -- Negatives with a null mask don't do anything
      let Right t = byteTrie Nothing
            [("ONE",
               BS.pack [0xd3],  -- which bits are important
               BS.pack [0x41],  -- values of the important bits
               [ (BS.pack [0], BS.pack [0x41])
               ],
               Just ("val one" :: String))
            , ("TWO", BS.pack[0x83], BS.pack [0x1],
                [ (BS.pack [0], BS.pack [m]) | m <- [0..255] ],
                Just ("val two"))
            ]
          matchONE = [ 0x41, 0x45, 0x49, 0x4d
                     , 0x61, 0x65, 0x69, 0x6d
                     ]
          matchTWO = [ 0x01, 0x05, 0x09, 0x0d
                     , 0x11, 0x15, 0x19, 0x1d
                     , 0x21, 0x25, 0x29, 0x2d
                     , 0x31, 0x35, 0x39, 0x3d
                     , 0x51, 0x55, 0x59, 0x5d
                     , 0x71, 0x75, 0x79, 0x7d
                     ]
        in
        [ testLookup t n (Just "val one") | n <- matchONE ]
        ++ [ testLookup t n (Just "val two") | n <- matchTWO ]
        ++ [ testLookup t n Nothing | n <- [0..255], not (n `elem` (matchONE ++ matchTWO)) ]

    , T.testGroup "0xd3/0x41 and 0x83/0x01 req mask/true mask, negatives" $
      -- Demonstrates that the negative match on the first entry
      -- allows the second to take over some of the matches.
      let Right t = byteTrie Nothing
            [("ONE",
               BS.pack [0xd3],  -- which bits are important
               BS.pack [0x41],  -- values of the important bits
               [
                 (BS.pack [0x69], BS.pack [0x41])  -- disallows 0x41 and 0x45 values
               ],
               Just ("val one" :: String))
            , ("TWO", BS.pack[0x83], BS.pack [0x1],
                [
                  -- note that this does not do anything because
                  -- there's no other match
                  (BS.pack [0x31], BS.pack [0x21])
                ],
                Just ("val two"))
            ]
          matchONE = [ 0x49, 0x4d, 0x61, 0x65, 0x69, 0x6d
                     ]
          matchTWO = [ 0x01, 0x05, 0x09, 0x0d
                     , 0x11, 0x15, 0x19, 0x1d
                     , 0x21, 0x25, 0x29, 0x2d
                     , 0x31, 0x35, 0x39, 0x3d
                     , 0x41, 0x45
                     , 0x51, 0x55, 0x59, 0x5d
                     , 0x71, 0x75, 0x79, 0x7d
                     ]
        in
        [ testLookup t n (Just "val one") | n <- matchONE ]
        ++ [ testLookup t n (Just "val two") | n <- matchTWO ]
        ++ [ testLookup t n Nothing | n <- [0..255], not (n `elem` (matchONE ++ matchTWO)) ]

    , T.testGroup "0x83/0x01 and 0xd3/0x41 req mask/true mask, negatives" $
      -- Repeats the above tests but declaring the entries in the
      -- reverse order; the same results are obtained demonstrating
      -- that the ByteTrie is stable relative to entry order.
      let Right t = byteTrie Nothing
            [ ("TWO", BS.pack[0x83], BS.pack [0x1],
                [
                  -- note that this does not do anything because
                  -- there's no other match
                  (BS.pack [0x31], BS.pack [0x21])
                ],
                Just ("val two"))
            , ("ONE",
               BS.pack [0xd3],  -- which bits are important
                BS.pack [0x41],  -- values of the important bits
                [
                  (BS.pack [0x69], BS.pack [0x41])  -- disallows 0x41 and 0x45 values
                ],
                Just ("val one" :: String))
            ]
          matchONE = [ 0x49, 0x4d, 0x61, 0x65, 0x69, 0x6d
                     ]
          matchTWO = [ 0x01, 0x05, 0x09, 0x0d
                     , 0x11, 0x15, 0x19, 0x1d
                     , 0x21, 0x25, 0x29, 0x2d
                     , 0x31, 0x35, 0x39, 0x3d
                     , 0x41, 0x45
                     , 0x51, 0x55, 0x59, 0x5d
                     , 0x71, 0x75, 0x79, 0x7d
                     ]
        in
        [ testLookup t n (Just "val one") | n <- matchONE ]
        ++ [ testLookup t n (Just "val two") | n <- matchTWO ]
        ++ [ testLookup t n Nothing | n <- [0..255], not (n `elem` (matchONE ++ matchTWO)) ]
    ]

  ----------------------------------------------------------------------

  , T.testGroup "multi-entry, multi-level trie"
    [
      T.testGroup "five opcodes various depths" $
      let Right t = byteTrie Nothing
            [ ("ONE", BS.pack [0xff,0xff,0xd3], BS.pack [0x12,0x34,0x41],
               [
                 (BS.pack [0x00,0x00,0x69], BS.pack [0x00,0x00,0x41])
               ],
               Just ("val one" :: String))
            , ("TWO", BS.pack [0x33,0x3f,0x83], BS.pack [0x12,0x34,0x01],
                [
                  -- note that this does not do anything because
                  -- there's no other match
                  (BS.pack [0x00,0x00,0x31], BS.pack [0x00,0x00,0x21])
                ],
                Just ("val two"))
            , ("THREE", BS.pack [0xd3], BS.pack [0x41],
               [
                 -- wrong depth... ignored
                 (BS.pack [0x00,0x00,0x69], BS.pack [0x00,0x00,0x41])
               ], Just "val three")
            , ("FOUR", BS.pack [0xd3,0xff], BS.pack [0x42,0x24], [], Just "val four")
            , ("FIVE", BS.pack [0x13,0x36,0xd3], BS.pack [0x12,0x34,0xd1],
                [],
                Just ("val five"))
            ]
          matchONE = [ 0x49, 0x4d, 0x61, 0x65, 0x69, 0x6d
                     ]
          matchTWO = [ 0x01, 0x05, 0x09, 0x0d
                     , 0x11, 0x15, 0x19, 0x1d
                     , 0x21, 0x25, 0x29, 0x2d
                     , 0x31, 0x35, 0x39, 0x3d
                     , 0x41, 0x45
                     , 0x51, 0x55, 0x59, 0x5d
                     , 0x71, 0x75, 0x79, 0x7d
                     ]
          matchTHREE = [ 0x41, 0x45, 0x49, 0x4d
                       , 0x61, 0x65, 0x69, 0x6d
                       ]
          matchFOURb1 = [ 0x42, 0x46, 0x4a, 0x4e
                        , 0x62, 0x66, 0x6a, 0x6e
                        ]
          matchFIVEb1 = [ 0x32, 0x36, 0x3a, 0x3e
                        , 0x72, 0x76, 0x7a, 0x7e
                        , 0xb2, 0xb6, 0xba, 0xbe
                        , 0xf2, 0xf6, 0xfa, 0xfe
                        ]
          matchFIVE = [ 0xd1, 0xd5, 0xd9, 0xdd
                      , 0xf1, 0xf5, 0xf9, 0xfd
                      ]
        in
        [ T.testGroup "three" [ testLookup t n (Just "val three") | n <- matchTHREE ]

        , T.testGroup "one/two/five"
          (let Left l1 = lookupByte t 0x12
               Left l2 = lookupByte l1 0x34
           in [ testLookup l2 n (if n `elem` matchONE
                                 then Just "val one"
                                 else if n `elem` matchTWO
                                  then Just "val two"
                                  else if n `elem` matchFIVE
                                       then Just "val five"
                                       else Nothing)
              | n <- [0..255] ])

        , T.testGroup "five shortcut" $
          -- See note for group four below
          [ testLookup t n (Just "val five") | n <- matchFIVEb1 ]

        , T.testGroup "four" $
          -- While FOUR is defined via a 2-byte sequence, the match on
          -- the first byte is sufficient to unambiguously identify
          -- these entries, so only the values of the first byte are
          -- needed to match at the base level and the additional byte
          -- is ignored.
          --
          -- QUESTIONABLE: ignoring the second byte will match against
          -- _any_ of the second byte value, even though the
          -- specification above defined a specific second byte value.
          [ testLookup t n (Just "val four") | n <- matchFOURb1 ]

        , T.testGroup "level 1" $
          [ testCase (hexStr n " get") $
            fromRight (Just "nextlevel") (lookupByte t n) @?=
            (if n `elem` matchTHREE
              then Just "val three"
              else if n `elem` matchFOURb1
                   then Just "val four"
                   else if n `elem` matchFIVEb1
                        then Just "val five"
                        else if n `elem` [ 0x12, 0x16, 0x1a, 0x1e
                                         , 0x52, 0x56, 0x5a, 0x5e
                                         , 0x92, 0x96, 0x9a, 0x9e
                                         , 0xd2, 0xd6, 0xda, 0xde
                                         ]
                             then Just "nextlevel"
                             else Nothing)
          | n <- [0..255]
          ]
        ]

    ]

  ----------------------------------------------------------------------

  , testCase "creation failures" $
    let Left e = byteTrie (Nothing :: Maybe String)
                 [ ("SOLO", BS.pack [0xff], BS.pack [0x01], [], Just "solo one")
                 , ("DUO",  BS.pack [0xff], BS.pack [0x01], [], Just "solo dup")
                 ]
        -- n.b. Pattern isn't exported from ByteTrie, so can only
        -- compare show representation.
    in show e @?= "OverlappingBitPattern [(Pattern { requiredMask = [255], trueMask = [1]}, [\"DUO\",\"SOLO\"], 1)]"

  , testCase "mask length error" $
    let Left e = byteTrie (Nothing :: Maybe String)
                 [ ("SOLO", BS.pack [0xff], BS.pack [0x01,0x02], [], Just "solo one")
                 ]
        -- n.b. Pattern isn't exported from ByteTrie, so can only
        -- compare show representation.
    in show e @?= "InvalidPatternLength Pattern { requiredMask = [255], trueMask = [1,2]}"

  , testCase "match length error" $
    let Left e = byteTrie (Nothing :: Maybe String)
                 [ ("SOLO", BS.pack [0xff,0xff], BS.pack [0x01], [], Just "solo one")
                 ]
        -- n.b. Pattern isn't exported from ByteTrie, so can only
        -- compare show representation.
    in show e @?= "InvalidPatternLength Pattern { requiredMask = [255,255], trueMask = [1]}"

  ]
