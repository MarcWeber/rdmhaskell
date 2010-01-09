module Main where
-- import Test.NoPrimary
import Test.PrimarySimple
import Test.PrimaryState
import Test.CDTracks
import Test.HUnit

main = do
  runTestTT $ TestList 
    [ -- testNoPrimary
    testPrimarySimple
    , testPrimaryState
    , testCDTracks 
    ]
