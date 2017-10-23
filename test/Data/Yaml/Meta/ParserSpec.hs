module Main where

import           Data.Yaml.Meta.Parser

import           Test.Framework                 (Test, defaultMain)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertFailure, (@?=))
import           Text.Parsec                    (runParser)

main :: IO ()
main =
    defaultMain
        [ yamlTest "5-1" "\xFEFF"
        , yamlTestError
              "5-2"
              "- Invalid use of BOM\n\xFEFF\n- Inside a document."
        , yamlTest
              "5-3"
              "sequence :\n- one\n- two\nmapping :\n? sky\n: blue\n sea : green"
        , yamlTest
              "5-4"
              "sequence: [ one , two , ]\nmapping: { sky: blue , sea: green }"
        , yamlTest "5-5" "# Comment only."
        , yamlTest "5-6" "anchored: ! local & anchor value\nalias: * anchor"
        , yamlTest "5-7" "literal: |\n some\n text\nfolded: >\n some\n text"
        , yamlTest "5-8" "single: ' text '\ndouble: \" text \""
        , yamlTest "5-9" "% YAML 1.2\n--- text"
        , yamlTestError "5-10" "commercial-at: @ text\ngrave-accent: ` text"
        ]

yamlTest :: String -> String -> Test
yamlTest name text = testCase name $ do
    let r = runParser lYamlStream initialState name text
    case r of
        Left e  -> assertFailure (show e)
        Right _ -> return ()

yamlTestError :: String -> String -> Test
yamlTestError name text = testCase name $ do
    let r = runParser lYamlStream initialState name text
    case r of
        Left e  -> return ()
        Right r' -> assertFailure $ "Expected parsing to fail; produced output: " ++ show r'
