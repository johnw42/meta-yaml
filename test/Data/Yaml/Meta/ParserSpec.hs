module Main where

import           Data.Yaml.Meta
import           Data.Yaml.Meta.Parser

import           Test.Framework                 (Test, defaultMain)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertFailure, (@=?))
import           Text.Parsec                    (runParser)

main :: IO ()
main =
    defaultMain
        [ streamTest "5-1" "\xFEFF"
        , errorTest "5-2" "- Invalid use of BOM\n\xFEFF\n- Inside a document."
        , streamTest
              "5-3"
              "sequence :\n- one\n- two\nmapping :\n? sky\n: blue\n sea : green"
        , streamTest
              "5-4"
              "sequence: [ one , two , ]\nmapping: { sky: blue , sea: green }"
        , streamTest "5-5" "# Comment only."
        , streamTest "5-6" "anchored: ! local & anchor value\nalias: * anchor"
        , streamTest "5-7" "literal: |\n some\n text\nfolded: >\n some\n text"
        , streamTest "5-8" "single: ' text '\ndouble: \" text \""
        , streamTest "5-9" "% YAML 1.2\n--- text"
        , errorTest "5-10" "commercial-at: @ text\ngrave-accent: ` text"
        ]

yamlTest :: Parser a -> String -> String -> Test
yamlTest rule name text = testCase name $ do
    let r = runParser rule initialState name text
    case r of
        Left e  -> assertFailure (show e)
        Right _ -> return ()

streamTest :: String -> String -> Test
streamTest = yamlText lYamlStream

errorTest :: String -> String -> Test
errorTest name text = testCase name $ do
    let r = runParser lYamlStream initialState name text
    case r of
        Left e  -> return ()
        Right r' -> assertFailure $ "Expected parsing to fail; produced output: " ++ show r'
