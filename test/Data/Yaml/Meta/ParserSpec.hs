{-# LANGUAGE DeriveFunctor #-}

module Main where

import           Data.Yaml.Meta
import           Data.Yaml.Meta.Parser

import           Test.Framework                 (Test, defaultMain)
import           Test.Framework.Providers.HUnit (testCase)
import           Test.HUnit                     (assertFailure, (@=?))
import           Text.Parsec                    hiding (Stream)
--(runParser)

data Expectation a = Fail | Succeed | Produce a deriving (Show, Functor)

main :: IO ()
main =
    defaultMain
        [ yamlTest "nbChar-1" nbChar "a" Succeed
        , yamlTest
              "cNsAliasNode"
              cNsAliasNode
              "*foo"
              (Produce $ AliasNode "foo")
        , yamlTest
              "cSingleQuoted-1"
              (cSingleQuoted 0 FlowOut)
              "'foo'"
              (Produce "foo")
        , yamlTest
              "cSingleQuoted-2"
              (cSingleQuoted 0 FlowOut)
              "'fo''o'"
              (Produce "fo'o")
        , yamlTest
              "cSingleQuoted-3"
              (cSingleQuoted 0 FlowOut)
              "'fo  o'"
              (Produce "fo  o")
        , yamlTest
              "cSingleQuoted-4"
              (cSingleQuoted 1 FlowOut)
              "'fo\n  o'"
              (Produce "fo o")
        , yamlTest
              "cDoubleQuoted-1"
              (cDoubleQuoted 0 FlowOut)
              "\"foo\""
              (Produce "foo")
        , yamlTest "sSingleNextLine-1" (sSingleNextLine 0) "\no" (Produce " o")
        , yamlTest
              "sSingleNextLine-2"
              (sSingleNextLine 1)
              "\n  o"
              (Produce " o")
        , yamlTest "nsSingleChar-1" nsSingleChar "o" (Produce 'o')
        , yamlTest "nbNsSingleInLine-1" nbNsSingleInLine "" (Produce "")
        , yamlTest "nbNsSingleInLine-2" nbNsSingleInLine "o" (Produce "o")
        , yamlTest "nbNsSingleInLine-3" nbNsSingleInLine " o" (Produce " o")
        , yamlTest "bAsSpace" bAsSpace "\n" (Produce " ")
        , yamlTest "sFlowFolded-1" (sFlowFolded 0) "\n" Succeed
        , yamlTest "sFlowFolded-2" (sFlowFolded 2) "\n    " Succeed
        , yamlTest
              "sFlowFolded-3"
              (sFlowFolded 0 *> char 'x')
              "\nx"
              (Produce 'x')
        , yamlTest "sFlowFolded-4" (sFlowFolded 0) "\n " Fail
        , yamlTest "sFlowLinePrefix-0" (sFlowLinePrefix 0) "" Succeed
        , yamlTest "sFlowLinePrefix-1" (sFlowLinePrefix 1) "  " Succeed
        , yamlTest "sFlowLinePrefix-2" (sFlowLinePrefix 2) "   " Succeed
        , yamlTest "sFlowLinePrefix-3" (sFlowLinePrefix 0) " " Fail
        , yamlTest "sFlowLinePrefix-4" (sFlowLinePrefix 1) " " Fail
        , yamlTest "sFlowLinePrefix-5" (sFlowLinePrefix 2) "  " Fail
        , yamlTest "sIndent" (sIndent 2) "  " Succeed
        , yamlTest "lineStart" lineStart "" Succeed
        , streamTest "0-1" "" Succeed
        , streamTest "0-2" "- foo\n- bar\n" Succeed
        , yamlTest
              "lPlusBlockSequence-1"
              (lPlusBlockSequence $ -1)
              "- foo"
              (Produce $ SeqNode [ScalarNode "foo"])
        , yamlTest
              "lPlusBlockSequence-2"
              (lPlusBlockSequence $ -1)
              "- foo\n- bar\n"
              (Produce $ SeqNode [ScalarNode "foo", ScalarNode "bar"])
        , yamlTest
              "cLBlockSeqEntry"
              (cLBlockSeqEntry 0)
              "- foo"
              (Produce $ ScalarNode "foo")
        -- , yamlTest
        --       "sLPlusBlockIndented"
        --       (sLPlusBlockIndented 0 BlockIn)
        --       " foo"
        --       (Produce $ ScalarNode "foo")
        , yamlTest
              "sLPlusBlockNode-1"
              (sLPlusBlockNode 0 BlockIn)
              "foo"
              (Produce $ ScalarNode "foo")
        , yamlTest "sLPlusBlockNode-2" (sLPlusBlockNode 0 BlockIn) " foo" Fail
        , yamlTest "0-3" sIndent' "   " (Produce 3)
        , yamlTest "0-4" (sIndent 2 *> sIndent') "    " (Produce 2)
        , yamlTest "0-5" (sIndent 0 *> sIndent') "    " (Produce 4)
        , streamTest "5-1" "\xFEFF" Succeed
        , errorTest "5-2" "- Invalid use of BOM\n\xFEFF\n- Inside a document."
        , streamTest
              "5-3"
              "sequence:\n\
              \- one\n\
              \- two\n\
              \mapping:\n\
              \  ? sky\n\
              \  : blue\n\
              \  sea : green"
              Succeed
        , streamTest
              "5-4"
              "sequence: [ one , two , ]\n\
              \mapping: { sky: blue , sea: green }"
              Succeed
        , streamTest "5-5" "# Comment only." Succeed
        , streamTest
              "5-6"
              "anchored: !local &anchor value\n\
              \alias: *anchor"
              Succeed
        , streamTest
              "5-7"
              "literal: |\n\
              \  some\n\
              \  text\n\
              \folded: >\n\
              \  some\n\
              \  text\n"
              Succeed
        , streamTest
              "5-8"
              "single: 'text'\n\
              \double: \"text\""
              Succeed
        , streamTest "5-9" "%YAML 1.2\n--- text" Succeed
        , errorTest "5-10" "commercial-at: @ text\ngrave-accent: ` text"
        , streamTest
              "5-11"
              "|\n\
              \  Line break (no glyph)\n\
              \  Line break (glyphed)\n"
              Succeed
        , streamTest
              "5-12"
              "# Tabs and spaces\n\
              \quoted: \"Quoted \t\"\n\
              \block:\t|\n\
              \  void main() {\n\
              \  \tprintf(\"Hello, world!\\n\");\n\
              \  }\n"
              Succeed
        , nodeTest
              "5-13"
              "\"Fun with \\\\\n\
              \\\\" \\a \\b \\e \\f \\\n\
              \\\n \\r \\t \\v \\0 \\\n\
              \\\  \\_ \\N \\L \\P \\\n\
              \\\x41 \\u0041 \\U00000041\""
              (Produce $
               ScalarNode
                   "Fun with \x5C \x22 \x07 \x08 \x1B \x0C \x0A \x0D \x09 \x0B \x00 \x20 \xA0 \x85 \x2028 \x2029 A A A")
        , errorTest
              "5-14"
              "Bad escapes:\n\
              \  \"\\c\n\
              \  \\xq-\""
        , nodeTest
              "7-5"
              "\"folded \n\
              \to a space,\t\n\
              \ \n\
              \to a line feed, or \t\\\n\
              \ \\ \t non-content\""
              (Produce $
               ScalarNode
                   "folded to a space,\nto a line feed, or \t \tnon-content")
        , yamlTest
              "7-5-cDoubleQuoted"
              (cDoubleQuoted 0 FlowIn)
              "\"folded \n\
              \to a space,\t\n\
              \ \n\
              \to a line feed, or \t\\\n\
              \ \\ \t non-content\""
              (Produce $
               "folded to a space,\nto a line feed, or \t \tnon-content")
        , yamlTest "sFlowFolded-1" (sFlowFolded 0) "\t\n \n" Succeed
        , yamlTest "sFlowFolded-2" (sFlowFolded 0) "\n  " Succeed
        , yamlTest "sFlowFolded-3" (sFlowFolded 0) " \n \n  \t " Succeed
        , yamlTest "sFlowFolded-4" (sFlowFolded 0) "\n\n  " Succeed
        , yamlTest "sFlowFolded-5" (sFlowFolded 0) "\n" Succeed
        , yamlTest "sSeparateInLine-1" sSeparateInLine " " Succeed
        , yamlTest "sSeparateInLine-2" sSeparateInLine "\t " Succeed
        , yamlTest "sSeparateInLine-3" sSeparateInLine " " Succeed
          -- from ex 6.7
        , yamlTest "bLFolded-1" (bLFolded 0 BlockIn) " \n" Succeed
          -- from ex 6.7
        , yamlTest "bLFolded-2" (bLFolded 0 BlockIn) "\n" Succeed
          -- from ex 6.6
        , yamlTest
              "bLTrimmed-1"
              (bLTrimmed 2 BlockIn)
              "\n\
              \  \n\
              \ \n\
              \\n\
              \"
              Succeed
        , yamlTest "lEmpty-1" (lEmpty 2 BlockIn) "   \t\n" Succeed
        , yamlTest "lEmpty-2" (lEmpty 2 BlockIn) " \n" Succeed
        , nodeTest
              "6-8"
              "\"\n\
              \  foo \n\
              \ \n\
              \  \t bar\n\
              \\n\
              \  baz↓\n\
              \\"\n\
              \"
              (Produce $ ScalarNode " foo\nbar\nbaz ")
        ]

yamlTest ::
       (Show a, Eq a)
    => String        -- ^ test name
    -> Parser a      -- ^ rule
    -> String        -- ^ input
    -> Expectation a -- ^ expected outcome
    -> Test
yamlTest name rule text expected =
    testCase name $ do
        let r = runParser (rule <* eof) initialState name text
        case r of
            Left e ->
                case expected of
                    Fail -> return ()
                    _    -> assertFailure (show e)
            Right actual ->
                case expected of
                    Produce x -> x @=? actual
                    Succeed -> return ()
                    Fail ->
                        assertFailure $ "expected failure, got " ++ show actual

nodeTest :: String -> String -> Expectation Node -> Test
nodeTest name text expected = docTest name text (Document <$> expected)

docTest :: String -> String -> Expectation Document -> Test
docTest name text expected = streamTest name text (Stream . (: []) <$> expected)

streamTest :: String -> String -> Expectation Stream -> Test
streamTest name text expected = yamlTest name lYamlStream text expected

errorTest :: String -> String -> Test
errorTest name text = yamlTest name lYamlStream text Fail
