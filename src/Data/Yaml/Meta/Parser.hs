module Data.Yaml.Meta.Parser where

import           Data.Yaml.Meta

import           Control.Applicative (empty)
import           Data.Char           (chr, digitToInt)
import           Data.Maybe          (maybeToList)
import           Debug.Trace
import           Numeric             (readHex)
import           Text.Parsec         (Column, Parsec, SourcePos, char, count,
                                      digit, eof, getPosition, getState,
                                      hexDigit, label, lookAhead, many, many1,
                                      modifyState, notFollowedBy, oneOf, option,
                                      optionMaybe, optional, satisfy, skipMany,
                                      skipMany1, sourceColumn, string, try,
                                      unexpected, (<?>), (<|>))
import           Text.Parsec.Prim    (getInput, setInput)

data ParserState = PS
    { lastPlainCharPos :: Maybe SourcePos
    }

initialState :: ParserState
initialState = PS Nothing

type Parser a = Parsec String ParserState a

-- | Try, but probably not needed.
try' :: Parser a -> Parser a
try' = try

infixl 4 <++>, <:>

(<++>) :: Parser [a] -> Parser [a] -> Parser [a]
p <++> p' = (++) <$> p <*> p'

(<:>) :: Parser a -> Parser [a] -> Parser [a]
p <:> p' = (:) <$> p <*> p'

one :: Parser a -> Parser [a]
one = fmap (: [])

concatMany :: Parser [a] -> Parser [a]
concatMany = fmap concat . many

concatMany1 :: Parser [a] -> Parser [a]
concatMany1 = fmap concat . many1

optionList :: Parser a -> Parser [a]
optionList p = maybeToList <$> optionMaybe p

rule :: Int -> String -> Parser a -> Parser a
rule num name p = p <?> "[" ++ show num ++ "] " ++ name

ruleN :: Int -> String -> Column -> Parser a -> Parser a
ruleN num name n = rule num (name ++ "(" ++ show n ++ ")")

ruleNC :: Int -> String -> Column -> Context -> Parser a -> Parser a
ruleNC num name n c = rule num (name ++ "(" ++ show n ++ "," ++ show c ++ ")")

-- http://yaml.org/spec/1.2/spec.pdf

-- data Stream = Stream
--     { streamDocuments :: [Document]
--     , streamTail :: [String]
--     }

-- data Document = Document
--     { docContent :: Node
--     , docDirectives :: [Directive]
--     , docCommentsBefore :: String
--     , docCommentsAfter :: String
--     }

-- data Directive = Directive

-- 1
cPrintable :: Parser Char
cPrintable = satisfy p <?> "printable charcter"
  where
    p '\x9' = True
    p '\xa' = True
    p '\xd' = True
    p '\x85' = True
    p c = '\x20' <= c && c <= '\x7E' ||
          '\xA0' <= c && c <= '\xD7FF' ||
          '\xE000' <= c && c <= '\xFFFD' ||
          '\x10000' <= c && c <= '\x10FFFF'

-- 2
nbJson :: Parser Char
nbJson = satisfy p <?> "JSON character"
  where
    p '\x9' = True
    p c     = '\x20' <= c && c <= '\x10FFFF'

-- 3
cByteOrderMark :: Parser Char
cByteOrderMark = char '\xFEFF' <?> "byte order mark"

-- 4
cSequenceEntry :: Parser Char
cSequenceEntry = char '-'

-- 5
cMappingKey :: Parser Char
cMappingKey = char '?'

-- 6
cMappingValue :: Parser Char
cMappingValue = char ':'

-- 7
cCollectEntry :: Parser Char
cCollectEntry = char ','

-- 8
cSequenceStart :: Parser Char
cSequenceStart = char '['

-- 9
cSequenceEnd :: Parser Char
cSequenceEnd = char ']'

-- 10
cMappingStart :: Parser Char
cMappingStart = char '{'

-- 11
cMappingEnd :: Parser Char
cMappingEnd = char '}'

-- 12
cComment :: Parser Char
cComment = char '#'

-- 13
cAnchor :: Parser Char
cAnchor = char '&'

-- 14
cAlias :: Parser Char
cAlias = char '*'

-- 15
cTag :: Parser Char
cTag = char '!'

-- 16
cLiteral :: Parser Char
cLiteral = char '|'

-- 17
cFolded :: Parser Char
cFolded = char '>'

-- 18
cSingleQuote :: Parser Char
cSingleQuote = rule 18 "c-single-quote" $ char '\''

-- 19
cDoubleQuote :: Parser Char
cDoubleQuote = rule 19 "c-double-quote" $ char '"'

-- 20
cDirective :: Parser Char
cDirective = rule 20 "c-directive" $ char '%'

-- 21 TODO: Fail if this rule matches.
cReserved :: Parser Char
cReserved = oneOf "@`"

-- 22
cIndicator :: Parser Char
cIndicator = oneOf "-?:,[]{}#&*!|>'\"%@`"

-- 23
cFlowIndicator :: Parser Char
cFlowIndicator = oneOf ",[]{}"

-- 24
bLineFeed :: Parser Char
bLineFeed = char '\xA'

-- 25
bCarriageReturn :: Parser Char
bCarriageReturn = char '\xD'

-- 26
bChar :: Parser Char
bChar = rule 26 "b-char" $ bLineFeed <|> bCarriageReturn

-- 27
nbChar :: Parser Char
nbChar =
    rule 27 "nb-char" $
    notFollowedBy bChar *> notFollowedBy cByteOrderMark *> cPrintable

-- 28
bBreak :: Parser ()
bBreak =
    rule 28 "b-break" $
    () <$ (try (bCarriageReturn <* bLineFeed) <|> bCarriageReturn <|> bLineFeed)

-- 29
bAsLineFeed :: Parser String
bAsLineFeed = rule 29 "b-as-line-feed" $ "\n" <$ bBreak

-- 30
bNonContent :: Parser ()
bNonContent = rule 30 "b-non-content" $ bBreak

-- 31, 32, 33
sSpace, sTab, sWhite :: Parser Char
sSpace = rule 31 "s-space" $ char '\x20'
sTab = char '\x9'
sWhite = rule 33 "s-white" $ sSpace <|> sTab

-- 34
nsChar :: Parser Char
nsChar = do
    try (notFollowedBy sWhite)
    c <- nbChar
    pos <- getPosition
    modifyState (\s -> s { lastPlainCharPos = Just pos })
    return c


-- 35
nsDecDigit :: Parser Char
nsDecDigit = digit

-- 36
nsHexDigit :: Parser Char
nsHexDigit = hexDigit

-- 37
nsAsciiLetter :: Parser Char
nsAsciiLetter = satisfy (\c -> 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z')

-- 38
nsWordChar :: Parser Char
nsWordChar = nsDecDigit <|> nsAsciiLetter <|> char '-'

-- 39
nsUriChar :: Parser String
nsUriChar =
    (\d1 d2 -> ['%', d1, d2]) <$ char '%' <*> nsHexDigit <*> nsHexDigit <|>
    count 1 (nsWordChar <|> (oneOf "#;/?:@&=+$,_.!~*'()[]"))

-- 40
nsTagChar :: Parser String
nsTagChar =
    try (notFollowedBy $ char '!') *> try (notFollowedBy cFlowIndicator) *>
    nsUriChar

-- 41
cEscape :: Parser ()
cEscape = () <$ char '\\'

-- 42
nsEscNull :: Parser Char
nsEscNull = '\0' <$ char '0'

-- 43
nsEscBell :: Parser Char
nsEscBell = '\x7' <$ char 'a'

-- 44
nsEscBackspace :: Parser Char
nsEscBackspace = '\x8' <$ char 'b'

-- 45
nsEscHorizontalTab :: Parser Char
nsEscHorizontalTab = '\x9' <$ oneOf "t\x9"

-- 46
nsEscLineFeed :: Parser Char
nsEscLineFeed = '\xA' <$ char 'n'

-- 47
nsEscVerticalTab :: Parser Char
nsEscVerticalTab = '\xB' <$ char 'v'

-- 48
nsEscFormFeed :: Parser Char
nsEscFormFeed = '\xC' <$ char 'f'

-- 49
nsEscCarriageReturn :: Parser Char
nsEscCarriageReturn = '\xD' <$ char 'r'

-- 50
nsEscEscape :: Parser Char
nsEscEscape = '\x1B' <$ char 'e'

-- 51
nsEscSpace :: Parser Char
nsEscSpace = char '\x20'

-- 52
nsEscDoubleQuote :: Parser Char
nsEscDoubleQuote = char '"'

-- 53
nsEscSlash :: Parser Char
nsEscSlash = char '/'

-- 54
nsEscBackslash :: Parser Char
nsEscBackslash = char '\\'

-- 55
nsEscNextLine :: Parser Char
nsEscNextLine = '\x85' <$ char 'N'

-- 56
nsEscNonBreakingSpace :: Parser Char
nsEscNonBreakingSpace = '\xA0' <$ char '_'

-- 57
nsEscLineSeparator :: Parser Char
nsEscLineSeparator = '\x2028' <$ char 'L'

-- 58
nsEscParagraphSeparator :: Parser Char
nsEscParagraphSeparator = '\x2029' <$ char 'P'

readHexChar :: String -> Char
readHexChar = chr . fst . head . readHex

-- 59
nsEsc8Bit :: Parser Char
nsEsc8Bit = readHexChar <$ char 'x' <*> count 2 nsHexDigit

-- 60
nsEsc16Bit :: Parser Char
nsEsc16Bit = readHexChar <$ char 'u' <*> count 4 nsHexDigit

-- 61
nsEsc32Bit :: Parser Char
nsEsc32Bit = readHexChar <$ char 'U' <*> count 8 nsHexDigit

-- 62
cNsEscChar :: Parser Char
cNsEscChar =
    char '\\' *>
    (nsEscNull <|>
     nsEscBell <|>
     nsEscBackspace <|>
     nsEscHorizontalTab <|>
     nsEscLineFeed <|>
     nsEscVerticalTab <|>
     nsEscFormFeed <|>
     nsEscCarriageReturn <|>
     nsEscEscape <|>
     nsEscSpace <|>
     nsEscDoubleQuote <|>
     nsEscSlash <|>
     nsEscBackslash <|>
     nsEscNextLine <|>
     nsEscNonBreakingSpace <|>
     nsEscLineSeparator <|>
     nsEscParagraphSeparator <|>
     nsEsc8Bit <|>
     nsEsc16Bit <|>
     nsEsc32Bit)

-- 63
sIndent :: Column -> Parser ()
sIndent n = ruleN 63 "s-indent" n $ () <$ count n sSpace

sIndent' :: Parser Column
sIndent' = length <$> many sSpace

-- 64
sIndentLt :: Int -> Parser ()
sIndentLt n = sIndentLe (n - 1)

-- 65
sIndentLe :: Int -> Parser ()
sIndentLe n
    | n > 0 = sSpace *> sIndentLe (n - 1)
    | otherwise = pure ()

lineStart :: Parser ()
lineStart =
    (do col <- sourceColumn <$> getPosition
        case col of
            1 -> return ()
            _ -> empty) <?>
    "start of line"

-- 66: Mandatory whitespace except at the start of a line.
sSeparateInLine :: Parser ()
sSeparateInLine = lineStart <|> skipMany1 sWhite

data Context
    = BlockOut
    | BlockIn
    | FlowOut
    | FlowIn
    | BlockKey
    | FlowKey
    deriving (Show)

-- 67
sLinePrefix :: Int -> Context -> Parser ()
sLinePrefix n c =
    (case c of
         BlockOut -> sBlockLinePrefix n
         BlockIn  -> sBlockLinePrefix n
         FlowOut  -> sFlowLinePrefix n
         FlowIn   -> sFlowLinePrefix n
         _        -> badContext c) <?>
    "s-line-prefix(" ++
    show n ++ "," ++ show c ++ ") [67]"

badContext :: Context -> Parser a
badContext c = error ("Unexpected context: " ++ show c)

-- 68
sBlockLinePrefix :: Int -> Parser ()
sBlockLinePrefix n = ruleN 68 "s-block-line-prefix" n $ sIndent n

-- 69
sFlowLinePrefix :: Int -> Parser ()
sFlowLinePrefix n =
    ruleN 69 "s-flow-line-prefix" n $ sIndent n *> sSeparateInLine

-- 70
lEmpty :: Int -> Context -> Parser String
lEmpty n c =
    ruleNC 70 "l-empty" n c $
    (try (sLinePrefix n c) <|> sIndentLt n) *> bAsLineFeed

-- 71
bLTrimmed :: Int -> Context -> Parser String
bLTrimmed n c =
    ruleNC 71 "b-l-trimmed" n c $ bNonContent *> concatMany1 (try $ lEmpty n c)

-- 72
bAsSpace :: Parser String
bAsSpace = rule 72 "b-as-space" $ " " <$ bBreak

-- 73
bLFolded :: Column -> Context -> Parser String
bLFolded n c = ruleNC 73 "b-l-folded" n c $ try (bLTrimmed n c) <|> bAsSpace

-- 74
sFlowFolded :: Column -> Parser String
sFlowFolded n =
    ruleN 74 "s-flow-folded" n $
    " " <$ optional (try sSeparateInLine) <* bLFolded n FlowIn <*
    sFlowLinePrefix n

-- 75
cNbCommentText :: Parser String
cNbCommentText = rule 75 "c-nb-comment-text" $ (:) <$> char '#' <*> many nbChar

-- 76
bComment :: Parser ()
bComment = bNonContent <|> eof

-- 77
sBComment :: Parser String
sBComment =
    option "" (try $ sSeparateInLine *> option "" (try cNbCommentText)) <*
    bComment

-- 78
lComment :: Parser String
lComment = sSeparateInLine *> cNbCommentText <* bComment

-- 79
sLComments :: Parser String
sLComments = ("" <$ lineStart <|> sBComment) <|> concatMany (try lComment)

-- 80
sSeparate :: Column -> Context -> Parser ()
sSeparate n BlockOut = sSeparateLines n
sSeparate n BlockIn  = sSeparateLines n
sSeparate n FlowOut  = sSeparateLines n
sSeparate n FlowIn   = sSeparateLines n
sSeparate _ BlockKey = sSeparateInLine
sSeparate _ FlowKey  = sSeparateInLine

-- 81
sSeparateLines :: Column -> Parser ()
sSeparateLines n = try' (() <$ (sLComments >> sFlowLinePrefix n)) <|> sSeparateInLine

-- 82
lDirective :: Parser ()
lDirective =
    () <$ char '%' <*
    (try nsYamlDirective <|> try nsTagDirective <|> nsReservedDirective) <*
    sLComments

-- 83
nsReservedDirective :: Parser ()
nsReservedDirective =
    () <$ nsDirectiveName <*
    many (try $ sSeparateInLine <* nsDirectiveParameter)

-- 84
nsDirectiveName :: Parser ()
nsDirectiveName = () <$ many1 nsChar

-- 85
nsDirectiveParameter :: Parser ()
nsDirectiveParameter = () <$ many1 nsChar

-- 86
nsYamlDirective :: Parser ()
nsYamlDirective =
    rule 86 "ns-yaml-directive" $
    string "YAML" *> sSeparateInLine *> nsYamlVersion

-- 87
nsYamlVersion :: Parser ()
nsYamlVersion =
    rule 87 "ns-yaml-version" $
    () <$ many1 nsDecDigit <* char '.' <* many1 nsDecDigit

-- 88
nsTagDirective :: Parser ()
nsTagDirective =
    rule 88 "ns-tag-directive" $
    () <$ string "TAG" <* sSeparateInLine <* cTagHandle <* sSeparateInLine <*
    nsTagPrefix

-- 89
cTagHandle :: Parser ()
cTagHandle = rule 89 "c-tag-handle" $
    try cNamedTagHandle <|> try cSecondaryTagHande <|>
    cPrimaryTagHandle

-- 90
cPrimaryTagHandle :: Parser ()
cPrimaryTagHandle = () <$ char '!'

-- 91
cSecondaryTagHande :: Parser ()
cSecondaryTagHande = () <$ string "!!"

-- 92
cNamedTagHandle :: Parser ()
cNamedTagHandle = () <$ char '!' <* many1 nsWordChar <* char '!'

-- 93
nsTagPrefix :: Parser ()
nsTagPrefix = rule 93 "ns-tag-prefix" $ cNsLocalTagPrefix <|> nsGlobalTagPrefix

-- 94
cNsLocalTagPrefix :: Parser ()
cNsLocalTagPrefix = char '!' *> skipMany nsUriChar

-- 95
nsGlobalTagPrefix :: Parser ()
nsGlobalTagPrefix = nsTagChar *> skipMany nsUriChar

-- 96
cNsProperties :: Column -> Context -> Parser ()
cNsProperties n c =
    ruleNC 96 "c-ns-properties" n c $
    try (cNsTagProperty *> optional (try $ sSeparate n c *> cNsAnchorProperty)) <|>
    (cNsAnchorProperty *> optional (try $ sSeparate n c *> cNsTagProperty))

-- 97
cNsTagProperty :: Parser ()
cNsTagProperty =
    rule 97 "c-ns-tag-property" $
    try cVerbatimTag <|> try cNsShorthandTag <|> cNonSpecificTag

-- 98
cVerbatimTag :: Parser ()
cVerbatimTag = () <$ try (string "!<") <* many1 nsUriChar <* char '>'

-- 99
cNsShorthandTag :: Parser ()
cNsShorthandTag = cTagHandle *> skipMany1 nsTagChar

-- 100
cNonSpecificTag :: Parser ()
cNonSpecificTag = () <$ char '!'

-- 101
cNsAnchorProperty :: Parser String
cNsAnchorProperty = char '&' *> nsAnchorName <?> "anchor property"

-- 102
nsAnchorChar :: Parser Char
nsAnchorChar = try (notFollowedBy cFlowIndicator) *> nsChar

-- 103
nsAnchorName :: Parser String
nsAnchorName = many1 nsAnchorChar <?> "anchor name"

-- 104
cNsAliasNode :: Parser Node
cNsAliasNode = AliasNode <$ char '*' <*> nsAnchorName <?> "anchor reference"

-- 105
eScalar :: Parser Node
eScalar = return emptyNode

-- 106
eNode :: Parser Node
eNode = eScalar <?> "empty node"

-- 107
nbDoubleChar :: Parser Char
nbDoubleChar = cNsEscChar <|> try (notFollowedBy $ oneOf "\\\"") *> nbJson
               <?> "double-quoted character"

-- 108
nsDoubleChar :: Parser Char
nsDoubleChar =
    try (notFollowedBy sWhite) *>
    nbDoubleChar <?> "double-quoted non-whitespace character"

-- 109
cDoubleQuoted :: Column -> Context -> Parser String
cDoubleQuoted n c = char '"' *> nbDoubleText n c <* char '"'
                    <?> "double-quoted string"

-- 110
nbDoubleText :: Column -> Context -> Parser String
nbDoubleText n FlowOut  = nbDoubleMultiLine n
nbDoubleText n FlowIn   = nbDoubleMultiLine n
nbDoubleText _ BlockKey = nbDoubleOneLine
nbDoubleText _ FlowKey  = nbDoubleOneLine
nbDoubleText _ c        = badContext c

-- 111
nbDoubleOneLine :: Parser String
nbDoubleOneLine = many nbDoubleChar

-- 112
sDoubleEscaped :: Column -> Parser String
sDoubleEscaped n =
    (many sWhite <* char '\\') <++>
    (bNonContent *> concatMany (try $ lEmpty n FlowIn)) <*
    sFlowLinePrefix n

-- 113
sDoubleBreak :: Column -> Parser String
sDoubleBreak n = try (sDoubleEscaped n) <|> sFlowFolded n

-- 114
nbNsDoubleInLine :: Parser String
nbNsDoubleInLine = concatMany (try $ many sWhite <++> one nsDoubleChar)

-- 115
sDoubleNextLine :: Column -> Parser String
sDoubleNextLine n =
    sDoubleBreak n <++>
    option
        ""
        (try $
         nsDoubleChar <:> nbNsDoubleInLine <++>
         (try (sDoubleNextLine n) <|> "" <$ skipMany sWhite))

-- 116
nbDoubleMultiLine :: Column -> Parser String
nbDoubleMultiLine n =
    nbNsDoubleInLine <++> (sDoubleNextLine n <|> "" <$ skipMany sWhite)

-- 117
cQuotedQuote :: Parser Char
cQuotedQuote = '\'' <$ try (string "''")

-- 118
nbSingleChar :: Parser Char
nbSingleChar =
    try (notFollowedBy (char '\'') *> nbJson) <|>
    cQuotedQuote <?> "single-quoted character"

-- 119
nsSingleChar :: Parser Char
nsSingleChar =
    notFollowedBy sWhite *>
    nbSingleChar <?> "single-quoted non-whitespace character"

-- 120
cSingleQuoted :: Column -> Context -> Parser String
cSingleQuoted n c = char '\'' *> nbSingleText n c <* char '\''

-- 121
nbSingleText :: Column -> Context -> Parser String
nbSingleText n FlowOut  = nbSingleMultiLine n
nbSingleText n FlowIn   = nbSingleMultiLine n
nbSingleText _ BlockKey = nbSingleOneLine
nbSingleText _ FlowKey  = nbSingleOneLine
nbSingleText _ c        = badContext c

-- 122
nbSingleOneLine :: Parser String
nbSingleOneLine = many $ try nbSingleChar

-- 123
nbNsSingleInLine :: Parser String
nbNsSingleInLine = concatMany (try $ many sWhite <++> one nsSingleChar)

-- 124
sSingleNextLine :: Column -> Parser String
sSingleNextLine n =
    sFlowFolded n <++>
    option
        ""
        (try $
         nsSingleChar <:> nbNsSingleInLine <++>
         (try (sSingleNextLine n) <|> many sWhite))

-- 125
nbSingleMultiLine :: Column -> Parser String
nbSingleMultiLine n =
    nbNsSingleInLine <++> (sSingleNextLine n <|> "" <$ skipMany sWhite)

-- 126
nsPlainFirst :: Context -> Parser Char
nsPlainFirst c =
    notFollowedBy cIndicator *> nsChar <|>
    oneOf "?:-" *> try (lookAhead $ nsPlainSafe c)

-- 127
nsPlainSafe :: Context -> Parser Char
nsPlainSafe FlowOut  = nsPlainSafeOut
nsPlainSafe FlowIn   = nsPlainSafeIn
nsPlainSafe BlockKey = nsPlainSafeOut
nsPlainSafe FlowKey  = nsPlainSafeIn
nsPlainSafe c        = badContext c

-- 128
nsPlainSafeOut :: Parser Char
nsPlainSafeOut = nsChar

-- 129
nsPlainSafeIn :: Parser Char
nsPlainSafeIn = notFollowedBy cFlowIndicator *> nsChar

-- 130
nsPlainChar :: Context -> Parser Char
nsPlainChar c =
    notFollowedBy (oneOf ":#") *> nsPlainSafe c <|>
    (do pos <- getPosition
        plainCharPos <- lastPlainCharPos <$> getState
        if plainCharPos == Just pos
            then char '#'
            else empty) <|>
    (char ':' <* lookAhead (nsPlainSafe c))

-- 131
nsPlain :: Column -> Context -> Parser String
nsPlain n FlowOut  = nsPlainMultiLine n FlowOut
nsPlain n FlowIn   = nsPlainMultiLine n FlowIn
nsPlain _ BlockKey = nsPlainOneLine BlockKey
nsPlain _ FlowKey  = nsPlainOneLine FlowKey
nsPlain _ c        = badContext c

-- 132
nbNsPlainInLine :: Context -> Parser String
nbNsPlainInLine c = many (try $ many sWhite *> nsPlainChar c)

-- 133
nsPlainOneLine :: Context -> Parser String
nsPlainOneLine c = nsPlainFirst c <:> nbNsPlainInLine c

-- 134
sNsPlainNextLine :: Column -> Context -> Parser String
sNsPlainNextLine n c = sFlowFolded n <++> (nsPlainChar c <:> nbNsPlainInLine c)

-- 135
nsPlainMultiLine :: Column -> Context -> Parser String
nsPlainMultiLine n c =
    nsPlainOneLine c <++> concatMany (try $ sNsPlainNextLine n c)

-- 136
inFlow :: Context -> Parser Context
inFlow FlowOut  = return FlowIn
inFlow FlowIn   = return FlowIn
inFlow BlockKey = return FlowKey
inFlow FlowKey  = return FlowKey
inFlow c        = badContext c

-- 137
cFlowSequence :: Column -> Context -> Parser Node
cFlowSequence n c =
    SeqNode <$ char '[' <* optional (try $ sSeparate n c) <*>
    option [] (try $ inFlow c >>= nsSFlowSeqEntries n) <*
    char ']'

-- 138
nsSFlowSeqEntries :: Column -> Context -> Parser [Node]
nsSFlowSeqEntries n c =
    (:) <$> nsFlowSeqEntry n c <* optional (try $ sSeparate n c) <*>
    option
        []
        (try $
         char ',' *> optional (sSeparate n c) *>
         option [] (try $ nsSFlowSeqEntries n c))

-- 139
nsFlowSeqEntry :: Column -> Context -> Parser Node
nsFlowSeqEntry n c = MapNode . (:[]) <$> nsFlowPair n c <|> nsFlowNode n c

-- 140
cFlowMapping :: Column -> Context -> Parser Node
cFlowMapping n c =
    MapNode <$ char '{' <* optional (try $ sSeparate n c) <*>
    option [] (try $ inFlow c >>= nsSFlowMapEntries n) <*
    char '}'

-- 141
nsSFlowMapEntries :: Column -> Context -> Parser [KeyVal]
nsSFlowMapEntries n c =
    (:) <$> nsFlowMapEntry n c <* optional (try $ sSeparate n c) <*>
    option
        []
        (try $
         char ',' *> optional (try $ sSeparate n c) *>
         option [] (try $ nsSFlowMapEntries n c))

-- 142
nsFlowMapEntry :: Column -> Context -> Parser KeyVal
nsFlowMapEntry n c =
    char '?' *> sSeparate n c *> nsFlowMapExplicitEntry n c <|>
    nsFlowMapImplicitEntry n c

-- 143
nsFlowMapExplicitEntry :: Column -> Context -> Parser KeyVal
nsFlowMapExplicitEntry n c = try' (nsFlowMapImplicitEntry n c) <|>
    KeyVal <$> eNode <*> eNode

-- 144
nsFlowMapImplicitEntry :: Column -> Context -> Parser KeyVal
nsFlowMapImplicitEntry n c =
    try' (nsFlowMapYamlKeyEntry n c) <|> try (cNsFlowMapEmptyKeyEntry n c) <|>
    cNsFlowMapJsonKeyEntry n c

-- 145
nsFlowMapYamlKeyEntry :: Column -> Context -> Parser KeyVal
nsFlowMapYamlKeyEntry n c =
    KeyVal <$> nsFlowYamlNode n c <*>
    (try (optional (try $ sSeparate n c) *> cNsFlowMapSeparateValue n c) <|>
     eNode)

-- 146
cNsFlowMapEmptyKeyEntry :: Column -> Context -> Parser KeyVal
cNsFlowMapEmptyKeyEntry n c = KeyVal <$> eNode <*> cNsFlowMapSeparateValue n c

-- 147
cNsFlowMapSeparateValue :: Column -> Context -> Parser Node
cNsFlowMapSeparateValue n c =
    char ':' *>
    notFollowedBy (nsPlainSafe c) *>
    (try (sSeparate n c *> nsFlowNode n c) <|> eNode)

-- 148
cNsFlowMapJsonKeyEntry :: Column -> Context -> Parser KeyVal
cNsFlowMapJsonKeyEntry n c =
    KeyVal <$> cFlowJsonNode n c <*>
    (optional (try $ sSeparate n c) *> cNsFlowMapAdjacentValue n c <|> eNode)

-- 149
cNsFlowMapAdjacentValue :: Column -> Context -> Parser Node
cNsFlowMapAdjacentValue n c =
    char ':' *> optional (try $ sSeparate n c) *> nsFlowNode n c <|> eNode

-- 150
nsFlowPair :: Column -> Context -> Parser KeyVal
nsFlowPair n c =
    char '?' *> sSeparate n c *> nsFlowMapExplicitEntry n c <|>
    nsFlowPairEntry n c

-- 151
nsFlowPairEntry :: Column -> Context -> Parser KeyVal
nsFlowPairEntry n c =
    try' (nsFlowPairYamlKeyEntry n c) <|> try' (cNsFlowMapEmptyKeyEntry n c) <|>
    cNsFlowPairJsonKeyEntry n c

-- 152
nsFlowPairYamlKeyEntry :: Column -> Context -> Parser KeyVal
nsFlowPairYamlKeyEntry n c =
    KeyVal <$> nsSImplicitYamlKey FlowKey <*> cNsFlowMapSeparateValue n c

-- 153
cNsFlowPairJsonKeyEntry :: Column -> Context -> Parser KeyVal
cNsFlowPairJsonKeyEntry n c =
    KeyVal <$> cSImplicitJsonKey FlowKey <*> cNsFlowMapAdjacentValue n c

-- 154
nsSImplicitYamlKey :: Context -> Parser Node
nsSImplicitYamlKey c =
    limitLookahead $ nsFlowYamlNode undefined c <* optional (try sSeparateInLine)

-- 155
cSImplicitJsonKey :: Context -> Parser Node
cSImplicitJsonKey c = limitLookahead $
    cFlowJsonNode undefined c <* optional (try sSeparateInLine)

-- | Parse with lookahead limited to 1024 characters, as required by
-- YAML spec.
limitLookahead :: Parser a -> Parser a
limitLookahead p = do
    (input, input') <- splitAt 1024 <$> getInput
    setInput input
    r <- p
    unused <- getInput
    setInput (unused ++ input')
    return r

-- 156
nsFlowYamlContent :: Column -> Context -> Parser String
nsFlowYamlContent = nsPlain

-- 157
cFlowJsonContent :: Column -> Context -> Parser Node
cFlowJsonContent n c =
    try' (cFlowSequence n c) <|> try' (cFlowMapping n c) <|>
    try' (ScalarNode <$> cSingleQuoted n c) <|>
    ScalarNode <$> cDoubleQuoted n c

-- 158
nsFlowContent :: Column -> Context -> Parser Node
nsFlowContent n c = try' (ScalarNode <$> nsFlowYamlContent n c) <|> cFlowJsonContent n c

-- 159
nsFlowYamlNode :: Column -> Context -> Parser Node
nsFlowYamlNode n c =
    try' cNsAliasNode <|> try' (ScalarNode <$> nsFlowYamlContent n c) <|>
    cNsProperties n c *>
    (try (ScalarNode <$ sSeparate n c <*> nsFlowYamlContent n c) <|> eScalar)

-- 160
cFlowJsonNode :: Column -> Context -> Parser Node
cFlowJsonNode n c =
    optional (try $ cNsProperties n c *> sSeparate n c) *> cFlowJsonContent n c

-- 161
nsFlowNode :: Column -> Context -> Parser Node
nsFlowNode n c =
    try' cNsAliasNode <|> try' (nsFlowContent n c) <|>
    (cNsProperties n c >> (try (sSeparate n c >> nsFlowContent n c) <|> eScalar))

data Chomping = Strip | Clip | Keep
data Indentation = Indent Int | AutoDetect

-- 162
cBBlockHeader :: Parser (Indentation, Chomping)
cBBlockHeader =
    (((,) <$> cIndentationIndicator <*> cChompingIndicator) <|>
     (flip (,) <$> cChompingIndicator <*> cIndentationIndicator)) <*
    sBComment

-- 163
cIndentationIndicator :: Parser Indentation
cIndentationIndicator =
    try' (Indent . digitToInt <$> nsDecDigit) <|> pure AutoDetect

-- 164
cChompingIndicator :: Parser Chomping
cChompingIndicator = Strip <$ char '-' <|> Keep <$ char '+' <|> pure Clip

-- 165
bChompedLast :: Chomping -> Parser String
bChompedLast Strip = "" <$ bNonContent
bChompedLast Clip  = bAsLineFeed
bChompedLast Keep  = bAsLineFeed

-- 166
lChompedEmpty :: Column -> Chomping -> Parser String
lChompedEmpty n Strip = lStripEmpty n
lChompedEmpty n Clip  = lStripEmpty n
lChompedEmpty n Keep  = lKeepEmpty n

-- 167
lStripEmpty :: Column -> Parser String
lStripEmpty n =
    many (try $ sIndentLe n *> bNonContent) *>
    option "" (try $ lTrailComments n)

-- 168
lKeepEmpty :: Column -> Parser String
lKeepEmpty n =
    concatMany (try $ lEmpty n BlockIn) <++> option "" (try $ lTrailComments n)

-- 169
lTrailComments :: Column -> Parser String
lTrailComments n =
    sIndentLe n *> cNbCommentText <* bComment <* many (try lComment)

-- 170
cLPlusLiteral :: Column -> Parser String
cLPlusLiteral n = do
    _ <- char '|'
    (m, t) <- cBBlockHeader
    -- TODO: This is probably wrong.
    let n' =
            case m of
                Indent m'  -> n + m'
                AutoDetect -> n
    lLiteralContent n' t

-- 171
lNbLiteralText :: Column -> Parser String
lNbLiteralText n =
    concatMany (try $ lEmpty n BlockIn) <++> (sIndent n *> many1 nbChar)

-- 172
bNbLiteralNext :: Column -> Parser String
bNbLiteralNext n = bAsLineFeed <++> lNbLiteralText n

-- 173
lLiteralContent :: Column -> Chomping -> Parser String
lLiteralContent n t =
    option
        ""
        (try $
         lNbLiteralText n <++> concatMany (try $ bNbLiteralNext n) <++>
         bChompedLast t) <++>
    lChompedEmpty n t

-- 174
cLPlusFolded :: Column -> Parser String
cLPlusFolded n = do
    char '>'
    (m, t) <- cBBlockHeader
    -- TODO: This is probably wrong.
    let n' =
            case m of
                Indent m'  -> n + m'
                AutoDetect -> n
    lFoldedContent n' t

-- 175
sNbFoldedText :: Column -> Parser String
sNbFoldedText n = sIndent n *> (nsChar <:> many nbChar)

-- 176
lNbFoldedLines :: Column -> Parser String
lNbFoldedLines n =
    sNbFoldedText n <++>
    concatMany (try $ bLFolded n BlockIn <++> sNbFoldedText n)

-- 177
sNbSpacedText :: Column -> Parser String
sNbSpacedText n = sIndent n *> sWhite *> many nbChar

-- 178
bLSpaced :: Column -> Parser String
bLSpaced n = bAsLineFeed <++> concatMany (try $ lEmpty n BlockIn)

-- 179
lNbSpacedLines :: Column -> Parser String
lNbSpacedLines n =
    sNbSpacedText n <++> concatMany (try $ bLSpaced n <++> sNbSpacedText n)

-- 180
lNbSameLines :: Column -> Parser String
lNbSameLines n =
    concatMany (try $ lEmpty n BlockIn) <++>
    (try (lNbFoldedLines n) <|> lNbSpacedLines n)

-- 181
lNbDiffLines :: Column -> Parser String
lNbDiffLines n =
    lNbSameLines n <++>
    concatMany (try $ bAsLineFeed <++> lNbSameLines n)

-- 182
lFoldedContent :: Column -> Chomping -> Parser String
lFoldedContent n t =
    optional (try $ lNbDiffLines n >> bChompedLast t) *> lChompedEmpty n t

-- 183
lPlusBlockSequence :: Column -> Parser Node
lPlusBlockSequence n = do
    sIndent (n + 1)
    m <- sIndent'
    let n' = n + 1 + m
    SeqNode <$>
        ((:) <$> cLBlockSeqEntry n' <*>
         many (try $ sIndent n' *> cLBlockSeqEntry n'))

-- 184
cLBlockSeqEntry :: Column -> Parser Node
cLBlockSeqEntry n =
    char '-' *> notFollowedBy nsChar *> sLPlusBlockIndented n BlockIn
    <?> "block sequence entry"

-- 185
sLPlusBlockIndented :: Column -> Context -> Parser Node
sLPlusBlockIndented n c =
    try
        (do m <- sIndent'
            let n' = n + 1 + m
            try' (nsLCompactSequence n') <|> nsLCompactMapping n') <|>
    try (sLPlusBlockNode n c) <|>
    eNode <* sLComments

-- 186
nsLCompactSequence :: Column -> Parser Node
nsLCompactSequence n =
    SeqNode <$>
    ((:) <$> cLBlockSeqEntry n <*> many (try $ sIndent n *> cLBlockSeqEntry n)) <?>
    "compact sequence"

-- 187
lPlusBlockMapping :: Column -> Parser Node
lPlusBlockMapping n =
    ruleN 187 "l+block-mapping" n $ do
        sIndent (n + 1)
        m <- sIndent'
        let n' = n + 1 + m
        -- traceM $ "in 187: n=" ++ show n ++ ", n'=" ++ show n' ++ ", m=" ++ show m
        -- input <- getInput
        -- traceM $ "input=" ++ show (take 10 input)
        MapNode <$>
            ((:) <$> nsLBlockMapEntry n' <*>
             many (try $ sIndent n' *> nsLBlockMapEntry n'))

-- 188
nsLBlockMapEntry :: Column -> Parser KeyVal
nsLBlockMapEntry n =
    ruleN 188 "ns-l-block-map-entry" n $ do
        -- traceM ("in 188 n=" ++ show n)
        -- input <- getInput
        -- traceM $ "input=" ++ show (take 10 input)
        try (cLBlockMapExplicitEntry n) <|> nsLBlockMapImplicitEntry n

-- 189
cLBlockMapExplicitEntry :: Column -> Parser KeyVal
cLBlockMapExplicitEntry n =
    ruleN 189 "c-l-block-map-explicit-entry" n $
    KeyVal <$> cLBlockMapExplicitKey n <*>
    (try' (lBlockMapExplicitValue n) <|> eNode)

-- 190
cLBlockMapExplicitKey :: Column -> Parser Node
cLBlockMapExplicitKey n =
    ruleN 190 "c-l-block-map-explicit-key" n $
    char '?' *> sLPlusBlockIndented n BlockOut

-- 191
lBlockMapExplicitValue :: Column -> Parser Node
lBlockMapExplicitValue n =
    ruleN 191 "l-block-map-explicit-value" n $
    sIndent n *> char ':' *> sLPlusBlockIndented n BlockOut

-- 192
nsLBlockMapImplicitEntry :: Column -> Parser KeyVal
nsLBlockMapImplicitEntry n =
    ruleN 192 "ns-l-block-map-implicit-entry" n $
    KeyVal <$> (try' nsSBlockMapImplicitKey <|> eNode) <*>
    cLBlockMapImplicitValue n

-- 193
nsSBlockMapImplicitKey :: Parser Node
nsSBlockMapImplicitKey =
    rule 193 "ns-s-block-map-implicit-key" $
    try' (cSImplicitJsonKey BlockKey) <|> nsSImplicitYamlKey BlockKey

-- 194
cLBlockMapImplicitValue :: Column -> Parser Node
cLBlockMapImplicitValue n =
    ruleN 194 "c-l-block-map-implicit-value" n $
    char ':' *> (try' (sLPlusBlockNode n BlockOut) <|> eNode <* sLComments)

-- 195
nsLCompactMapping :: Column -> Parser Node
nsLCompactMapping n =
    ruleN 195 "ns-l-compact-mapping" n $
    MapNode <$>
    ((:) <$> nsLBlockMapEntry n <*> many (try $ sIndent n *> nsLBlockMapEntry n))

-- 196
sLPlusBlockNode :: Column -> Context -> Parser Node
sLPlusBlockNode n c =
    ruleNC 196 "s-l+block-node" n c $
    try' (sLPlusBlockInBlock n c) <|> sLPlusFlowInBlock n

-- 197
sLPlusFlowInBlock :: Column -> Parser Node
sLPlusFlowInBlock n =
    sSeparate (n + 1) FlowOut *> nsFlowNode (n + 1) FlowOut <* sLComments

-- 198
sLPlusBlockInBlock :: Column -> Context -> Parser Node
sLPlusBlockInBlock n c =
    try' (sLPlusBlockScalar n c) <|> sLPlusBlockCollection n c

-- 199
sLPlusBlockScalar :: Column -> Context -> Parser Node
sLPlusBlockScalar n c =
    ScalarNode <$ sSeparate (n + 1) c <*
    optional (try $ cNsProperties (n + 1) c <* sSeparate (n + 1) c) <*>
    (try' (cLPlusLiteral n) <|> cLPlusFolded n)

-- 200
sLPlusBlockCollection :: Column -> Context -> Parser Node
sLPlusBlockCollection n c =
    optional (try $ sSeparate (n + 1) c >> cNsProperties (n + 1) c) *>
    sLComments *>
    ((seqSpaces n c >>= lPlusBlockSequence) <|> lPlusBlockMapping n)

-- 201
seqSpaces :: Column -> Context -> Parser Column
seqSpaces n BlockOut = return $ n - 1
seqSpaces n BlockIn  = return $ n
seqSpaces _ c        = badContext c

-- 202
lDocumentPrefix :: Parser ()
lDocumentPrefix = optional (try cByteOrderMark) >> skipMany (try lComment)

lDocumentPrefix' :: Parser ()
lDocumentPrefix' =
    (cByteOrderMark >> skipMany (try lComment)) <|> skipMany1 (try lComment)

-- 203
cDirectivesEnd :: Parser ()
cDirectivesEnd = () <$ try (string "---")

-- 204
cDocumentEnd :: Parser ()
cDocumentEnd = () <$ try (string "...")

-- 205
lDocumentSuffix :: Parser ()
lDocumentSuffix = cDocumentEnd >> sLComments >> return ()

-- 206
cForbidden :: Parser ()
cForbidden = do
    lineStart
    (try' cDirectivesEnd <|> cDocumentEnd)
    (() <$ try' bChar <|> () <$ try' sWhite <|> eof)
    return ()

-- 207
lBareDocument :: Parser Document
lBareDocument = do
    notFollowedBy cForbidden
    node <- sLPlusBlockNode (-1) BlockIn
    return $ Document node

-- 208
lExplicitDocument :: Parser Document
lExplicitDocument =
    cDirectivesEnd *>
    (try' lBareDocument <|> (Document <$> eNode) <* sLComments)

-- 209
lDirectiveDocument :: Parser Document
lDirectiveDocument = many1 (try lDirective) *> lExplicitDocument

-- 210
lAnyDocument :: Parser Document
lAnyDocument =
    try' lDirectiveDocument <|> try' lExplicitDocument <|> lBareDocument

-- 211
lYamlStream :: Parser Stream
lYamlStream = do
    _ <- many (try lDocumentPrefix')
    maybeDoc <- optionList (try lAnyDocument)
    docs <-
        concatMany
            ((many1 (try lDocumentSuffix) *> many (try lDocumentPrefix) *>
              optionList (try lAnyDocument)) <|>
             (many1 (try lDocumentPrefix') *> optionList (try lExplicitDocument)) <|>
             one lExplicitDocument)
    eof
    return . Stream $ maybeDoc ++ docs
