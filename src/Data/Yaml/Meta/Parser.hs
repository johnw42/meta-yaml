module Data.Yaml.Meta.Parser where

import           Control.Applicative (empty)
import           Data.Char           (chr, digitToInt)
import           Numeric             (readHex)
import           Text.Parsec         (Column, Parsec, SourcePos, char, count,
                                      digit, eof, getPosition, getState,
                                      hexDigit, lookAhead, many, many1,
                                      modifyState, notFollowedBy, oneOf, option,
                                      optional, satisfy, skipMany, skipMany1,
                                      sourceColumn, string, try, unexpected,
                                      (<|>))
import           Text.Parsec.Prim    (getInput, setInput)

data ParserState = PS
    { lastPlainCharPos :: Maybe SourcePos
    }

initialState :: ParserState
initialState = PS Nothing

type Parser a = Parsec String ParserState a

-- | Try, but probably not needed.
try' = try

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
cPrintable = satisfy p
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
nbJson = satisfy p
  where
    p '\x9' = True
    p c     = '\x20' <= c && c <= '\x10FFFF'

-- 3
cByteOrderMark :: Parser Char
cByteOrderMark = char '\xFEFF'

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
cSingleQuote = char '\''

-- 19
cDoubleQuote :: Parser Char
cDoubleQuote = char '"'

-- 20
cDirective :: Parser Char
cDirective = char '%'

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
bChar = bLineFeed <|> bCarriageReturn

-- 27
nbChar :: Parser Char
nbChar = do
    notFollowedBy bChar
    notFollowedBy cByteOrderMark
    cPrintable

-- 28, 29, 30
bBreak, bAsLineFeed, bNonContent :: Parser String
bBreak =
    try ((\cr lf -> [cr, lf]) <$> bCarriageReturn <*> bLineFeed) <|>
    (: []) <$> bCarriageReturn <|>
    (: []) <$> bLineFeed
bAsLineFeed = bBreak
bNonContent = bBreak

-- 31, 32, 33
sSpace, sTab, sWhite :: Parser Char
sSpace = char '\x20'
sTab = char '\x9'
sWhite = sSpace <|> sTab

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
nsAsciiLetter = satisfy p
  where
    p c = 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z'

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
nsTagChar = do
    try (notFollowedBy $ char '!')
    try (notFollowedBy cFlowIndicator)
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
nsEsc8Bit = readHexChar <$> count 2 nsHexDigit

-- 60
nsEsc16Bit :: Parser Char
nsEsc16Bit = readHexChar <$> count 4 nsHexDigit

-- 61
nsEsc32Bit :: Parser Char
nsEsc32Bit = readHexChar <$> count 8 nsHexDigit

-- 62
cNsEscChar =
    char '\\' >>
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
sIndent n = () <$ count n sSpace

sIndent' :: Parser Column
sIndent' = length <$> many sSpace

-- 64
sIndentLt :: Int -> Parser ()
sIndentLt 0 = unexpected "space"
sIndentLt 1 = pure ()
sIndentLt n = sSpace >> sIndentLt (n - 1)

-- 65
sIndentLe :: Int -> Parser ()
sIndentLe n = sIndentLt (n + 1)

lineStart :: Parser ()
lineStart = do
    col <- sourceColumn <$> getPosition
    case col of
        1 -> return ()
        _ -> empty

-- 66
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
sLinePrefix n BlockOut = sBlockLinePrefix n
sLinePrefix n BlockIn  = sBlockLinePrefix n
sLinePrefix n FlowOut  = sFlowLinePrefix n
sLinePrefix n FlowIn   = sFlowLinePrefix n
sLinePrefix _ c        = badContext c

badContext :: Context -> Parser a
badContext c = unexpected ("Unexpected context: " ++ show c)

-- 68
sBlockLinePrefix :: Int -> Parser ()
sBlockLinePrefix = sIndent

-- 69
sFlowLinePrefix :: Int -> Parser ()
sFlowLinePrefix n = sIndent n >> sSeparateInLine

-- 70
lEmpty :: Int -> Context -> Parser ()
lEmpty n c = do
    try (sLinePrefix n c) <|> sIndentLt n
    () <$ bAsLineFeed

-- 71
blTrimmed :: Int -> Context -> Parser ()
blTrimmed n c = bNonContent >> skipMany1 (lEmpty n c)

-- 72
bAsSpace :: Parser ()
bAsSpace = () <$ bBreak

-- 73
bLFolded :: Column -> Context -> Parser ()
bLFolded n c = blTrimmed n c <|> bAsSpace

-- 74
sFlowFolded :: Column -> Parser ()
sFlowFolded n = do
    optional sSeparateInLine
    bLFolded n FlowIn
    sFlowLinePrefix n

-- 75
cNbCommentText :: Parser String
cNbCommentText = (:) <$> char '#' <*> many nbChar

-- 76
bComment :: Parser String
bComment = bNonContent <|> "" <$ eof

-- 77
sBComment :: Parser String
sBComment =
    (++) <$> option "" (sSeparateInLine >> option "" cNbCommentText) <*>
    bComment

-- 78
lComment :: Parser String
lComment = sSeparateInLine >> (++) <$> cNbCommentText <*> bComment

-- 79
sLComments :: Parser String
sLComments = ("" <$ lineStart <|> sBComment) <|> concat <$> many lComment

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
lDirective = do
    char '%'
    try nsYamlDirective <|> try nsTagDirective <|> nsReservedDirective
    () <$ sLComments

-- 83
nsReservedDirective :: Parser ()
nsReservedDirective =
    () <$ (nsDirectiveName >> many (sSeparateInLine >> nsDirectiveParameter))

-- 84
nsDirectiveName :: Parser ()
nsDirectiveName = () <$ many1 nsChar

-- 85
nsDirectiveParameter :: Parser ()
nsDirectiveParameter = () <$ many1 nsChar

-- 86
nsYamlDirective :: Parser ()
nsYamlDirective = do
    string "YAML"
    sSeparateInLine
    nsYamlVersion

-- 87
nsYamlVersion :: Parser ()
nsYamlVersion = () <$ do
    many1 nsDecDigit
    char '.'
    many1 nsDecDigit

-- 88
nsTagDirective :: Parser ()
nsTagDirective = do
    string "TAG"
    sSeparateInLine
    cTagHandle
    sSeparateInLine
    nsTagPrefix

-- 89
cTagHandle :: Parser ()
cTagHandle =
    try cNamedTagHandle <|> try cSecondaryTagHande <|> cPrimaryTagHandle

-- 90
cPrimaryTagHandle :: Parser ()
cPrimaryTagHandle = () <$ char '!'

-- 91
cSecondaryTagHande :: Parser ()
cSecondaryTagHande = () <$ string "!!"

-- 92
cNamedTagHandle :: Parser ()
cNamedTagHandle = do
    char '!'
    many1 nsWordChar
    char '!'
    return ()

-- 93
nsTagPrefix :: Parser ()
nsTagPrefix = cNsLocalTagPrefix <|> nsGlobalTagPrefix

-- 94
cNsLocalTagPrefix :: Parser ()
cNsLocalTagPrefix = char '!' >> skipMany nsUriChar

-- 95
nsGlobalTagPrefix :: Parser ()
nsGlobalTagPrefix = nsTagChar >> skipMany nsUriChar

-- 96
cNsProperties :: Column -> Context -> Parser ()
cNsProperties n c =
    (cNsTagProperty >> optional (sSeparate n c >> cNsAnchorProperty)) <|>
    (cNsAnchorProperty >> optional (sSeparate n c >> cNsTagProperty))

-- 97
cNsTagProperty :: Parser ()
cNsTagProperty = try cVerbatimTag <|> try cNsShorthandTag <|> cNonSpecificTag

-- 98
cVerbatimTag :: Parser ()
cVerbatimTag = do
    string "!<"
    many1 nsUriChar
    char '>'
    return ()

-- 99
cNsShorthandTag :: Parser ()
cNsShorthandTag = cTagHandle >> skipMany1 nsTagChar

-- 100
cNonSpecificTag :: Parser ()
cNonSpecificTag = () <$ char '!'

-- 101
cNsAnchorProperty :: Parser ()
cNsAnchorProperty = char '&' >> nsAnchorName

-- 102
nsAnchorChar :: Parser ()
nsAnchorChar = try (notFollowedBy cFlowIndicator) >> (() <$ nsChar)

-- 103
nsAnchorName :: Parser ()
nsAnchorName = skipMany1 nsAnchorChar

-- 104
cNsAliasNode :: Parser ()
cNsAliasNode = char '*' >> nsAnchorName

-- 105
eScalar :: Parser ()
eScalar = return ()

-- 106
eNode :: Parser ()
eNode = eScalar

-- 107
nbDoubleChar :: Parser ()
nbDoubleChar =
    () <$ cNsEscChar <|>
    (try (notFollowedBy $ oneOf "\\\"") >> nbJson >> return ())

-- 108
nsDoubleChar :: Parser ()
nsDoubleChar = try (notFollowedBy sWhite) >> nbDoubleChar

-- 109
cDoubleQuoted :: Column -> Context -> Parser ()
cDoubleQuoted n c = do
    char '"'
    nbDoubleText n c
    char '"'
    return ()

-- 110
nbDoubleText :: Column -> Context -> Parser ()
nbDoubleText n FlowOut  = nbDoubleMultiLine n
nbDoubleText n FlowIn   = nbDoubleMultiLine n
nbDoubleText _ BlockKey = nbDoubleOneLine
nbDoubleText _ FlowKey  = nbDoubleOneLine
nbDoubleText _ c        = badContext c

-- 111
nbDoubleOneLine :: Parser ()
nbDoubleOneLine = skipMany nbDoubleChar

-- 112
sDoubleEscaped :: Column -> Parser ()
sDoubleEscaped n = do
    many sWhite
    char '\\'
    bNonContent
    many (lEmpty n FlowIn)
    sFlowLinePrefix n

-- 113
sDoubleBreak :: Column -> Parser ()
sDoubleBreak n = sDoubleEscaped n <|> sFlowFolded n

-- 114
nbNsDoubleInLine :: Parser ()
nbNsDoubleInLine = skipMany (many sWhite >> nsDoubleChar)

-- 115
sDoubleNextLine :: Column -> Parser ()
sDoubleNextLine n =
    sDoubleBreak n >>
    optional
        (nsDoubleChar >> nbNsDoubleInLine >>
         (sDoubleNextLine n <|> skipMany sWhite))

-- 116
nbDoubleMultiLine :: Column -> Parser ()
nbDoubleMultiLine n = nbNsDoubleInLine >> (sDoubleNextLine n <|> skipMany sWhite)

-- 117
cQuotedQuote :: Parser ()
cQuotedQuote = () <$ string "''"

-- 118
nbSingleChar :: Parser ()
nbSingleChar =
    try (notFollowedBy (char '\'') >> nbJson >> return ()) <|> cQuotedQuote

-- 119
nsSingleChar :: Parser ()
nsSingleChar = try (notFollowedBy sWhite) >> nbSingleChar

-- 120
cSingleQuoted :: Column -> Context -> Parser ()
cSingleQuoted n c = do
    char '\''
    nbSingleText n c
    char '\''
    return ()

-- 121
nbSingleText :: Column -> Context -> Parser ()
nbSingleText n FlowOut  = nbSingleMultiLine n
nbSingleText n FlowIn   = nbSingleMultiLine n
nbSingleText _ BlockKey = nbSingleOneLine
nbSingleText _ FlowKey  = nbSingleOneLine
nbSingleText _ c        = badContext c

-- 122
nbSingleOneLine :: Parser ()
nbSingleOneLine = skipMany nbSingleChar

-- 123
nbNsSingleInLine :: Parser ()
nbNsSingleInLine = skipMany (many sWhite >> nsSingleChar)

-- 124
sSingleNextLine :: Column -> Parser ()
sSingleNextLine n =
    sFlowFolded n >>
    optional
        (nsSingleChar >> nbNsSingleInLine >>
         (sSingleNextLine n <|> skipMany sWhite))

-- 125
nbSingleMultiLine :: Column -> Parser ()
nbSingleMultiLine n =
    nbNsSingleInLine >> (sSingleNextLine n <|> skipMany sWhite)

-- 126
nsPlainFirst :: Context -> Parser ()
nsPlainFirst c =
    (notFollowedBy cIndicator >> nsChar >> return ()) <|>
    (oneOf "?:-" >> try (lookAhead $ nsPlainSafe c))

-- 127
nsPlainSafe :: Context -> Parser ()
nsPlainSafe FlowOut  = nsPlainSafeOut
nsPlainSafe FlowIn   = nsPlainSafeIn
nsPlainSafe BlockKey = nsPlainSafeOut
nsPlainSafe FlowKey  = nsPlainSafeIn
nsPlainSafe c        = badContext c

-- 128
nsPlainSafeOut :: Parser ()
nsPlainSafeOut = nsChar >> return ()

-- 129
nsPlainSafeIn :: Parser ()
nsPlainSafeIn = notFollowedBy cFlowIndicator >> nsChar >> return ()

-- 130
nsPlainChar :: Context -> Parser ()
nsPlainChar c =
    (notFollowedBy (oneOf ":#") >> nsPlainSafe c) <|>
    (do pos <- getPosition
        plainCharPos <- lastPlainCharPos <$> getState
        if plainCharPos == Just pos
            then () <$ char '#'
            else empty) <|>
    (char ':' >> lookAhead (nsPlainSafe c))

-- 131
nsPlain :: Column -> Context -> Parser ()
nsPlain n FlowOut  = nsPlainMultiLine n FlowOut
nsPlain n FlowIn   = nsPlainMultiLine n FlowIn
nsPlain _ BlockKey = nsPlainOneLine BlockKey
nsPlain _ FlowKey  = nsPlainOneLine FlowKey
nsPlain _ c        = badContext c

-- 132
nbNsPlainInLine :: Context -> Parser ()
nbNsPlainInLine c = skipMany (many sWhite >> nsPlainChar c)

-- 133
nsPlainOneLine :: Context -> Parser ()
nsPlainOneLine c = nsPlainFirst c >> nbNsPlainInLine c

-- 134
sNsPlainNextLine :: Column -> Context -> Parser ()
sNsPlainNextLine n c = do
    sFlowFolded n
    nsPlainChar c
    nbNsPlainInLine c

-- 135
nsPlainMultiLine :: Column -> Context -> Parser ()
nsPlainMultiLine n c = do
    nsPlainOneLine c
    many (sNsPlainNextLine n c)
    return ()

-- 136
inFlow :: Context -> Parser Context
inFlow FlowOut  = return FlowIn
inFlow FlowIn   = return FlowIn
inFlow BlockKey = return FlowKey
inFlow FlowKey  = return FlowKey
inFlow c        = badContext c

-- 137
cFlowSequence :: Column -> Context -> Parser ()
cFlowSequence n c = do
    char '['
    optional (sSeparate n c)
    c' <- inFlow c
    optional (nsSFlowSeqEntries n c')
    char ']'
    return ()

-- 138
nsSFlowSeqEntries :: Column -> Context -> Parser ()
nsSFlowSeqEntries n c = do
    nsFlowSeqEntry n c
    optional (sSeparate n c)
    optional
        (do char ','
            optional (sSeparate n c)
            optional (nsSFlowSeqEntries n c))

-- 139
nsFlowSeqEntry :: Column -> Context -> Parser ()
nsFlowSeqEntry n c = nsFlowPair n c <|> nsFlowNode n c

-- 140
cFlowMapping :: Column -> Context -> Parser ()
cFlowMapping n c = do
    char '{'
    optional (sSeparate n c)
    c' <- inFlow c
    optional (nsSFlowMapEntries n c')
    char '}'
    return ()

-- 141
nsSFlowMapEntries :: Column -> Context -> Parser ()
nsSFlowMapEntries n c = do
    nsFlowMapEntry n c
    optional (sSeparate n c)
    optional
        (do char ','
            optional (sSeparate n c)
            optional (nsSFlowMapEntries n c))

-- 142
nsFlowMapEntry :: Column -> Context -> Parser ()
nsFlowMapEntry n c =
    (do char '?'
        sSeparate n c
        nsFlowMapExplicitEntry n c) <|>
    nsFlowMapImplicitEntry n c

-- 143
nsFlowMapExplicitEntry :: Column -> Context -> Parser ()
nsFlowMapExplicitEntry n c = try' (nsFlowMapImplicitEntry n c) <|>
    (eNode >> eNode)

-- 144
nsFlowMapImplicitEntry :: Column -> Context -> Parser ()
nsFlowMapImplicitEntry n c =
    try' (nsFlowMapYamlKeyEntry n c) <|> try (cNsFlowMapEmptyKeyEntry n c) <|>
    cNsFlowMapJsonKeyEntry n c

-- 145
nsFlowMapYamlKeyEntry :: Column -> Context -> Parser ()
nsFlowMapYamlKeyEntry n c =
    nsFlowYamlNode n c >>
    (try (optional (sSeparate n c) >> cNsFlowMapSeparateValue n c) <|> eNode)

-- 146
cNsFlowMapEmptyKeyEntry :: Column -> Context -> Parser ()
cNsFlowMapEmptyKeyEntry n c = eNode >> cNsFlowMapSeparateValue n c

-- 147
cNsFlowMapSeparateValue :: Column -> Context -> Parser ()
cNsFlowMapSeparateValue n c = do
    char ':'
    notFollowedBy (nsPlainSafe c)
    (try (sSeparate n c >> nsFlowNode n c) <|> eNode)

-- 148
cNsFlowMapJsonKeyEntry :: Column -> Context -> Parser ()
cNsFlowMapJsonKeyEntry n c =
    cFlowJsonNode n c >>
    ((try (optional $ sSeparate n c) >> cNsFlowMapAdjacentValue n c) <|> eNode)

-- 149
cNsFlowMapAdjacentValue :: Column -> Context -> Parser ()
cNsFlowMapAdjacentValue n c = do
    char ':'
    (try (optional $ sSeparate n c) >> nsFlowNode n c) <|> eNode
    return ()

-- 150
nsFlowPair :: Column -> Context -> Parser ()
nsFlowPair n c =
    (char '?' >> sSeparate n c >> nsFlowMapExplicitEntry n c) <|>
    nsFlowPairEntry n c

-- 151
nsFlowPairEntry :: Column -> Context -> Parser ()
nsFlowPairEntry n c =
    try' (nsFlowPairYamlKeyEntry n c) <|> try' (cNsFlowMapEmptyKeyEntry n c) <|>
    cNsFlowPairJsonKeyEntry n c

-- 152
nsFlowPairYamlKeyEntry :: Column -> Context -> Parser ()
nsFlowPairYamlKeyEntry n c =
    nsSImplicitYamlKey FlowKey >> cNsFlowMapSeparateValue n c

-- 153
cNsFlowPairJsonKeyEntry :: Column -> Context -> Parser ()
cNsFlowPairJsonKeyEntry n c =
    cSImplicitJsonKey FlowKey >> cNsFlowMapAdjacentValue n c

-- 154
nsSImplicitYamlKey :: Context -> Parser ()
nsSImplicitYamlKey c =
    limitLookahead $ nsFlowYamlNode undefined c >> optional sSeparateInLine

-- 155
cSImplicitJsonKey :: Context -> Parser ()
cSImplicitJsonKey c = limitLookahead $
    cFlowJsonNode undefined c >> optional sSeparateInLine

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
nsFlowYamlContent :: Column -> Context -> Parser ()
nsFlowYamlContent = nsPlain

-- 157
cFlowJsonContent :: Column -> Context -> Parser ()
cFlowJsonContent n c =
    try' (cFlowSequence n c) <|> try' (cFlowMapping n c) <|>
    try' (cSingleQuoted n c) <|>
    cDoubleQuoted n c

-- 158
nsFlowContent :: Column -> Context -> Parser ()
nsFlowContent n c = try' (nsFlowYamlContent n c) <|> cFlowJsonContent n c

-- 159
nsFlowYamlNode :: Column -> Context -> Parser ()
nsFlowYamlNode n c =
    try' cNsAliasNode <|> try' (nsFlowYamlContent n c) <|>
    (cNsProperties n c >> (try (sSeparate n c >> nsFlowYamlContent n c) <|> eScalar))

-- 160
cFlowJsonNode :: Column -> Context -> Parser ()
cFlowJsonNode n c =
    optional (cNsProperties n c >> sSeparate n c) >> cFlowJsonContent n c

-- 161
nsFlowNode :: Column -> Context -> Parser ()
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
bChompedLast :: Chomping -> Parser ()
bChompedLast Strip = () <$ bNonContent
bChompedLast Clip  = () <$ bAsLineFeed
bChompedLast Keep  = () <$ bAsLineFeed

-- 166
lChompedEmpty :: Column -> Chomping -> Parser ()
lChompedEmpty n Strip = lStripEmpty n
lChompedEmpty n Clip  = lStripEmpty n
lChompedEmpty n Keep  = lKeepEmpty n

-- 167
lStripEmpty :: Column -> Parser ()
lStripEmpty n = many (sIndentLe n >> bNonContent) >> optional (lTrailComments n)

-- 168
lKeepEmpty :: Column -> Parser ()
lKeepEmpty n = many (lEmpty n BlockIn) >> optional (lTrailComments n)

-- 169
lTrailComments :: Column -> Parser ()
lTrailComments n = do
    sIndentLe n
    cNbCommentText
    bComment
    many lComment
    return ()

-- 170
cLPlusLiteral :: Column -> Parser ()
cLPlusLiteral n = do
    char '|'
    (m, t) <- cBBlockHeader
    -- TODO: This is probably wrong.
    let n' =
            case m of
                Indent m'  -> n + m'
                AutoDetect -> n
    lLiteralContent n' t

-- 171
lNbLiteralText :: Column -> Parser ()
lNbLiteralText n = do
    many (lEmpty n BlockIn)
    sIndent n
    many1 nbChar
    return ()

-- 172
bNbLiteralNext :: Column -> Parser ()
bNbLiteralNext n = bAsLineFeed >> lNbLiteralText n

-- 173
lLiteralContent :: Column -> Chomping -> Parser ()
lLiteralContent n t =
    optional (lNbLiteralText n >> many (bNbLiteralNext n) >> bChompedLast t) >>
    lChompedEmpty n t

-- 174
cLPlusFolded :: Column -> Parser ()
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
sNbFoldedText :: Column -> Parser ()
sNbFoldedText n = do
    sIndent n
    nsChar
    many nbChar
    return ()

-- 176
lNbFoldedLines :: Column -> Parser ()
lNbFoldedLines n =
    sNbFoldedText n >> skipMany (bLFolded n BlockIn >> sNbFoldedText n)

-- 177
sNbSpacedText :: Column -> Parser ()
sNbSpacedText n = sIndent n >> sWhite >> skipMany nbChar

-- 178
bLSpaced :: Column -> Parser ()
bLSpaced n = bAsLineFeed >> skipMany (lEmpty n BlockIn)

-- 179
lNbSpacedLines :: Column -> Parser ()
lNbSpacedLines n = sNbSpacedText n >> skipMany (bLSpaced n >> sNbSpacedText n)

-- 180
lNbSameLines :: Column -> Parser ()
lNbSameLines n =
    many (lEmpty n BlockIn) >> (lNbFoldedLines n <|> lNbSpacedLines n)

-- 181
lNbDiffLines :: Column -> Parser ()
lNbDiffLines n = lNbSameLines n >> skipMany (bAsLineFeed >> lNbSameLines n)

-- 182
lFoldedContent :: Column -> Chomping -> Parser ()
lFoldedContent n t =
    try (optional $ lNbDiffLines n >> bChompedLast t) >> lChompedEmpty n t

-- 183
lPlusBlockSequence :: Column -> Parser ()
lPlusBlockSequence n = do
    sIndent (n  + 1)
    m <- sIndent'
    let n' = n + 1 + m
    cLBlockSeqEntry n'
    many (sIndent n' >> cLBlockSeqEntry n')
    return ()

-- 184
cLBlockSeqEntry :: Column -> Parser ()
cLBlockSeqEntry n = do
    char '-'
    notFollowedBy nsChar
    sLPlusBlockIndented n BlockIn

-- 185
sLPlusBlockIndented :: Column -> Context -> Parser ()
sLPlusBlockIndented n c =
    (do m <- sIndent'
        let n' = n + 1 + m
        (try' (nsLCompactSequence n') <|> nsLCompactMapping n')) <|>
    sLPlusBlockNode n c <|>
    (eNode >> sLComments >> return ())

-- 186
nsLCompactSequence :: Column -> Parser ()
nsLCompactSequence n =
    cLBlockSeqEntry n >> skipMany (sIndent n >> cLBlockSeqEntry n)

-- 187
lPlusBlockMapping :: Column -> Parser ()
lPlusBlockMapping n = do
    sIndent (n + 1)
    m <- sIndent'
    let n' = n + 1 + m
    nsLBlockMapEntry n'
    many (sIndent n' >> nsLBlockMapEntry n')
    return ()

-- 188
nsLBlockMapEntry :: Column -> Parser ()
nsLBlockMapEntry n = try' (cLBlockMapExplicitEntry n) <|> nsLBlockMapImplicitEntry n

-- 189
cLBlockMapExplicitEntry :: Column -> Parser ()
cLBlockMapExplicitEntry n =
    cLBlockMapExplicitKey n >> (try' (lBlockMapExplicitValue n) <|> eNode)

-- 190
cLBlockMapExplicitKey :: Column -> Parser ()
cLBlockMapExplicitKey n = char '?' >> sLPlusBlockIndented n BlockOut

-- 191
lBlockMapExplicitValue :: Column -> Parser ()
lBlockMapExplicitValue n =
    sIndent n >> char ':' >> sLPlusBlockIndented n BlockOut

-- 192
nsLBlockMapImplicitEntry :: Column -> Parser ()
nsLBlockMapImplicitEntry n =
    (try' nsSBlockMapImplicitKey <|> eNode) >> cLBlockMapImplicitValue n

-- 193
nsSBlockMapImplicitKey :: Parser ()
nsSBlockMapImplicitKey =
    try' (cSImplicitJsonKey BlockKey) <|> nsSImplicitYamlKey BlockKey

-- 194
cLBlockMapImplicitValue :: Column -> Parser ()
cLBlockMapImplicitValue n =
    char ':' >>
    (try' (sLPlusBlockNode n BlockOut) <|> (eNode >> sLComments >> return ()))

-- 195
nsLCompactMapping :: Column -> Parser ()
nsLCompactMapping n =
    nsLBlockMapEntry n >> skipMany (sIndent n >> nsLBlockMapEntry n)

-- 196
sLPlusBlockNode :: Column -> Context -> Parser ()
sLPlusBlockNode n c = try' (sLPlusBlockInBlock n c) <|> sLPlusFlowInBlock n

-- 197
sLPlusFlowInBlock :: Column -> Parser ()
sLPlusFlowInBlock n = do
    sSeparate (n + 1) FlowOut
    nsFlowNode (n + 1) FlowOut
    sLComments
    return ()

-- 198
sLPlusBlockInBlock :: Column -> Context -> Parser ()
sLPlusBlockInBlock n c =
    try' (sLPlusBlockScalar n c) <|> sLPlusBlockCollection n c

-- 199
sLPlusBlockScalar :: Column -> Context -> Parser ()
sLPlusBlockScalar n c = do
    sSeparate (n + 1) c
    optional (cNsProperties (n + 1) c >> sSeparate (n + 1) c)
    (try' (cLPlusLiteral n) <|> cLPlusFolded n)
    return ()

-- 200
sLPlusBlockCollection :: Column -> Context -> Parser ()
sLPlusBlockCollection n c = do
    try (optional $ sSeparate (n + 1) c >> cNsProperties (n + 1) c)
    sLComments
    ((seqSpaces n c >>= lPlusBlockSequence) <|> lPlusBlockMapping n)

-- 201
seqSpaces :: Column -> Context -> Parser Column
seqSpaces n BlockOut = return $ n - 1
seqSpaces n BlockIn  = return $ n
seqSpaces _ c        = badContext c

-- 202
lDocumentPrefix :: Parser ()
lDocumentPrefix = try' (optional cByteOrderMark) >> skipMany lComment

lDocumentPrefix' :: Parser ()
lDocumentPrefix' = (cByteOrderMark >> skipMany lComment) <|> skipMany1 lComment

-- 203
cDirectivesEnd :: Parser ()
cDirectivesEnd = () <$ string "---"

-- 204
cDocumentEnd :: Parser ()
cDocumentEnd = () <$ string "..."

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
lBareDocument :: Parser ()
lBareDocument = do
    notFollowedBy cForbidden
    sLPlusBlockNode (-1) BlockIn

-- 208
lExplicitDocument :: Parser ()
lExplicitDocument =
    cDirectivesEnd >>
    (try' lBareDocument <|> (eNode >> sLComments >> return ()))

-- 209
lDirectiveDocument :: Parser ()
lDirectiveDocument = many1 lDirective >> lExplicitDocument

-- 210
lAnyDocument :: Parser ()
lAnyDocument =
    try' lDirectiveDocument <|> try' lExplicitDocument <|> lBareDocument

-- 211
lYamlStream :: Parser ()
lYamlStream = do
    many (try' lDocumentPrefix')
    try (optional lAnyDocument)
    many
        ((many1 (try' lDocumentSuffix) >> many (try' lDocumentPrefix) >>
          try (optional lAnyDocument)) <|>
         (many1 (try' lDocumentPrefix') >> optional (try' lExplicitDocument)) <|>
         lExplicitDocument)
    return ()
