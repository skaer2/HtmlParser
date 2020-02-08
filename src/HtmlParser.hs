module HtmlParser where

import           Control.Applicative          ((<|>))

-- HtmlDocument = Tag
-- Tag = ClosableTag | NotClosableTag
-- NotClosableTag = < TagName Arguments >
-- ClosableTag = < TagName Arguments > List Content </ TagName >
-- TagName = Text
-- Content = Tag | Text
-- ContentEnd = ""
-- Text = neskolko char
--
--
import           Text.ParserCombinators.ReadP

data Tag
    = TagClosable ClosableTag
    | TagNotClosable NotClosableTag
    deriving (Eq, Show)

data NotClosableTag =
    NotClosableTag String
    deriving (Eq, Show)

data ClosableTag =
    ClosableTag String [Content]
    deriving (Eq, Show)

data Content
    = TextContent String
    | TagContent Tag
    deriving (Eq, Show)

showResult :: [(a, String)] -> [(a, String)]
showResult = filter (\(x, leftovers) -> if leftovers == "" then True else False)

tagSymbol :: Char -> Bool
tagSymbol c = (c == tagSymbolOpen) || (c == tagSymbolClosed)

tagSymbolOpen :: Char
tagSymbolOpen = '<'

tagSymbolClosed :: Char
tagSymbolClosed = '>'

isWhitespace :: Char -> Bool
isWhitespace c = elem c wschars
  where
    wschars = " \t\r\n"

text :: Char -> Bool
text = not . tagSymbol

textParser :: ReadP String
textParser = many1 $ satisfy text

tagNameParser :: ReadP String
tagNameParser = many1 $ satisfy $ not . ((||) <$> tagSymbols <*> isWhitespace)
    where
        tagSymbols = (||) <$> tagSymbol <*> isSlashSymbol
        isSlashSymbol c = c == slashSymbol 
        slashSymbol = '/'

tagParser :: ReadP Tag
tagParser = closableTagParser <|> notClosableTagParser

notClosableTagParser :: ReadP Tag
notClosableTagParser = do
    tagName <- openingTagParser
    return (TagNotClosable $ NotClosableTag tagName)

closableTagParser :: ReadP Tag
closableTagParser = do
    tagName <- openingTagParser
    content <- contsParser
    closingTagParser tagName
    return (TagClosable $ ClosableTag tagName content)

openingTagParser :: ReadP String
openingTagParser = do
    char '<'
    skipSpaces
    tagName <- tagNameParser
    skipSpaces
    char '>'
    return tagName

contsParser :: ReadP [Content]
contsParser = do
    content <- many contentParser
    return content

closingTagParser :: String -> ReadP ()
closingTagParser tagName = do
    skipSpaces
    char '<'
    char '/'
    string tagName
    char '>'
    return ()

contentParser :: ReadP Content
contentParser = (TagContent <$> tagParser) <|> (TextContent <$> munch1 text)
