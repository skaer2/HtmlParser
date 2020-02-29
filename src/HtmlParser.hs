module HtmlParser where

import           Control.Applicative          ((<|>))
import           Data.List
import           Text.ParserCombinators.ReadP

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
data Tag
    = TagClosable ClosableTag 
    | TagNotClosable NotClosableTag 
    deriving (Eq, Show)

data NotClosableTag =
    NotClosableTag String [Attribute]
    deriving (Eq, Show)

data ClosableTag =
    ClosableTag String [Attribute] [Content]
    deriving (Eq, Show)

data Attribute = Attribute String String
    deriving (Eq, Show)

data Content
    = TextContent String
    | TagContent Tag
    deriving (Eq, Show)

printTree :: (Show a) => [(a, String)] -> String
printTree tree = helper (-1) $ show tree
  where
    helper _ [] = []
    helper deep (x:xs) 
        | x `elem` charsToDeeper = '\n':(tabs 1) ++ (x : ' ' : (helperDeeper xs) )
        | x `elem` charsToHigher = '\n':(tabs (0)) ++ (x : (helperHigher xs) )
        | x `elem` charsToStay = '\n':(tabs 0)++ (x : ' ' : (helperStay xs))
        | otherwise = x : helperStay xs
      where
        tabs n = replicate (deep + n) '\t'
        helperDeeper = helper (deep + 1)
        helperStay = helper deep
        helperHigher = helper (deep - 1)
        charsToDeeper = "(["
        charsToStay = ","
        charsToHigher = ")]"

showResult :: [(a, String)] -> [(a, String)]
showResult =
    filter
        (\(x, leftovers) ->
             if leftovers == ""
                 then True
                 else False)

text :: Char -> Bool
text = not . isTagSymbol

textParser :: ReadP String
textParser = many1 $ satisfy text

tagNameParser :: ReadP String
tagNameParser = many1 $ satisfy $ not . ((||) <$> isTagSymbol <*> isWhitespace)

isTagSymbol :: Char -> Bool 
isTagSymbol c = c `elem` tagSymbols 

tagSymbols :: String
tagSymbols = "<>/"

isWhitespace :: Char -> Bool
isWhitespace c = elem c wschars
  where
    wschars = " \t\r\n"

tagParser :: ReadP Tag
tagParser = closableTagParser <|> notClosableTagParser

notClosableTagParser :: ReadP Tag
notClosableTagParser = do
    (tagName, attributes) <- openingTagParser
    attributes <- attrsParser
    return (TagNotClosable $ NotClosableTag tagName attributes)

closableTagParser :: ReadP Tag
closableTagParser = do
    (tagName, attributes) <- openingTagParser
    content <- contsParser
    closingTagParser tagName
    return (TagClosable $ ClosableTag tagName attributes content)

openingTagParser :: ReadP (String, [Attribute])
openingTagParser = do
    char '<'
    skipSpaces
    tagName <- tagNameParser
    skipSpaces
    attributes <- attrsParser
    skipSpaces
    char '>'
    return (tagName, attributes)

attrNameParser :: ReadP String
attrNameParser = many1 $ satisfy $ not . ((||) <$> isAttrSymbols <*> isWhitespace)

isAttrSymbols :: Char -> Bool
isAttrSymbols c = c `elem` attrSymbols

attrSymbols :: String
attrSymbols = "=\""

attrsParser :: ReadP [Attribute]
attrsParser = many attrParser 

attrParser :: ReadP Attribute
attrParser = do
    attrName <- attrNameParser  
    skipSpaces
    char '='
    skipSpaces
    char 'x'
    attrValue <- attrNameParser  
    char 'x'
    return $ Attribute attrName attrValue


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
