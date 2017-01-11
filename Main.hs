{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Text (Text, pack, unpack)
import Data.Maybe
import Options.Applicative 
import Control.Applicative
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import qualified Text.Parsec as P hiding (many, (<|>)) 
import Data.Functor.Identity (Identity)

data Options = Options {
      indent :: Bool
    , templateOpt :: TemplateOpt
    , injects :: [RawInject]
    } deriving Show

type XPath = String

data TemplateOpt = TemplateFile FilePath 
                 | TemplateString String 
                   deriving Show
data RawInject = RawInjectFile String
               | RawInjectString String
            deriving Show

data Op = ReplaceChild | ReplaceNode deriving Show

data Inject = InjectFile (FilePath, Op, XPath)
            | InjectString (String, Op, XPath)
            deriving Show

parseTemplateOpt :: Parser TemplateOpt
parseTemplateOpt =
    (TemplateFile 
      <$> strArgument (metavar "TEMPLATE-FILE" <> help "Template file path"))
    <|> 
    (TemplateString 
      <$> strOption (short 'e' <> metavar "TEMPLATE-STRING" <> help "Template as inline string"))

parseInjectRaw :: Parser RawInject
parseInjectRaw = 
      -- use -[OP]XPATH for STDIN
          (RawInjectString <$> (strOption (short 's' <> metavar "STRING[OP]XPATH")))
      <|> (RawInjectFile <$> 
              (strOption 
                  (short 'f' 
                  <> metavar "FILE[OP]XPATH" 
                  <> help "Use -[OP]XPATH to use STDIN")))

options :: Parser Options
options = Options 
    <$> flag False True (short 'i' <> help "Pretty indent. Default false.")
    <*> parseTemplateOpt 
    <*> many parseInjectRaw

opts :: ParserInfo Options
opts = info (helper <*> options) 
          (fullDesc <> header "injecthtml"
          <> progDesc "OP expressions are >>[XPATH] to insert children and ^^[XPATH] to replace node")

-- Warning, if this is full HTML document, it will strip off the doctype
-- <!DOCTYPE html>
-- injecthtml is better used with smaller templates that get wrapped in layout later
-- in the pipeline.

main = do
    o@Options{..} <- execParser opts
    let injects' = map parseInject' injects
    injects'' :: [(XPath, Op, String)] <- mapM loadInject injects'
    template <- case templateOpt of
                  TemplateFile f -> readFile f
                  TemplateString s -> return s
    let indent' = if indent then yes else no
    res <- runX (processTemplate indent' template injects'') 
    mapM putStrLn res


parseInject' :: RawInject  -> Inject
parseInject' (RawInjectFile x) = InjectFile $ parseInject x
parseInject' (RawInjectString x) = InjectString $ parseInject x

parseInject :: String -> (String, Op, XPath)
parseInject s = 
    let (op, sep) = if T.isInfixOf ">>" (T.pack s) 
                    then (ReplaceChild, ">>")
                    else (ReplaceNode, "^^")
        (x,y) = T.breakOn sep  (T.pack s)
    in  (unpack x, op, drop 2 . unpack $  y)

loadInject :: Inject -> IO (FilePath, Op, XPath)
loadInject (InjectFile (filePath, op, xpath)) 
    | filePath == "-" = (,,) <$> getContents <*> pure op <*> pure xpath
    | otherwise       = (,,) <$> readFile filePath <*> pure op <*> pure xpath
loadInject (InjectString x) = return x


processTemplate indent html injects = 
      readString [withValidate no, withParseHTML yes, withInputEncoding utf8] html
      >>> setTraceLevel 0
      >>> (foldl (>>>) this . map process $ injects)
      >>> writeDocumentToString [withIndent indent, withOutputHTML, withXmlPi no]

process (replacement, op, xpath') = 
    processXPathTrees 
      (
        let s = readString [withValidate no, withParseHTML yes, withInputEncoding utf8] replacement
        in case op of
              ReplaceChild -> replaceChildren s
              ReplaceNode -> s
      ) xpath'

