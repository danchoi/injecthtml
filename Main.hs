{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Options.Applicative 
import Options.Applicative.Builder
import Options.Applicative.Types (readerAsk)
import Control.Applicative
import Text.XML.HXT.Core
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows

data Options = Options {
      templateOpt :: TemplateOpt
    , injects :: [Inject]
    } deriving Show

type XPath = String

data TemplateOpt = TemplateFile FilePath 
                 | TemplateString String 
                   deriving Show

data Inject = InjectSTDIN XPath 
            | InjectFile (XPath, FilePath)
            | InjectString (XPath, String)
            deriving Show

parseTemplateOpt :: Parser TemplateOpt
parseTemplateOpt =
    (TemplateFile 
      <$> strArgument (metavar "TEMPLATE-FILE" <> help "Template file path"))
    <|> 
    (TemplateString 
      <$> strOption (short 'e' <> metavar "TEMPLATE-STRING" <> help "Template as inline string"))

parseInject :: Parser Inject
parseInject = 
      (InjectSTDIN <$> strArgument (metavar "XPATH"))
      <|> InjectFile <$> (parseInjectOpt <$> (strOption (short 'f' <> metavar "FILE@XPATH")))
      <|> InjectString <$> (parseInjectOpt <$> (strOption (short 's' <> metavar "STRING@XPATH")))

sepChar = '#'
parseInjectOpt = (takeWhile (/= sepChar)) &&& (drop 1 . dropWhile (/= sepChar)) 

 
options :: Parser Options
options = Options 
    <$> parseTemplateOpt 
    <*> many parseInject

opts :: ParserInfo Options
opts = info (helper <*> options) 
          (fullDesc <> header "injecthtml"
          <> progDesc "HTML template inject")

main = do
    o@Options{..} <- execParser opts
    print o

    template <- case templateOpt of
                  TemplateFile f -> readFile f
                  TemplateString s -> return s
    inject <- TL.getContents
    r <- run True template
    putStrLn r


run :: Bool -> String -> IO String
run indent rawHTML = do
    let indent' = if indent then yes else no
    res <- runX (processTemplate indent' rawHTML)  
    return . concat $ res


processTemplate indent html = 
      readString [withValidate no, withParseHTML yes, withInputEncoding utf8] html
      >>> setTraceLevel 0
      >>> process 
      >>> writeDocumentToString [withIndent indent, withOutputHTML, withXmlPi no]

process = processXPathTrees (
    none  -- replace
  ) "//h2/text()"

