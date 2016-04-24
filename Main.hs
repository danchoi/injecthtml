{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Options.Applicative 
import Control.Applicative
import Text.XML.HXT.Core
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows

data Options = Options {
      templateOpt :: TemplateOpt
    }

data TemplateOpt = TemplateFile FilePath | TemplateString String

options :: Parser Options
options = Options 
    <$> ( (TemplateFile <$> 
          (strArgument 
            ( metavar "TEMPLATE-FILE" 
            <> help "Template file path")))
        <|> 
          (TemplateString <$>
            (strOption 
              (short 'e'
              <> metavar "TEMPLATE-STRING"
              <> help "Template as inline string"
              )
            ))
        )

opts :: ParserInfo Options
opts = info (helper <*> options) 
          (fullDesc <> header "injecthtml"
          <> progDesc "HTML template inject")

main = do
    Options{..} <- execParser opts
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

