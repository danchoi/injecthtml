{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-} 
module Main where
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe
import Options.Applicative ((<>))
import qualified Options.Applicative as O
import Control.Applicative
import Text.XML.HXT.Core
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows

data Options = Options {
      templateOpt :: TemplateOpt
    }

data TemplateOpt = TemplateFile FilePath | TemplateString String

options :: O.Parser Options
options = Options 
    <$> ( (TemplateFile <$> 
          (O.strArgument 
            ( O.metavar "TEMPLATE-FILE" 
            <> O.help "Template file path")))
        <|> 
          (TemplateString <$>
            (O.strOption 
              (O.short 'e'
              <> O.metavar "TEMPLATE-STRING"
              <> O.help "Template as inline string"
              )
            ))
        )

opts :: O.ParserInfo Options
opts = O.info (O.helper <*> options) 
          (O.fullDesc <> O.header "injecthtml"
          <> O.progDesc "HTML template inject")

main = do
    Options{..} <- O.execParser opts
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

