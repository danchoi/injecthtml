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
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows
import qualified Text.Parsec as P hiding (many, (<|>)) 
import Data.Functor.Identity (Identity)

data Options = Options {
      templateOpt :: TemplateOpt
    , separatorChars :: String
    , injects :: [RawInject]
    } deriving Show

type XPath = String

data TemplateOpt = TemplateFile FilePath 
                 | TemplateString String 
                   deriving Show
data RawInject = RawInjectFile String
               | RawInjectString String
            deriving Show

data Inject = InjectFile (FilePath, XPath)
            | InjectString (String, XPath)
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
      -- use -@XPATH for STDIN
      (RawInjectString <$> (strOption (short 's' <> metavar "STRING@XPATH")))
      <|> (RawInjectFile <$> (strOption (short 'f' <> metavar "FILE@XPATH")))

options :: Parser Options
options = Options 
    <$> parseTemplateOpt 
    <*> strOption (short 'k' <> metavar "SEPARATOR"
                  <> value "::" 
                  <> help "Separator characters between FILE/STRING and XPATH. Default ::")
    <*> many parseInjectRaw

opts :: ParserInfo Options
opts = info (helper <*> options) 
          (fullDesc <> header "injecthtml"
          <> progDesc "HTML template inject")

-- Warning, if this is full HTML document, it will strip off the doctype
-- <!DOCTYPE html>
-- injecthtml is better used with smaller templates

main = do
    o@Options{..} <- execParser opts
    let injects' = map (parseInject' separatorChars) injects
    injects'' :: [(XPath, String)] <- mapM loadInject injects'
    template <- case templateOpt of
                  TemplateFile f -> readFile f
                  TemplateString s -> return s
    let indent = True
    let indent' = if indent then yes else no
    res <- runX (processTemplate indent' template injects'') 
    mapM putStrLn res


parseInject' :: String -> RawInject  -> Inject
parseInject' sep (RawInjectFile x) = InjectFile $ parseInject sep x
parseInject' sep (RawInjectString x) = InjectString $ parseInject sep x

parseInject :: String -> String -> (String, XPath)
parseInject separator s = 
    let (x,y) = T.breakOn (T.pack separator) (T.pack s)
    in  (unpack x, drop (length separator) . unpack $  y)

loadInject :: Inject -> IO (String, XPath)
loadInject (InjectFile (filePath, xpath)) | filePath == "-" = (,) <$> getContents <*> pure xpath
                                          | otherwise = (,) <$> (readFile filePath) <*> pure xpath
loadInject (InjectString x) = return x


processTemplate indent html injects = 
      readString [withValidate no, withParseHTML yes, withInputEncoding utf8] html
      >>> setTraceLevel 0
      >>> (foldl (>>>) this . map process $ injects)
      >>> writeDocumentToString [withIndent indent, withOutputHTML, withXmlPi no]

process (replacement, xpath') = 
    processXPathTrees 
      (
        replaceChildren (
          constA replacement >>> xread
        )
      ) xpath'

