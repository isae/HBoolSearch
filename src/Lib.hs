module Lib
    (
      Corpus(..),
      Index(..),
      parseCorpus
    ) where


import Text.XML.Light
import Data.List
import qualified Data.Map.Lazy as M
import Data.Array

data Corpus = Corpus { title :: String
                     , snippets :: [String]
                     } deriving (Show)

type Path = String
type Term = String
type Occurence = (Int, Int) -- first - number of document, second - number of snippet, 0 - title

data Index = Index { paths :: Array Int (Path, Corpus)
                   , termins :: M.Map Term (Array Int Occurence)
                     } deriving (Show)


getC :: String -> Element -> [Element]
getC str = findChildren (unqual str)

getCs :: String -> [Element] -> [Element]
getCs str docs = concat $ findChildren (unqual str) <$> docs

parseCorpus :: String -> Corpus -- maybe instance Read ?
parseCorpus str =
  let doc = (onlyElems $ parseXML str) !! 0
      corpTitle = intercalate " " $ strContent <$>
        (getCs "title" $ getCs "titleStmt" $ getCs "fileDesc" $ getC "teiHeader" doc)
      body = getCs "body" $ getC "text" doc
      corpBody = strContent <$> (getCs "head" body) ++ (getCs "p" body)
   in Corpus { title = corpTitle, snippets = corpBody}
