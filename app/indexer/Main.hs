{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import qualified Data.Map        as M
import           Prelude         hiding (readFile, writeFile)
import           System.Environment
import           System.IO
import           System.Directory
import           Data.Array.IO
import           Lib
import           Data.List

isXmlFile :: FilePath -> Bool
isXmlFile f = isSuffixOf ".xml" f

visitFileTree :: String -> IO ([String])
visitFileTree path = do
  isDir <- doesDirectoryExist path
  if isDir
     then do
       dirContents <- getDirectoryContents path
       files <- mapM visitFileTree $ (\n -> path ++ ('/':n)) <$> filter isXmlFile dirContents
       return $ concat files
     else return $ path:[]

getFilesArray :: [String] -> IO(IOArray Int String)
getFilesArray files = newListArray (1, length files) files

main :: IO ()
main = do
  args <- getArgs
  allFiles <- mapM visitFileTree args
  let files = concat allFiles
  xmls <- mapM (\f -> (openFile f ReadMode) >>= hGetContents) files
  let corpuses = parseCorpus <$> xmls
  fileArr <- getFilesArray files
  assocs <- getAssocs fileArr
  print assocs
  print corpuses

  return ()

