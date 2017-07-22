{-# LANGUAGE OverloadedStrings #-}

module Grep (processFiles) where

import Data.Text (Text)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Control.Applicative
import Data.Char
import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import System.Directory (doesFileExist)
import System.IO
import Text.Regex.PCRE


parseLines pattern = CL.filter f
  where
    f :: (B.ByteString, B.ByteString) -> Bool
    f (_, x) = x =~ (pattern :: String)

printMatch = CL.mapM_ (\(id, _) -> liftIO $ putStrLn $ show id)


processFiles :: String -> [String] -> IO ()
processFiles pattern = mapM_ (processFile pattern)

processFile :: String -> String -> IO ()
processFile _ [] = return ()
processFile pattern file = do
    exists <- doesFileExist file
    if not exists
        then putStrLn $ file ++ ": file does not exists"
        else do
            let parseConduit = CL.map $ B.span (':' /=)

            runConduitRes $
              CB.sourceFile file
              .| CB.lines
              .| parseConduit
              .| parseLines pattern
              .| printMatch
