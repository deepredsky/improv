{-# LANGUAGE OverloadedStrings #-}

import System.Environment (getArgs)
import Grep (processFiles)

main = do
    args <- getArgs
    processFiles (head args) (tail args)
