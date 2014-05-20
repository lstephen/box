#!/usr/bin/env runhaskell

import Control.Applicative

import Data.Monoid

import System.Environment
import System.Exit
import System.Process

main :: IO ()
main = do
  (file:args) <- getArgs
  output <- run validators file args
  putStrLn output
  if output == ""
    then exitSuccess
    else exitFailure

type Validator = FilePath -> [String] -> IO String

validators :: [Validator]
validators = [check, lint]

run :: [Validator] -> Validator
run vs f as = mconcat <$> sequence (apply <$> vs)
  where
    apply v = v f as

check :: Validator
check f as = execute "ghc-mod" ("check":f:as)

lint :: Validator
lint f as = execute "ghc-mod" (mconcat [["lint", f], lintArgs, as])
  where
    lintArgs = mappend "-h--hint=" <$> lints
    lints = ["Default", "Dollar", "Generalise"]


execute :: FilePath -> [String] -> IO String
execute exe args = do
  (_,out,err) <- readProcessWithExitCode exe args ""
  return $ out `mappend` err



