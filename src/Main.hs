{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Universum

import           Options.Applicative

main :: IO ()
main = do
  -- read arguments
  args <- execParser options'
  when (version args) $ do
    putStrLn "0.0.0"
    exitSuccess

  -- read stdin
  input <- toText <$> getContents
  let candidates = lines input

  -- render results
  traverse_ putStrLn candidates
data Options = Options { version :: Bool }

options :: Parser Options
options = Options
  <$> switch (long "version" <> short 'v' <> help "Show version")

options' = info (options <**> helper)
  (fullDesc <> progDesc "interactively filter STDIN" <> header "nrw")
