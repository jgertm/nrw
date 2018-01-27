{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Universum

import           Options.Applicative
import           Text.RE.PCRE.Text
import           Text.RE.Replace


main :: IO ()
main = do
  -- read arguments
  args <- execParser options'
  when (version args) $ do
    putStrLn "0.0.0"
    exitSuccess

  -- read stdin
  input <- lines . toText <$> getContents

  -- apply mask
  let candidates = case maskRx args of
        Nothing -> input
        Just re -> filter (not . null)
                 . map (\line -> maybe mempty capturedText
                               . (safeHead . snd <=< matchCaptures)
                               $ line ?=~ re)
                 $ input

  -- render results
  traverse_ putStrLn candidates



-- COMMAND LINE ARGUMENTS

data Options = Options { version :: Bool
                       , maskRx  :: Maybe RE
                       }

options :: Parser Options
options = Options
  <$> switch (long "version" <> short 'v' <> help "Show version")
  <*> option (eitherReader $ \rxStr -> compileRegex rxStr >>= pure . Just)
        (long "mask" <> short 'm' <> help "masking (PCRE) regex" <> metavar "REGEX" <> value Nothing)

options' = info (options <**> helper)
  (fullDesc <> progDesc "interactively filter STDIN" <> header "nrw")
