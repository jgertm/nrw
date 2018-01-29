{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Universum            hiding (list)

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Graphics.Vty
import           Options.Applicative
import           System.Posix.IO
import           Text.RE.PCRE.Text
import           Text.RE.Replace


main :: IO ()
main = do
  -- read arguments
  args <- execParser options'
  when (version args) $ do
    putTextLn "0.0.0"
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

  -- render UI
  let st = AppState { query = editorText Query (Just 1) mempty
                    , results = list Results (foldMap one candidates) 1
                    }
  withFile "/dev/tty" ReadMode $ \h -> do
    fd <- handleToFd h
    cfg <- standardIOConfig
    customMain (mkVty $ cfg {inputFd = Just fd}) Nothing app st

  exitSuccess


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


-- USER INTERFACE

app = App { appDraw = drawUI
          , appChooseCursor = showFirstCursor
          , appHandleEvent = handleEvent
          , appStartEvent = pure
          , appAttrMap = const (attrMap defAttr attrs)
          }
  where attrs = [ ("prompt", fg red)
                , ("selection", black `Brick.on` white)]

data Name = Query
          | Results
          deriving (Show, Eq, Ord)

data AppState = AppState { query   :: Editor Text Name
                         , results :: List Name Text
                         }

handleEvent :: AppState -> BrickEvent Name Event -> EventM Name (Next AppState)
handleEvent st (VtyEvent ev@(EvKey k ms)) = case (k,ms) of
  (KEsc, [])           -> halt st
  (KChar 'c', [MCtrl]) -> halt st
  (KEnter, [])         -> continue $ undefined -- TODO
  (KUp, [])            -> list
  (KDown, [])          -> list
  _                    -> editor
 where editor = do
         r <- handleEditorEvent ev $ query st
         continue $ st { query = r }
       list = do
         r <- handleListEvent ev $ results st
         continue $ st { results = r }

drawUI :: AppState -> [Widget Name]
drawUI st = [ searchBox <=> hBorder <=> resultsList ]
  where searchBox = (withAttr "prompt" $ txt "λ ") <+> renderEditor (txt . unlines) True (query st)
        resultsList = padLeftRight 2 $ renderList (\sel -> bool identity (withAttr "selection") sel . txt) False (results st)
