{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Universum            hiding (catMaybes, filter, lines, list,
                                       unlines)

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Data.Sequences
import           Data.Text            (strip)
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
  input <- repack . lines . toText <$> getContents

  -- apply mask
  let candidates = case maskRx args of
        Nothing -> map (\c -> (c,c)) input
        Just re -> catMaybes
                 . map (\line -> let match = map capturedText . safeHead . snd <=< matchCaptures $ line ?=~ re
                                 in  sequenceA (line, match))
                 $ input

  -- render UI
  let st = AppState { queryBox = editorText Query (Just 1) mempty
                    , resultsList = list Results candidates 1
                    , selectedElement = Nothing
                    , candidates = candidates
                    }
  st' <- withFile "/dev/tty" ReadMode $ \h -> do
    fd <- handleToFd h
    cfg <- standardIOConfig
    customMain (mkVty $ cfg {inputFd = Just fd}) Nothing app st

  whenJust (selectedElement st') putStrLn

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

data AppState = AppState { queryBox        :: Editor Text Name
                         , resultsList     :: List Name (Text,Text)
                         , selectedElement :: Maybe Text
                         , candidates      :: Vector (Text, Text)
                         }

handleEvent :: AppState -> BrickEvent Name Event -> EventM Name (Next AppState)
handleEvent st (VtyEvent ev@(EvKey k ms)) = case (k,ms) of
  (KEsc, [])           -> halt st
  (KChar 'c', [MCtrl]) -> halt st
  (KEnter, [])         -> case listSelectedElement $ resultsList st of
      Nothing     -> continue st
      Just (_, (e,_)) -> halt $ st { selectedElement = Just e }
  (KUp, [])            -> list
  (KDown, [])          -> list
  _                    -> editor
 where editor = do
          r <- handleEditorEvent ev $ queryBox st
          let query = strip . unlines . getEditContents $ r
              matchingCandidates = filter (isInfixOf (toLower query) . toLower . snd) . candidates $ st
              resultsList' = set listElementsL matchingCandidates $ resultsList st
          continue $ st { queryBox = r, resultsList = resultsList' }

       list = do
          r <- handleListEvent ev $ resultsList st
          continue $ st { resultsList = r }
handleEvent st _ = continue st

drawUI :: AppState -> [Widget Name]
drawUI st = [ queryBox' <=> hBorder <=> resultsList' ]
  where queryBox' = (withAttr "prompt" $ txt "λ ") <+> renderEditor (txt . unlines) True (queryBox st)
        resultsList' = padLeftRight 2 $ renderList (\sel -> bool identity (withAttr "selection") sel . txt) False (map snd . resultsList $ st)
