{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards     #-}

module Main where

import           Universum            hiding (catMaybes, filter, intersperse,
                                       lines, list, modify, unlines)

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Data.Sequences
import           Data.Text            (splitOn, strip)
import           Data.Vector          (indexM, indexed, modify)
import qualified Data.Vector.Mutable  as MV
import           Graphics.Vty
import           Options.Applicative
import           System.Posix.IO
import           Text.RE.PCRE.Text
import           Text.RE.Replace


main :: IO ()
main = do
  -- read arguments
  Options{..} <- execParser options'
  when version $ do
    putTextLn "0.0.0"
    exitSuccess

  -- read stdin
  input <- repack . lines . toText <$> getContents

  -- apply mask
  let candidates = catMaybes $ map (inputToEntry maskRx) input

  -- render UI
  let st = AppState { queryBox = editorText Query (Just 1) mempty
                    , resultsList = list Results (map fst . indexed $ candidates) 1
                    , entries = candidates
                    }
  st' <- withFile "/dev/tty" ReadMode $ \h -> do
    fd <- handleToFd h
    cfg <- standardIOConfig
    customMain (mkVty $ cfg {inputFd = Just fd}) Nothing app st

  traverse_ (putStrLn . original) . filter selected . entries $ st'

  exitSuccess


inputToEntry :: Maybe RE -> Text -> Maybe Entry
inputToEntry (Just re) t = do
  (_,[firstGroup]) <- matchCaptures $ t ?=~ re
  let maskingResult :: Text = capturedText firstGroup
  pure $ Entry { original = t
               , masked = maskingResult
               , norm = toLower maskingResult
               , match = Nothing
               , selected = False
               }
inputToEntry Nothing t = pure $ Entry { original = t
                                      , masked = t
                                      , norm = toLower t
                                      , match = Nothing
                                      , selected = False
                                      }


data Entry = Entry { original :: Text
                   , masked   :: Text
                   , norm     :: Text
                   , match    :: Maybe (Text,Text,Text)
                   , selected :: Bool
                   }


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
  where attrs = [ ("default", defAttr)
                , ("prompt", fg red)
                , ("selection", black `Brick.on` white)
                , ("highlight", withStyle (fg red) bold)
                ]

data Name = Query
          | Results
          deriving (Show, Eq, Ord)

data AppState = AppState { queryBox        :: Editor Text Name
                         , resultsList :: List Name Int
                         , entries     :: Vector Entry
                         }

handleEvent :: AppState -> BrickEvent Name Event -> EventM Name (Next AppState)
handleEvent st@AppState{..} (VtyEvent ev@(EvKey k ms)) = case (k,ms) of
  (KEsc, [])           -> halt st
  (KChar 'c', [MCtrl]) -> halt st
  (KEnter, [])         -> halt $ forceSelection st
  (KChar '\t', [])     -> continue $ toggleSelection st
  (KUp, [])            -> withList st ev
  (KDown, [])          -> withList st ev
  _                    -> withEditor st ev
handleEvent st _ = continue st

withEditor st@AppState{..} ev = do
  r <- handleEditorEvent ev queryBox
  let query = toLower . strip . unlines . getEditContents $ r
      matchingCandidates = map fst . filter (isInfixOf query . norm . snd) . indexed $ entries
      selectionIndex = view listSelectedL resultsList
      resultsList' = resultsList
                               & set listElementsL matchingCandidates
                               & set listSelectedL selectionIndex
          continue $ st { queryBox = r, resultsList = resultsList' }

withList st@AppState{..} ev = do
  r <- handleListEvent ev resultsList
          continue $ st { resultsList = r }

updateSelection f st@AppState{..} = maybe st identity $ do
  sel <- view listSelectedL resultsList
  idx <- indexM (view listElementsL resultsList) sel
  let entries' = modify (\v -> MV.modify v (\e -> let selected' = f $ selected e
                                                   in e { selected = selected' }) idx) entries
  pure $ st { entries = entries' }

forceSelection = updateSelection $ const True
toggleSelection = updateSelection not


drawUI :: AppState -> [Widget Name]
drawUI AppState{..} = [ queryBox' <=> hBorder <=> resultsList' ]
  where queryBox' = (withAttr "prompt" $ txt "λ ") <+> renderEditor (txt . unlines) True queryBox
        query = strip . unlines $ getEditContents queryBox
        resultsList' = padLeftRight 2 $ renderList (\sel -> bool identity (withAttr "selection") sel . drawMatch query . masked . indexEx entries) False resultsList

drawMatch query candidate =
  let instances = if null query then mempty else splitOn query candidate
      widgets = intersperse (withAttr "highlight" $ txt query) . map (withAttr "default" . txt) $ instances
  in if null instances
     then txt candidate
     else hBox widgets
