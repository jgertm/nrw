{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards     #-}

module Main where

import           Universum            hiding (catMaybes, filter, lines, list,
                                       modify, splitAt, unlines)

import           Brick
import           Brick.Widgets.Border
import           Brick.Widgets.Edit
import           Brick.Widgets.List
import           Control.Arrow        ((&&&))
import           Data.Sequences
import           Data.Text            (strip)
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
  input <- repack . filter (not . null) . lines . toText <$> getContents

  -- apply mask
  let candidates = catMaybes $ map (prepareInput maskingRegex) input

  -- render UI
  let st = AppState { queryBox = editorText Query (Just 1) mempty
                    , resultsList = list Results (repack [0..pred $ length candidates]) 1
                    , entries = candidates
                    }
  st' <- withFile "/dev/tty" ReadWriteMode $ \h -> do
    fd <- handleToFd h
    cfg <- standardIOConfig
    customMain (mkVty $ cfg {inputFd = Just fd, outputFd = Just fd}) Nothing app st

  traverse_ (putStrLn . original) . filter marked . entries $ st'

  exitSuccess


prepareInput :: Maybe RE -> Text -> Maybe Entry
prepareInput (Just re) t = do
  (_,[firstGroup]) <- matchCaptures $ t ?=~ re
  let maskingResult = capturedText firstGroup
  pure Entry { original = t
               , masked = maskingResult
               , norm = toLower maskingResult
               , match = Nothing
             , marked = False
               }
prepareInput Nothing t = pure Entry { original = t
                                      , masked = t
                                      , norm = toLower t
                                      , match = Nothing
                                    , marked = False
                                      }


data Entry = Entry { original :: Text
                   , masked   :: Text
                   , norm     :: Text
                   , match    :: Maybe (Text,Text,Text)
                   , marked   :: Bool
                   }


-- COMMAND LINE ARGUMENTS

data Options = Options { version :: Bool
                       , maskingRegex :: Maybe RE
                       }

options :: Parser Options
options = Options
  <$> switch (long "version" <> short 'v' <> help "Show version")
  <*> option (eitherReader $ compileRegex >=> (pure . Just))
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
      matchingCandidates = map fst . filter (liftA2 (||) marked (isInfixOf query . norm) . snd) . indexed $ entries
      selectionIndex = view listSelectedL resultsList
      resultsList' = resultsList
                               & set listElementsL matchingCandidates
                               & set listSelectedL selectionIndex
          continue $ st { queryBox = r, resultsList = resultsList' }

withList st@AppState{..} ev = do
  r <- handleListEvent ev resultsList
          continue $ st { resultsList = r }

updateSelection f st@AppState{..} = fromMaybe st $ do
  sel <- view listSelectedL resultsList
  idx <- indexM (view listElementsL resultsList) sel
  let entries' = modify (\v -> MV.modify v (\e -> let selected' = f $ marked e
                                                   in e { marked = selected' }) idx) entries
  pure $ st { entries = entries' }

forceSelection = updateSelection $ const True
toggleSelection = updateSelection not


layer b f = bool identity f b

drawUI :: AppState -> [Widget Name]
drawUI AppState{..} = one $ queryBox' <=> hBorder <=> resultsList'
  where queryBox' = withAttr "prompt" (txt "λ ") <+> renderEditor (txt . unlines) True queryBox
        query = strip . unlines . getEditContents $ queryBox
        resultsList' = renderList (\sel i -> fromMaybe emptyWidget $ do
                                      entry <- index entries i
                                      layer (marked entry) (\w -> w <+> padLeft Max (txt "+")) . layer sel (forceAttr "selection")
                                        <$> if null query
                                        then pure . drawMarkup . Plain . masked $ entry
                                        else do
                                          re <- compileRegexWith BlockInsensitive . repack $ query
                                          pure $ foldl1 (<+>) . map drawMarkup . markupMatches $ masked entry *=~ re) True resultsList

data Markup t = Highlight {unMarkup :: t}
              | Plain {unMarkup :: t}
              deriving (Show)

drawMarkup (Highlight t) = withAttr "highlight" $ txt t
drawMarkup (Plain t)     = withAttr "default" $ txt t

markupMatches :: Matches Text -> [Markup Text]
markupMatches ms =
  let idxs = map (captureOffset &&& captureLength)
           . mainCaptures
           $ ms
      markups = go 0 idxs
              . matchesSource
              $ ms
   in filter (not . null . unMarkup) markups
    where go _ [] _ = mempty
          go d ((co,cl):rest) t =
            let (prefix, matchAndSuffix) = splitAt (co - d) t
                (match, suffix) = splitAt cl matchAndSuffix
             in mconcat [ [Plain prefix, Highlight match]
                        , bool mempty [Plain suffix] (null rest)
                        , go (co+cl) rest suffix]
