{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Main where
import Hoogle
import Brick
import Brick.Widgets.Edit
import Brick.Widgets.List
import Brick.Widgets.Dialog
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Graphics.Vty

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Vector as V
import Data.Text (pack, unpack, splitOn)
import Data.Text.Zipper (clearZipper)
import Data.Char

import System.Command

data Name = SearchBox
          | DialogVp
          | DialogActual
          | DialogButtonA
          | SearchList deriving (Ord, Eq, Show)

data InfoDialogButton = Close
                      | OpenDocumentation deriving (Ord, Eq, Show)

data St = St {
                _curStr :: Editor String Name,
                _results :: List Name Target,
                _infoDialog :: Dialog InfoDialogButton Name,
                _infoDialogOpen :: Bool,
                _vp :: ViewportScroll Name
             }

makeLenses ''St

unescapeHTML :: [Char] -> [Char]
unescapeHTML ('&':xs)
    | Just x <- stripPrefix "lt;" xs = '<' : unescapeHTML x
    | Just x <- stripPrefix "gt;" xs = '>' : unescapeHTML x
    | Just x <- stripPrefix "amp;" xs = '&' : unescapeHTML x
    | Just x <- stripPrefix "quot;" xs = '\"' : unescapeHTML x
    | Just x <- stripPrefix "#"    xs = chr (read . take 2 . drop 1 $ xs) : unescapeHTML (dropWhile (/= ';') x)
unescapeHTML (x:xs) = x : unescapeHTML xs
unescapeHTML [] = []

stripTags :: [Char] -> [Char]
stripTags []         = []
stripTags ('<' : xs) = stripTags $ drop 1 $ dropWhile (/= '>') xs
stripTags (x : xs)   = x : stripTags xs

hstr :: String -> Widget n
hstr = str . unescapeHTML . stripTags

fmtpkg :: Maybe (String, URL) -> String
fmtpkg Nothing = ""
fmtpkg (Just x) = fst x

fmtmod :: Maybe (String, URL) -> String -> String
fmtmod Nothing = const ""
fmtmod (Just x) = (fst x ++)

targetName :: Target -> String
targetName = unpack . last . splitOn "<s0>" . head . splitOn "</s0>" . pack . targetItem

previewT :: Target -> Widget Name
previewT t =
  hstr (fmtmod (targetModule t) " =>> " ++ targetItem t)
  <+> (vLimit 1 $ fill ' ')
  <+> hstr (fmtpkg $ targetPackage t)

renderHoogleRes :: Bool -> Target -> Widget Name
renderHoogleRes s t = if s then withAttr listSelectedAttr ui else ui
  where
    name = previewT t
    ui = hyperlink (pack (targetURL t)) name

docsT :: Target -> Widget Name
docsT t = (title <+> (vLimit 1 $ fill ' ') <+> package)
          <=> hBorder
          <=> (withVScrollBars OnRight $ viewport DialogVp Vertical bread)
  where
    title = hstr $ fmtmod (targetModule t) " =>> " ++ targetItem t
    package = hstr . fmtpkg . targetPackage $ t
    bread = hstr . targetDocs $ t


cropAllBy :: Int -> Widget n -> Widget n
cropAllBy n = translateBy (Location (n, n)) . cropTopBy n . cropLeftBy n . cropBottomBy n . cropRightBy n


drawUI :: St -> [Widget Name]
drawUI st = [dialog, ui]
  where
    lst = renderList renderHoogleRes True (st^.results)
    sb = renderEditor (str . unlines) True (st^.curStr)
    tgt = (st^.results.listElementsL) V.! fromJust (st^.results.listSelectedL)
    idbody = docsT tgt
    rd = withAttr listSelectedAttr
       . cropAllBy 3
       . padAll 3
       $ renderDialog (st^.infoDialog)
                {dialogTitle=Just . str $ "Documentation for " ++ fmtmod (targetModule tgt) "." ++ targetName tgt} idbody
    dialog = if st^.infoDialogOpen then rd else emptyWidget
    ui = (if st^.infoDialogOpen then forceAttr $ attrName "hidden" else id)
      $ joinBorders
      $ withBorderStyle unicodeRounded
      $ borderWithLabel (str "Hoogle") $ vLimit 1 sb <=> hBorder <=> lst

appEvent :: BrickEvent Name e -> EventM Name St ()
appEvent (VtyEvent e@(EvKey KEsc [MMeta]))  = halt
appEvent (VtyEvent e@(EvKey KEsc []))  = do
  void (zoom curStr . modify $ applyEdit clearZipper)
  cs <- gets _curStr
  zoom results $ do
    res <- liftIO (searchHoogle 60 . head . getEditContents $ cs)
    modify $ listReplace (V.fromList res) (Just 0)

appEvent (VtyEvent e@(EvKey KEnter [MMeta])) = modify $ infoDialogOpen .~ True
appEvent (VtyEvent e@(EvKey KUp []))   = zoom results $ handleListEvent e
appEvent (VtyEvent e@(EvKey KDown [])) = zoom results $ handleListEvent e
appEvent (VtyEvent e@(EvKey KEnter [])) = do
  selUrl <- targetURL <$> liftM2 (V.!) (use $ results.listElementsL) (use $ results.listSelectedL.non 0)
  liftIO $ command_ [EchoStdout False, EchoStderr False] "xdg-open" [selUrl]
  halt
appEvent e = do
  zoom curStr $ handleEditorEvent e
  cs <- gets _curStr
  zoom results $ do
    res <- liftIO (searchHoogle 75 . head . getEditContents $ cs)
    modify $ listReplace (V.fromList res) (Just 0)

dialogEvent :: BrickEvent Name e -> EventM Name St ()
dialogEvent (VtyEvent (EvKey KEsc [])) = modify $ infoDialogOpen .~ False
dialogEvent (VtyEvent (EvKey KLeft [])) = modify $ results %~ listMoveUp
dialogEvent (VtyEvent (EvKey KRight [])) = modify $ results %~ listMoveDown
dialogEvent (VtyEvent (EvKey KUp [])) = use vp >>= \x -> vScrollBy x (-3)
dialogEvent (VtyEvent (EvKey KDown [])) = use vp >>= \x -> vScrollBy x 3
dialogEvent e@(VtyEvent (EvKey KEnter [])) = appEvent e
dialogEvent (VtyEvent e) = zoom infoDialog $ handleDialogEvent e

globalEvent :: BrickEvent Name e -> EventM Name St ()
globalEvent e = do
  d <- use infoDialogOpen
  if d then dialogEvent e else appEvent e

searchHoogle :: Int -> String -> IO [Target]
searchHoogle n s = do
  loc <- defaultDatabaseLocation
  withDatabase loc (pure . take n . (`searchDatabase` s))

theMap :: AttrMap
theMap = attrMap defAttr
    [ (editAttr,                   defAttr `withForeColor` brightWhite `withStyle` bold)
    , (editFocusedAttr,            defAttr)
    , (listAttr,                   defAttr)
    , (listSelectedAttr,           defAttr `withForeColor` brightWhite `withStyle` bold)
    , (attrName "hidden",          defAttr `withStyle` dim)
    ]

appCursor :: St -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor ls = find (\x -> cursorLocationName x == Just SearchBox)

theApp :: App St e Name
theApp =
    App   { appDraw = drawUI
          , appChooseCursor = appCursor
          , appHandleEvent = globalEvent
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }

initialState :: St
initialState =
    St (editor SearchBox (Just 1) "")
       (list SearchList V.empty 1)
       (dialog (Just . str $ "Documentation") (Just (DialogActual, [("Open Docs [Enter]", DialogButtonA, OpenDocumentation)])) 999)
       False
       (viewportScroll DialogVp)

main :: IO ()
main = do
  defaultMain theApp initialState
  return ()
