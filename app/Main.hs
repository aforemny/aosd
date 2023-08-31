{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Bits
import Data.Maybe
import GHC.Ptr (Ptr)
import Graphics.X11 qualified as X
import Graphics.X11.Xlib.Extras qualified as X
import Options.Applicative
import System.IO
import System.IO.Error

data Args = Args
  { min :: Int,
    max :: Int,
    position :: Position
  }

data Position
  = Top
  | Left'

argsParser :: Parser Args
argsParser =
  Args
    <$> option percentage (long "min" <> metavar "PERCENT" <> help "Minimum percentage" <> showDefault <> value 0)
    <*> option percentage (long "max" <> metavar "PERCENT" <> help "Maximum percentage" <> showDefault <> value 100)
    <*> option
      position
      ( long "position"
          <> metavar "POSITION"
          <> help "Position"
          <> showDefaultWith
            ( \position -> case position of
                Top -> "top"
                Left' -> "left"
            )
          <> value Top
      )
  where
    percentage = fi <$> auto
    position =
      maybeReader
        ( \string -> case string of
            "top" -> Just Top
            "left" -> Just Left'
            _ -> Nothing
        )

data Env = Env
  { args :: Args,
    dpy :: X.Display,
    win :: X.Window,
    width :: Int,
    height :: Int,
    gc :: X.GC,
    white :: X.Pixel,
    black :: X.Pixel,
    pixm :: X.Pixmap,
    wwidth :: Int,
    wheight :: Int
  }

data State = State
  { p :: Maybe Float,
    grabbing :: Bool,
    dirty :: Bool
  }
  deriving (Show, Eq)

main :: IO ()
main = do
  args <- execParser opts
  bracket (createWindow args) destroyWindow run
  where
    opts = info (argsParser <**> helper) (fullDesc <> header "aosd - a simple OSD")

run :: (Env, TVar State) -> IO ()
run (env@(Env {..}), stateT) = do
  pStateT <- newTVarIO Nothing
  paintIfChanged env pStateT stateT
  X.allocaXEvent $ \ev -> forever $ do
    state <- atomically $ readTVar stateT
    if not state.grabbing
      then do
        timeOut <- X.waitForEvent dpy maxBound
        when (not timeOut) $ processEvents ev env stateT
      else do
        processEvents ev env stateT
        paintIfChanged env pStateT stateT
        threadDelay 16666

processEvents :: Ptr X.XEvent -> Env -> TVar State -> IO ()
processEvents ev env@(Env {..}) stateT = do
  state <- atomically $ readTVar stateT
  state' <- go state
  atomically $ writeTVar stateT state'
  where
    go state = do
      timeOut <- X.waitForEvent dpy 0
      if timeOut
        then pure state
        else do
          state' <- processEvent ev env state
          go state'

processEvent :: Ptr X.XEvent -> Env -> State -> IO State
processEvent ev (Env {..}) state@(State {..}) = do
  X.nextEvent dpy ev
  e <- X.getEvent ev
  if
      | grabbing,
        X.MotionEvent {X.ev_x, X.ev_y} <- e -> do
          let p = case args.position of
                Top -> Just (fi ev_x / fi wwidth)
                Left' -> Just (fi (wheight - fi ev_y) / fi wheight)
          pure state {p = p}
      | grabbing,
        X.ButtonEvent {X.ev_event_type, X.ev_button, X.ev_x, X.ev_y} <- e,
        ev_event_type == X.buttonRelease,
        ev_button == X.button1 -> do
          X.ungrabPointer dpy X.currentTime
          let p = case args.position of
                Top -> Just (fi ev_x / fi wwidth)
                Left' -> Just (fi (wheight - fi ev_y) / fi wheight)
          pure state {p = p, grabbing = False}
      | not grabbing,
        X.ButtonEvent {X.ev_event_type, X.ev_button, X.ev_x, X.ev_y} <- e,
        ev_event_type == X.buttonPress,
        ev_button == X.button1 -> do
          grabStatus <- X.grabPointer dpy win True (X.pointerMotionMask .|. X.buttonReleaseMask) X.grabModeAsync X.grabModeAsync X.none X.none X.currentTime
          if (grabStatus == X.grabSuccess)
            then do
              let p = case args.position of
                    Top -> Just (fi ev_x / fi wwidth)
                    Left' -> Just (fi (wheight - fi ev_y) / fi wheight)
              pure state {p = p, grabbing = True}
            else pure state
      | X.ExposeEvent {} <- e ->
          pure state {dirty = True}
      | X.AnyEvent {ev_event_type} <- e,
        ev_event_type == X.visibilityNotify -> do
          X.raiseWindow dpy win
          X.flush dpy
          pure state
      | otherwise -> pure state

paintIfChanged :: Env -> TVar (Maybe State) -> TVar State -> IO ()
paintIfChanged env@(Env {..}) pStateT stateT = do
  (pState, state) <- atomically $ do
    (,) <$> readTVar pStateT <*> readTVar stateT
  when (Just state /= pState) $ do
    paint env state
    X.flush dpy
    atomically $ do
      let state' = state {dirty = False}
      writeTVar pStateT (Just state')
      writeTVar stateT state'

paint :: Env -> State -> IO ()
paint Env {..} State {grabbing = False} = do
  X.setForeground dpy gc 0
  X.setBackground dpy gc 0
  X.fillRectangle dpy pixm gc 0 0 (fi wwidth) (fi wheight)
  X.copyArea dpy pixm win gc 0 0 (fi wwidth) (fi wheight) 0 0
paint Env {..} State {grabbing = True, ..} = do
  X.setForeground dpy gc 0
  X.setBackground dpy gc 0
  X.fillRectangle dpy pixm gc 0 0 (fi wwidth) (fi wheight)
  X.setForeground dpy gc white
  X.setBackground dpy gc white
  case args.position of
    Top -> X.fillRectangle dpy pixm gc 0 0 (floor (fromMaybe 1 p * fi wwidth)) (fi wheight)
    Left' -> X.fillRectangle dpy pixm gc 0 (floor ((1 - fromMaybe 1 p) * fi wheight)) (fi wwidth) (fi wheight)
  X.copyArea dpy pixm win gc 0 0 (fi wwidth) (fi wheight) 0 0

destroyWindow :: (Env, TVar State) -> IO ()
destroyWindow (Env {..}, _) = do
  X.destroyWindow dpy win

createWindow :: Args -> IO (Env, TVar State)
createWindow args = do
  dpy <- X.openDisplay ""
  let scrn = X.defaultScreen dpy
      black = X.blackPixel dpy scrn
      white = X.whitePixel dpy scrn
      root = X.defaultRootWindow dpy
      trueColor = 4
  Just vinfo <- X.matchVisualInfo dpy scrn 32 trueColor
  let cls = X.inputOutput
      dpth = X.visualInfo_depth vinfo
      vis = X.visualInfo_visual vinfo
      vmsk = X.cWColormap .|. X.cWBorderPixel .|. X.cWBackingPixel .|. X.cWOverrideRedirect
      width = case args.position of
        Top -> fi (X.displayWidth dpy scrn)
        Left' -> fi (X.displayHeight dpy scrn)
      height = 64
      wwidth =
        ( case args.position of
            Top -> fi width
            Left' -> fi height
        )
      wheight =
        ( case args.position of
            Top -> fi height
            Left' -> fi width
        )
  cmap <- X.createColormap dpy root vis X.allocNone
  win <- X.allocaSetWindowAttributes $ \attr -> do
    X.set_colormap attr cmap
    X.set_border_pixel attr 0
    X.set_background_pixel attr 0
    X.set_override_redirect attr True
    X.createWindow dpy root 0 0 (fi wwidth) (fi wheight) 0 dpth cls vis vmsk attr
  atom <- X.internAtom dpy "ATOM" True
  wmState <- X.internAtom dpy "_NET_WM_STATE" False
  wmStateSticky <- X.internAtom dpy "_NET_WM_STATE_STICKY" False
  wmStateAbove <- X.internAtom dpy "_NET_WM_STATE_ABOVE" False
  wmStateFullscreen <- X.internAtom dpy "_NET_WM_STATE_FULLSCREEN" False
  X.changeProperty32 dpy win wmState atom X.propModeReplace $
    [ fi wmStateAbove,
      fi wmStateSticky,
      fi wmStateFullscreen
    ]
  pixm <- X.createPixmap dpy win (fi wwidth) (fi wheight) dpth
  gc <- X.createGC dpy win
  X.mapWindow dpy win
  X.selectInput dpy win (X.buttonPressMask .|. X.exposureMask .|. X.visibilityChangeMask)
  let p = Nothing
      grabbing = False
      dirty = True
  stateT <- newTVarIO State {..}
  let env = Env {..}
  _ <- forkIO $ consumeInput stateT
  _ <- forkIO $ produceOutput env stateT
  pure (env, stateT)

consumeInput :: TVar State -> IO ()
consumeInput stateT =
  catchJust
    (guard . isEOFError)
    ( forever $ do
        p <- Just . (/ 100) . read <$> getLine
        atomically $ do
          state <- readTVar stateT
          when (p /= state.p && not state.grabbing) $ do
            writeTVar stateT state {p = p}
    )
    (\_ -> pure ())

produceOutput :: Env -> TVar State -> IO ()
produceOutput Env {..} stateT = go Nothing
  where
    go o' = do
      o <- atomically $ do
        state <- readTVar stateT
        case state.p of
          Just p -> do
            let o = toOutput p
            when (Just o == o') retry
            pure o
          Nothing ->
            retry
      putStrLn o
      hFlush stdout
      go (Just o)
    toOutput p =
      show (floor ((fi args.max - fi args.min) * p + fi args.min) :: Int)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
