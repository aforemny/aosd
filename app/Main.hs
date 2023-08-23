{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Bits
import GHC.Ptr (Ptr)
import Graphics.X11 qualified as X
import Graphics.X11.Xlib.Extras qualified as X
import System.IO
import System.IO.Error

data Env = Env
  { dpy :: X.Display,
    win :: X.Window,
    width :: Int,
    height :: Int,
    gc :: X.GC,
    drw :: X.Drawable,
    white :: X.Pixel,
    black :: X.Pixel
  }

data State = State
  { p :: Float,
    grabbing :: Bool,
    dirty :: Bool
  }
  deriving (Show, Eq)

main :: IO ()
main = bracket createWindow destroyWindow run

run :: (Env, TVar State) -> IO ()
run (env@(Env {..}), stateT) = do
  X.allocaXEvent $ \ev -> forever $ do
    state <- atomically $ readTVar stateT
    state' <- processEvents ev env state
    -- XXX note that _NET_WM_STATE_FULLSCREEN should be enough to stack this above _NET_WM_TYPE_DOCK, but it isn't
    X.raiseWindow dpy win
    when (state' /= state) $ paint env state'
    X.sync dpy False
    atomically $ writeTVar stateT state' {dirty = False}
    threadDelay 1666

processEvents :: Ptr X.XEvent -> Env -> State -> IO State
processEvents ev env@(Env {..}) state = do
  n <- X.eventsQueued dpy X.queuedAlready
  foldM (\state _ -> processEvent ev env state) state [1 .. n]

processEvent :: Ptr X.XEvent -> Env -> State -> IO State
processEvent ev (Env {..}) state@(State {..}) = do
  X.nextEvent dpy ev
  e <- X.getEvent ev
  if
      | grabbing,
        X.MotionEvent {X.ev_x} <- e -> do
          let p = fi ev_x / fi width
          pure state {p = p}
      | grabbing,
        X.ButtonEvent {X.ev_event_type, X.ev_button, X.ev_x} <- e,
        ev_event_type == X.buttonRelease,
        ev_button == X.button1 -> do
          X.ungrabPointer dpy X.currentTime
          let p = fi ev_x / fi width
          pure state {p = p, grabbing = False}
      | not grabbing,
        X.ButtonEvent {X.ev_event_type, X.ev_button, X.ev_x} <- e,
        ev_event_type == X.buttonPress,
        ev_button == X.button1 -> do
          grabStatus <- X.grabPointer dpy win True (X.pointerMotionMask .|. X.buttonReleaseMask) X.grabModeAsync X.grabModeAsync X.none X.none X.currentTime
          if (grabStatus == X.grabSuccess)
            then do
              let p = fi ev_x / fi width
              pure state {p = p, grabbing = True}
            else pure state
      | X.ExposeEvent {} <- e ->
          pure state {dirty = True}
      | otherwise -> pure state

paint :: Env -> State -> IO ()
paint Env {..} State {grabbing = False} = do
  X.setForeground dpy gc 0
  X.setBackground dpy gc 0
  X.fillRectangle dpy drw gc 0 0 (fi width) (fi height)
paint Env {..} State {grabbing = True, ..} = do
  X.setForeground dpy gc 0
  X.setBackground dpy gc 0
  X.fillRectangle dpy drw gc 0 0 (fi width) (fi height)
  X.setForeground dpy gc white
  X.setBackground dpy gc white
  X.fillRectangle dpy drw gc 0 0 (floor (p * fi width)) (fi height)

destroyWindow :: (Env, TVar State) -> IO ()
destroyWindow (Env {..}, _) = do
  X.destroyWindow dpy win

createWindow :: IO (Env, TVar State)
createWindow = do
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
      width = fi (X.displayWidth dpy scrn)
      height = 64
  cmap <- X.createColormap dpy root vis X.allocNone
  win <- X.allocaSetWindowAttributes $ \attr -> do
    X.set_colormap attr cmap
    X.set_border_pixel attr 0
    X.set_background_pixel attr 0
    X.set_override_redirect attr True
    X.createWindow dpy root 0 0 (fi width) (fi height) 0 dpth cls vis vmsk attr
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
  let drw = win
  gc <- X.createGC dpy drw
  X.mapWindow dpy win
  X.selectInput dpy win (X.buttonPressMask .|. X.exposureMask)
  let p = 1
      grabbing = False
      dirty = True
  stateT <- newTVarIO State {..}
  _ <- forkIO $ consumeInput stateT
  _ <- forkIO $ produceOutput stateT
  pure (Env {..}, stateT)

consumeInput :: TVar State -> IO ()
consumeInput stateT =
  catchJust
    (guard . isEOFError)
    ( forever $ do
        p <- (/ 100) . read <$> getLine
        atomically $ do
          state <- readTVar stateT
          when (p /= state.p) $ do
            writeTVar stateT state {p = p}
    )
    (\_ -> pure ())

produceOutput :: TVar State -> IO ()
produceOutput stateT = go Nothing
  where
    go o' = do
      o <- atomically $ do
        state <- readTVar stateT
        let o = toOutput state.p
        when (Just o == o') retry
        pure o
      putStrLn o
      hFlush stdout
      go (Just o)
    toOutput p =
      show (floor (100 * p) :: Int)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
