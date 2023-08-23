{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bits
import Graphics.X11 qualified as X
import Graphics.X11.Xlib.Extras qualified as X
import System.IO
import Control.Concurrent.STM

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
    grabbing :: Bool
  }

main :: IO ()
main = bracket createWindow destroyWindow run

run :: (Env, TVar State) -> IO ()
run (Env {..}, stateT) = do
  X.allocaXEvent $ \ev -> forever $ do
    hFlush stdout
    State {..} <- atomically $ readTVar stateT
    -- XXX note that _NET_WM_STATE_FULLSCREEN should be enough to stack this above _NET_WM_TYPE_DOCK, but it isn't
    X.raiseWindow dpy win
    if grabbing
      then do
        X.setForeground dpy gc 0
        X.setBackground dpy gc 0
        X.fillRectangle dpy drw gc 0 0 (fi width) (fi height)
        X.setForeground dpy gc white
        X.setBackground dpy gc white
        X.fillRectangle dpy drw gc 0 0 (floor (p * fi width)) (fi height)
      else do
        X.setForeground dpy gc 0
        X.setBackground dpy gc 0
        X.fillRectangle dpy drw gc 0 0 (fi width) (fi height)
    X.flush dpy
    timedOut <- X.waitForEvent dpy 1666
    when (not timedOut) $ do
      X.nextEvent dpy ev
      e <- X.getEvent ev
      -- hPutStrLn stderr (show e)
      if grabbing
        then do
          case e of
            X.MotionEvent {X.ev_x} -> do
              let p = fi ev_x / fi width
              -- TODO debounce duplicate values
              putStrLn (show (floor (p * 100)))
              atomically $ modifyTVar stateT (\state -> state { p = p })
            X.ButtonEvent {X.ev_event_type, X.ev_button, X.ev_x} ->
              when (ev_event_type == X.buttonRelease && ev_button == X.button1) $ do
                X.ungrabPointer dpy X.currentTime
                let p = fi ev_x / fi width
                -- TODO debounce duplicate values
                putStrLn (show (floor (p * 100)))
                atomically $ modifyTVar stateT (\state -> state { p = p, grabbing = False })
                -- XXX since -threaded, sync is required or else the application locks up
                X.sync dpy True
            _ -> pure ()
        else do
          case e of
            X.ButtonEvent {X.ev_event_type, X.ev_button, X.ev_x} ->
              when (ev_event_type == X.buttonPress && ev_button == X.button1) $ do
                grabStatus <- X.grabPointer dpy win True (X.pointerMotionMask .|. X.buttonReleaseMask) X.grabModeAsync X.grabModeAsync X.none X.none X.currentTime
                when (grabStatus == X.grabSuccess) $ do
                  let p = fi ev_x / fi width
                  -- TODO debounce duplicate values
                  putStrLn (show (floor (p * 100)))
                  atomically $ modifyTVar stateT (\state -> state { p = p, grabbing = True })
                  -- XXX since -threaded, sync is required or else the application locks up
                  X.sync dpy True
                pure ()
            _ -> pure ()

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
  X.selectInput dpy win X.buttonPressMask
  let p = 1
      grabbing = False
  stateT <- newTVarIO State {..}
  forkIO $ forever $ do
    p <- (/ 100) . read <$> getLine
    atomically $ modifyTVar stateT (\state -> state { p = p })
  pure (Env {..}, stateT)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
