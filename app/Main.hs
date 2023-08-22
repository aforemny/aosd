module Main where

import Control.Concurrent
import Control.Monad
import Foreign.Marshal
import Graphics.X11
import Graphics.X11.Xlib.Extras
import System.IO

main :: IO ()
main = do
  dpy <- openDisplay ""
  let scrn = defaultScreen dpy
      scr = defaultScreenOfDisplay dpy
      black = blackPixel dpy scrn
      white = whitePixel dpy scrn
      root = defaultRootWindow dpy
      dpth = defaultDepthOfScreen scr
      cls = inputOutput
      vis = defaultVisualOfScreen scr
      vmsk = 0
  win <- allocaSetWindowAttributes $ createWindow dpy root 0 0 2560 64 0 dpth cls vis vmsk
  atom <- internAtom dpy "ATOM" True
  wmState <- internAtom dpy "_NET_WM_STATE" False
  wmStateSticky <- internAtom dpy "_NET_WM_STATE_STICKY" False
  wmStateAbove <- internAtom dpy "_NET_WM_STATE_ABOVE" False
  changeProperty32 dpy win wmState atom propModeReplace $
    [ fromIntegral wmStateAbove,
      fromIntegral wmStateSticky
    ]
  wmWindowType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  wmWindowTypeDock <- internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
  changeProperty32 dpy win wmWindowType atom propModeReplace $
    [ fromIntegral wmWindowTypeDock ]
  let drw = win
  gc <- createGC dpy drw
  mapWindow dpy win
  raiseWindow dpy win
  allocaXEvent $ \ev -> forever $ do
    putStr "."
    hFlush stdout
    setForeground dpy gc white
    setBackground dpy gc white
    fillRectangle dpy drw gc 0 0 2560 64
    flush dpy
    threadDelay (16666)
  destroyWindow dpy win
  return ()
