module Main where

import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.IORef
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
      trueColor = 4
  Just vinfo <- matchVisualInfo dpy scrn 32 trueColor
  let cls = inputOutput
      dpth = visualInfo_depth vinfo
      vis = visualInfo_visual vinfo
      vmsk = cWColormap .|. cWBorderPixel .|. cWBackingPixel .|. cWOverrideRedirect
  cmap <- createColormap dpy root vis allocNone
  win <- allocaSetWindowAttributes $ \attr -> do
    set_colormap attr cmap
    set_border_pixel attr 0
    set_background_pixel attr 0
    set_override_redirect attr True
    createWindow dpy root 0 0 2560 64 0 dpth cls vis vmsk attr
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
    [fromIntegral wmWindowTypeDock]
  let drw = win
  gc <- createGC dpy drw
  mapWindow dpy win
  raiseWindow dpy win
  selectInput dpy win buttonPressMask
  xR <- newIORef 2560
  grabbingR <- newIORef False
  allocaXEvent $ \ev -> forever $ do
    hFlush stdout
    x <- readIORef xR
    grabbing <- readIORef grabbingR
    if grabbing
      then do
        setForeground dpy gc black
        setBackground dpy gc black
        fillRectangle dpy drw gc 0 0 2560 64
        setForeground dpy gc white
        setBackground dpy gc white
        fillRectangle dpy drw gc 0 0 x 64
      else do
        setForeground dpy gc 0
        setBackground dpy gc 0
        fillRectangle dpy drw gc 0 0 2560 64
    flush dpy
    timedOut <- waitForEvent dpy (10 ^ 3)
    when (not timedOut) $ do
      nextEvent dpy ev
      e <- getEvent ev
      print e
      if grabbing
        then do
          case e of
            MotionEvent {ev_x} ->
              writeIORef xR (fromIntegral ev_x)
            ButtonEvent {ev_event_type, ev_button, ev_x, ev_time} ->
              when (ev_event_type == buttonRelease && ev_button == button1) $ do
                ungrabPointer dpy ev_time
                writeIORef xR (fromIntegral ev_x)
                writeIORef grabbingR False
            _ -> pure ()
        else do
          case e of
            ButtonEvent {ev_event_type, ev_button, ev_x, ev_time} ->
              when (ev_event_type == buttonPress && ev_button == button1) $ do
                grabStatus <- grabPointer dpy win True (pointerMotionMask .|. buttonReleaseMask) grabModeAsync grabModeAsync 0 0 ev_time
                when (grabStatus == grabSuccess) $ do
                  writeIORef xR (fromIntegral ev_x)
                  writeIORef grabbingR True
                pure ()
            _ -> pure ()
  destroyWindow dpy win
  return ()
