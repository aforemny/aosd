module Main where

import Control.Concurrent
import Control.Monad
import Data.Bits
import Data.IORef
import Graphics.X11
import Graphics.X11.Xlib.Extras
import System.IO

main :: IO ()
main = do
  dpy <- openDisplay ""
  let scrn = defaultScreen dpy
      black = blackPixel dpy scrn
      white = whitePixel dpy scrn
      root = defaultRootWindow dpy
      trueColor = 4
  Just vinfo <- matchVisualInfo dpy scrn 32 trueColor
  let cls = inputOutput
      dpth = visualInfo_depth vinfo
      vis = visualInfo_visual vinfo
      vmsk = cWColormap .|. cWBorderPixel .|. cWBackingPixel .|. cWOverrideRedirect
      width = fi (displayWidth dpy scrn)
      height = 64
  cmap <- createColormap dpy root vis allocNone
  win <- allocaSetWindowAttributes $ \attr -> do
    set_colormap attr cmap
    set_border_pixel attr 0
    set_background_pixel attr 0
    set_override_redirect attr True
    createWindow dpy root 0 0 width height 0 dpth cls vis vmsk attr
  atom <- internAtom dpy "ATOM" True
  wmState <- internAtom dpy "_NET_WM_STATE" False
  wmStateSticky <- internAtom dpy "_NET_WM_STATE_STICKY" False
  wmStateAbove <- internAtom dpy "_NET_WM_STATE_ABOVE" False
  wmStateFullscreen <- internAtom dpy "_NET_WM_STATE_FULLSCREEN" False
  changeProperty32 dpy win wmState atom propModeReplace $
    [ fi wmStateAbove,
      fi wmStateSticky,
      fi wmStateFullscreen
    ]
  let drw = win
  gc <- createGC dpy drw
  mapWindow dpy win
  selectInput dpy win buttonPressMask
  pR <- newIORef (1 :: Float)
  grabbingR <- newIORef False
  forkIO $ forever $ do
    p <- (/ 100) . read <$> getLine
    writeIORef pR p
  allocaXEvent $ \ev -> forever $ do
    hFlush stdout
    p <- readIORef pR
    grabbing <- readIORef grabbingR
    raiseWindow dpy win
    if grabbing
      then do
        setForeground dpy gc 0
        setBackground dpy gc 0
        fillRectangle dpy drw gc 0 0 width height
        setForeground dpy gc white
        setBackground dpy gc white
        fillRectangle dpy drw gc 0 0 (floor (p * fi width)) height
      else do
        setForeground dpy gc 0
        setBackground dpy gc 0
        fillRectangle dpy drw gc 0 0 width height
    flush dpy
    timedOut <- waitForEvent dpy 1666
    when (not timedOut) $ do
      nextEvent dpy ev
      e <- getEvent ev
      -- hPutStrLn stderr (show e)
      if grabbing
        then do
          case e of
            MotionEvent {ev_x} -> do
              let p = fi ev_x / fi width
              writeIORef pR p
              -- TODO debounce duplicate values
              putStrLn (show (floor (p * 100)))
            ButtonEvent {ev_event_type, ev_button, ev_x} ->
              when (ev_event_type == buttonRelease && ev_button == button1) $ do
                ungrabPointer dpy currentTime
                let p = fi ev_x / fi width
                writeIORef pR p
                -- TODO debounce duplicate values
                putStrLn (show (floor (p * 100)))
                writeIORef grabbingR False
                -- XXX since -threaded, sync is required or else the application locks up
                sync dpy True
            _ -> pure ()
        else do
          case e of
            ButtonEvent {ev_event_type, ev_button, ev_x} ->
              when (ev_event_type == buttonPress && ev_button == button1) $ do
                grabStatus <- grabPointer dpy win True (pointerMotionMask .|. buttonReleaseMask) grabModeAsync grabModeAsync none none currentTime
                when (grabStatus == grabSuccess) $ do
                  let p = fi ev_x / fi width
                  writeIORef pR p
                  -- TODO debounce duplicate values
                  putStrLn (show (floor (p * 100)))
                  writeIORef grabbingR True
                  -- XXX since -threaded, sync is required or else the application locks up
                  sync dpy True
                pure ()
            _ -> pure ()
  destroyWindow dpy win
  return ()

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
