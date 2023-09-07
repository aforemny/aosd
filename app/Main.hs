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
import Graphics.X11.Xshape qualified as X
import Options.Applicative
import System.IO
import System.IO.Error

data Args = Args
  { pmin :: Int,
    pmax :: Int,
    position :: Position,
    passive :: Bool
  }

data Position
  = Top
  | Left'

argsParser :: Parser Args
argsParser =
  Args
    <$> option
      percentage
      ( long "min"
          <> metavar "PERCENT"
          <> help "Minimum percentage"
          <> showDefault
          <> value 0
      )
    <*> option
      percentage
      ( long "max"
          <> metavar "PERCENT"
          <> help "Maximum percentage"
          <> showDefault
          <> value 100
      )
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
    <*> flag
      False
      True
      ( long "passive"
          <> help "Whether to react to mouse events (active)"
          <> showDefault
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
    swidth :: Int,
    sheight :: Int,
    gc :: X.GC,
    white :: X.Pixel,
    black :: X.Pixel,
    pixm :: X.Pixmap,
    wleft :: Int,
    wtop :: Int,
    wwidth :: Int,
    wheight :: Int
  }

data State = State
  { p :: Maybe Float,
    grabbing :: Bool,
    dirty :: Bool,
    remainFor :: Maybe Int
  }
  deriving (Show, Eq)

main :: IO ()
main = do
  args <- execParser opts
  bracket (createWindow args) destroyWindow mainLoop
  where
    opts = info (argsParser <**> helper) (fullDesc <> header "aosd - a simple OSD")

mainLoop :: (Env, TVar State) -> IO ()
mainLoop (env@(Env {..}), stateT) = do
  pStateT <- newTVarIO Nothing
  paintIfChanged env pStateT stateT
  X.allocaXEvent $ \ev -> forever $ do
    state <- atomically $ readTVar stateT
    if state.grabbing || isJust state.remainFor || state.dirty
      then do
        processEvents ev env stateT
        paintIfChanged env pStateT stateT
        threadDelay dt
      else do
        timeOut <- X.waitForEvent dpy (fi dt)
        when (not timeOut) $ processEvents ev env stateT
  where
    dt = 16666

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
  let p x' y' =
        let x = clamp 0 wwidth (fi x')
            y = clamp 0 wheight (fi y')
         in Just $ clamp 0 1 $ case args.position of
              Top -> fi x / fi wwidth
              Left' -> fi (wheight - fi y) / fi wheight
  if
      | grabbing,
        X.MotionEvent {X.ev_x, X.ev_y} <- e -> do
          pure state {p = p ev_x ev_y}
      | grabbing,
        X.ButtonEvent {X.ev_event_type, X.ev_button, X.ev_x, X.ev_y} <- e,
        ev_event_type == X.buttonRelease,
        ev_button == X.button1 -> do
          X.ungrabPointer dpy X.currentTime
          pure state {p = p ev_x ev_y, grabbing = False, remainFor = Just 1500}
      | not grabbing,
        X.ButtonEvent {X.ev_event_type, X.ev_button, X.ev_x, X.ev_y} <- e,
        ev_event_type == X.buttonPress,
        ev_button == X.button1 -> do
          grabStatus <- X.grabPointer dpy win True (X.pointerMotionMask .|. X.buttonReleaseMask) X.grabModeAsync X.grabModeAsync X.none X.none X.currentTime
          if (grabStatus == X.grabSuccess)
            then pure state {p = p ev_x ev_y, grabbing = True}
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
  when (Just state /= pState || state.dirty) $ do
    paint env state
    X.flush dpy
    atomically $ do
      let state' =
            state
              { dirty = True,
                remainFor =
                  state.remainFor >>= \forMs -> do
                    let forMs' = forMs - 16
                    if forMs' <= 0 then Nothing else Just forMs'
              }
      writeTVar pStateT (Just state)
      writeTVar stateT state'

paint :: Env -> State -> IO ()
paint Env {..} State {grabbing = False, remainFor = Nothing} = do
  X.setForeground dpy gc 0
  X.fillRectangle dpy pixm gc 0 0 (fi wwidth) (fi wheight)
  X.copyArea dpy pixm win gc 0 0 (fi wwidth) (fi wheight) 0 0
paint Env {..} State {..} = do
  let mar = 20
      pad = 4
  X.setForeground dpy gc 0x00000000
  X.fillRectangle dpy pixm gc 0 0 (fi wwidth) (fi wheight)

  X.setForeground dpy gc 0xff000000
  case args.position of
    Top -> X.fillRectangle dpy pixm gc 0 (fi mar) (floor (fi wwidth)) (fi (wheight - 2 * mar))
    Left' -> X.fillRectangle dpy pixm gc (fi mar) 0 (fi (wwidth - 2 * mar)) (fi wheight)

  X.setForeground dpy gc 0xffffffff
  case args.position of
    Top -> X.fillRectangle dpy pixm gc (fi pad) (fi (mar + pad)) (floor (fromMaybe 1 p * fi (wwidth - 2 * pad))) (fi (wheight - 2 * (mar + pad)))
    Left' -> X.fillRectangle dpy pixm gc (fi (mar + pad)) (fi pad + floor ((1 - fromMaybe 1 p) * fi (wheight - 2 * pad))) (fi (wwidth - 2 * (mar + pad))) (floor ((fromMaybe 1 p) * fi (wheight - 2 * pad)))

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
      swidth = fi (X.displayWidth dpy scrn)
      sheight = fi (X.displayHeight dpy scrn)
      wwidth = case args.position of
        Top -> floor (0.5 * fi swidth)
        Left' -> fi 64
      wheight = case args.position of
        Top -> fi 64
        Left' -> floor (0.5 * fi sheight)
      wleft = case args.position of
        Top -> floor (0.25 * fi swidth)
        Left' -> floor (0.05 * fi (min swidth sheight))
      wtop = case args.position of
        Top -> floor (0.05 * fi (min swidth sheight))
        Left' -> floor (0.25 * fi sheight)
  cmap <- X.createColormap dpy root vis X.allocNone
  win <- X.allocaSetWindowAttributes $ \attr -> do
    X.set_background_pixel attr 0
    X.set_border_pixel attr 0
    X.set_colormap attr cmap
    X.set_override_redirect attr True
    X.createWindow dpy root (fi wleft) (fi wtop) (fi wwidth) (fi wheight) 0 dpth cls vis vmsk attr
  X.setClassHint dpy win X.ClassHint { resName = "aosd", resClass = "aosd" }
  atom <- X.internAtom dpy "ATOM" True
  wmState <- X.internAtom dpy "_NET_WM_STATE" False
  wmStateSticky <- X.internAtom dpy "_NET_WM_STATE_STICKY" False
  wmStateAbove <- X.internAtom dpy "_NET_WM_STATE_ABOVE" False
  wmStateFullscreen <- X.internAtom dpy "_NET_WM_STATE_FULLSCREEN" False
  wmWindowType <- X.internAtom dpy "_NET_WM_WINDOW_TYPE" False
  wmWindowTypeNotification <- X.internAtom dpy "_NET_WM_WINDOW_TYPE_NOTIFICATION" False
  X.changeProperty32 dpy win wmState atom X.propModeReplace $
    [ fi wmStateAbove,
      fi wmStateSticky,
      fi wmStateFullscreen
    ]
  X.changeProperty32 dpy win wmWindowType atom X.propModeReplace $
    [ fi wmWindowTypeNotification
    ]
  pixm <- X.createPixmap dpy win (fi wwidth) (fi wheight) dpth
  gc <- X.createGC dpy win
  X.mapWindow dpy win
  when args.passive $ do
    spixm <- X.createPixmap dpy win (fi swidth) (fi sheight) 1
    sgc <- X.createGC dpy spixm
    X.setBackground dpy sgc 0
    X.setForeground dpy sgc 1
    X.fillRectangle dpy spixm sgc 0 0 0 0
    X.xshapeCombineMask dpy win X.shapeInput 0 0 spixm X.shapeSet
  X.selectInput dpy win (X.buttonPressMask .|. X.exposureMask .|. X.visibilityChangeMask)
  let p = Nothing
      grabbing = False
      dirty = True
      remainFor = Nothing
  stateT <- newTVarIO State {..}
  let env = Env {..}
  _ <- forkIO $ consumeInput env stateT
  _ <- forkIO $ produceOutput env stateT
  pure (env, stateT)

consumeInput :: Env -> TVar State -> IO ()
consumeInput Env {..} stateT =
  catchJust
    (guard . isEOFError)
    ( forever $ do
        p <- Just . clamp 0 1 . (\p -> fi (p - args.pmin) / fi args.pmax) . read <$> getLine
        atomically $ do
          state <- readTVar stateT
          when (p /= state.p && not state.grabbing) $ do
            writeTVar stateT $
              state
                { p = p,
                  remainFor = if isNothing state.p then Nothing else Just 1500
                }
    )
    (\_ -> pure ())

produceOutput :: Env -> TVar State -> IO ()
produceOutput Env {..} stateT = go Nothing
  where
    go o' = do
      o <- atomically $ do
        state <- readTVar stateT
        when (not state.grabbing) retry
        case state.p of
          Just p -> do
            let o = show (floor ((fi args.pmax - fi args.pmin) * clamp 0 1 p + fi args.pmin) :: Int)
            when (Just o == o') retry
            pure o
          Nothing ->
            retry
      putStrLn o
      hFlush stdout
      go (Just o)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

clamp :: Ord a => a -> a -> a -> a
clamp mi ma = max mi . min ma
