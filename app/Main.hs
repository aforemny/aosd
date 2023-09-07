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
    owin :: X.Window,
    iwin :: X.Window,
    swidth :: Int,
    sheight :: Int,
    gc :: X.GC,
    white :: X.Pixel,
    black :: X.Pixel,
    pixm :: X.Pixmap,
    iwidth :: Int,
    iheight :: Int,
    owidth :: Int,
    oheight :: Int,
    margin :: Int,
    padding :: Int
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
        let x = clamp 0 iwidth (fi x')
            y = clamp 0 iheight (fi y')
         in Just $ clamp 0 1 $ case args.position of
              Top -> fi x / fi iwidth
              Left' -> fi (iheight - fi y) / fi iheight
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
        X.ButtonEvent {X.ev_event_type, X.ev_button, X.ev_x, X.ev_y, X.ev_window} <- e,
        ev_event_type == X.buttonPress,
        ev_button == X.button1 -> do
          grabStatus <- X.grabPointer dpy ev_window True (X.pointerMotionMask .|. X.buttonReleaseMask) X.grabModeAsync X.grabModeAsync X.none X.none X.currentTime
          if (grabStatus == X.grabSuccess)
            then pure state {p = p ev_x ev_y, grabbing = True}
            else pure state
      | X.ExposeEvent {} <- e ->
          pure state {dirty = True}
      | X.AnyEvent {ev_event_type} <- e,
        ev_event_type == X.createNotify -> do
          X.raiseWindow dpy iwin
          X.raiseWindow dpy owin
          X.flush dpy
          pure state
      | otherwise ->
          pure state

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
              { dirty = isJust state.remainFor,
                remainFor =
                  state.remainFor >>= \forMs -> do
                    let forMs' = forMs - 16
                    if forMs' <= 0 then Nothing else Just forMs'
              }
      writeTVar pStateT (Just state)
      writeTVar stateT state'

paint :: Env -> State -> IO ()
paint Env {..} State {grabbing = False, remainFor = Nothing} =
  X.unmapWindow dpy owin
paint Env {..} State {..} = do
  X.mapWindow dpy owin
  {-X.setForeground dpy gc 0xffff0000
  X.fillRectangle dpy iwin gc 0 0 (fi iwidth) (fi iheight)-}

  X.setForeground dpy gc 0xff000000
  X.fillRectangle dpy pixm gc 0 0 (fi owidth) (fi oheight)

  let oleft' = padding
      otop' = padding
      owidth' = owidth - 2 * padding
      oheight' = oheight - 2 * padding
  X.setForeground dpy gc 0xffffffff
  case args.position of
    Top -> X.fillRectangle dpy pixm gc (fi oleft') (fi otop') (floor (fromMaybe 1 p * fi owidth')) (fi oheight')
    Left' -> X.fillRectangle dpy pixm gc (fi oleft') (fi otop' + floor ((1 - fromMaybe 1 p) * fi oheight')) (fi owidth') (floor ((fromMaybe 1 p) * fi oheight'))

  X.copyArea dpy pixm owin gc 0 0 (fi owidth) (fi oheight) 0 0

destroyWindow :: (Env, TVar State) -> IO ()
destroyWindow (Env {..}, _) = do
  X.destroyWindow dpy owin
  X.destroyWindow dpy iwin

createWindow :: Args -> IO (Env, TVar State)
createWindow args = do
  dpy <- X.openDisplay ""
  let scrn = X.defaultScreen dpy
      black = X.blackPixel dpy scrn
      white = X.whitePixel dpy scrn
      root = X.defaultRootWindow dpy
      trueColor = 4
  Just vinfo <- X.matchVisualInfo dpy scrn 32 trueColor
  let dpth = X.visualInfo_depth vinfo
      vis = X.visualInfo_visual vinfo
      vmsk = X.cWColormap .|. X.cWBorderPixel .|. X.cWBackingPixel .|. X.cWOverrideRedirect
      swidth = fi (X.displayWidth dpy scrn)
      sheight = fi (X.displayHeight dpy scrn)
      margin = 20
      padding = 4
      ileft = case args.position of
        Top -> floor (0.25 * fi swidth)
        Left' -> floor (0.05 * fi (min swidth sheight))
      itop = case args.position of
        Top -> floor (0.05 * fi (min swidth sheight))
        Left' -> floor (0.25 * fi sheight)
      iwidth = case args.position of
        Top -> floor (0.5 * fi swidth)
        Left' -> fi 64
      iheight = case args.position of
        Top -> fi 64
        Left' -> floor (0.5 * fi sheight)
      oleft = case args.position of
        Top -> ileft
        Left' -> ileft + margin
      otop = case args.position of
        Top -> itop + margin
        Left' -> itop
      owidth = case args.position of
        Top -> iwidth
        Left' -> iwidth - 2 * margin
      oheight = case args.position of
        Top -> iheight - 2 * margin
        Left' -> iheight
  cmap <- X.createColormap dpy root vis X.allocNone
  iwin <- X.allocaSetWindowAttributes $ \attr -> do
    X.set_override_redirect attr True
    {-X.set_background_pixel attr 0
    X.set_border_pixel attr 0
    X.set_colormap attr cmap
    -- X.createWindow dpy root (fi ileft) (fi itop) (fi iwidth) (fi iheight) 0 dpth X.inputOutput vis vmsk attr-}
    X.createWindow dpy root (fi ileft) (fi itop) (fi iwidth) (fi iheight) 0 0 X.inputOnly vis X.cWOverrideRedirect attr
  owin <- X.allocaSetWindowAttributes $ \attr -> do
    X.set_override_redirect attr True
    X.set_background_pixel attr 0
    X.set_border_pixel attr 0
    X.set_colormap attr cmap
    X.createWindow dpy root (fi oleft) (fi otop) (fi owidth) (fi oheight) 0 dpth X.inputOutput vis vmsk attr
  X.setClassHint dpy owin X.ClassHint {resName = "aosd", resClass = "aosd"}
  atom <- X.internAtom dpy "ATOM" True
  wmState <- X.internAtom dpy "_NET_WM_STATE" False
  wmStateSticky <- X.internAtom dpy "_NET_WM_STATE_STICKY" False
  wmStateAbove <- X.internAtom dpy "_NET_WM_STATE_ABOVE" False
  wmStateFullscreen <- X.internAtom dpy "_NET_WM_STATE_FULLSCREEN" False
  X.changeProperty32 dpy owin wmState atom X.propModeReplace $
    [ fi wmStateAbove,
      fi wmStateSticky,
      fi wmStateFullscreen
    ]
  pixm <- X.createPixmap dpy owin (fi owidth) (fi oheight) dpth
  gc <- X.createGC dpy owin
  X.mapWindow dpy owin
  X.mapWindow dpy iwin
  when args.passive $ do
    spixm <- X.createPixmap dpy owin (fi swidth) (fi sheight) 1
    sgc <- X.createGC dpy spixm
    X.setBackground dpy sgc 0
    X.setForeground dpy sgc 1
    X.fillRectangle dpy spixm sgc 0 0 0 0
    X.xshapeCombineMask dpy iwin X.shapeInput 0 0 spixm X.shapeSet
    X.xshapeCombineMask dpy owin X.shapeInput 0 0 spixm X.shapeSet
  X.selectInput dpy iwin X.buttonPressMask
  X.selectInput dpy owin (X.buttonPressMask .|. X.exposureMask)
  X.selectInput dpy root X.substructureNotifyMask
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
