{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Apecs
import Apecs.Gloss
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import Linear
import System.Exit
import System.Random
import Prelude hiding (Down, Map, exitSuccess, get, modify)

newtype Position = Position (V2 Float) deriving stock (Show)

instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving stock (Show)

instance Component Velocity where type Storage Velocity = Map Velocity

data Target = Target deriving stock (Show)

instance Component Target where type Storage Target = Map Target

data Bullet = Bullet deriving stock (Show)

instance Component Bullet where type Storage Bullet = Map Bullet

data Particle = Particle Float deriving stock (Show)

instance Component Particle where type Storage Particle = Map Particle

data Player = Player deriving stock (Show)

instance Component Player where type Storage Player = Unique Player

newtype Score = Score Int deriving (Show, Num)

instance Semigroup Score where (<>) = (+)

instance Monoid Score where mempty = 0

instance Component Score where type Storage Score = Global Score

newtype Time = Time Float deriving (Show, Num)

instance Semigroup Time where (<>) = (+)

instance Monoid Time where mempty = 0

instance Component Time where type Storage Time = Global Time

makeWorld "World" [''Position, ''Velocity, ''Player, ''Target, ''Bullet, ''Score, ''Time, ''Particle, ''Camera]

type System' a = System World a

type Kinetic = (Position, Velocity)

playerSpeed, bulletSpeed, enemySpeed, xmin, xmax :: Float
playerSpeed = 170
bulletSpeed = 500
enemySpeed = 80
xmin = -100
xmax = 100

hitBonus, missPenalty :: Int
hitBonus = 100
missPenalty = 40

playerPos, scorePos :: V2 Float
playerPos = V2 0 (-120)
scorePos = V2 xmin (-170)

initialize :: System' ()
initialize = do
  playerEty <- newEntity (Player, Position playerPos, Velocity 0)
  return ()

stepPosition :: Float -> System' ()
stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

clampPlayer :: System' ()
clampPlayer = cmap $ \(Player, Position (V2 x y)) ->
  Position (V2 (min xmax . max xmin $ x) y)

incrTime :: Float -> System' ()
incrTime dT = modify global $ \(Time t) -> Time (t + dT)

clearTargets :: System' ()
clearTargets = cmap $ \all@(Target, Position (V2 x _), Velocity _) ->
  if x < xmin || x > xmax
    then Nothing
    else Just all

stepParticles :: Float -> System' ()
stepParticles dT = cmap $ \(Particle t) ->
  if t < 0
    then Right $ Not @(Particle, Kinetic)
    else Left $ Particle (t - dT)

clearBullets :: System' ()
clearBullets = cmap $ \(Bullet, Position (V2 _ y), Score s) ->
  if y > 170
    then Right $ (Not @(Bullet, Kinetic), Score (s - missPenalty))
    else Left ()

handleCollisions =
  cmapM_ $ \(Target, Position posT, etyT) ->
    cmapM_ $ \(Bullet, Position posB, etyB) ->
      when (norm (posT - posB) < 10) $ do
        destroy etyT (Proxy @(Target, Kinetic))
        destroy etyB (Proxy @(Bullet, Kinetic))
        spawnParticles 15 (Position posB) (-500, 500) (200, -50)
        modify global $ \(Score x) -> Score (x + hitBonus)

triggerEvery :: Float -> Float -> Float -> System' a -> System' ()
triggerEvery dT period phase sys = do
  Time t <- get global
  let t' = t + phase
      trigger = floor (t' / period) /= floor ((t' + dT) / period)
  when trigger $ void sys

spawnParticles :: Int -> Position -> (Float, Float) -> (Float, Float) -> System' ()
spawnParticles n pos dvx dvy = replicateM_ n $ do
  vx <- liftIO $ randomRIO dvx
  vy <- liftIO $ randomRIO dvy
  t <- liftIO $ randomRIO (0.02, 0.3)
  newEntity (Particle t, pos, Velocity (V2 vx vy))

step :: Float -> System' ()
step dT = do
  incrTime dT
  stepPosition dT
  clampPlayer
  clearTargets
  clearBullets
  stepParticles dT
  handleCollisions
  triggerEvery dT 0.6 0 $ newEntity (Target, Position (V2 xmin 80), Velocity (V2 enemySpeed 0))
  triggerEvery dT 0.6 0.3 $ newEntity (Target, Position (V2 xmax 120), Velocity (V2 (negate enemySpeed) 0))

handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x - playerSpeed) 0)
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) =
  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x + playerSpeed) 0)
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x + playerSpeed) 0)
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) =
  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 (x - playerSpeed) 0)
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) =
  cmapM_ $ \(Player, pos) -> do
    newEntity (Bullet, pos, Velocity (V2 0 bulletSpeed))
    spawnParticles 7 pos (-80, 80) (10, 100)
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent _ = return ()

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate x y

triangle, diamond :: Picture
triangle = Line [(0, 0), (-0.5, -1), (0.5, -1), (0, 0)]
diamond = Line [(-1, 0), (0, -1), (1, 0), (0, 1), (-1, 0)]

draw :: System' Picture
draw = do
  player <- foldDraw $ \(Player, pos) -> translate' pos . color white . scale 10 20 $ triangle
  targets <- foldDraw $ \(Target, pos) -> translate' pos . color red . scale 10 10 $ diamond
  bullets <- foldDraw $ \(Bullet, pos) -> translate' pos . color yellow . scale 4 4 $ diamond

  particles <- foldDraw $
    \(Particle _, Velocity (V2 vx vy), pos) ->
      translate' pos . color orange $ Line [(0, 0), (vx / 10, vy / 10)]

  Score s <- get global
  let score = color white . translate' (Position scorePos) . scale 0.1 0.1 . Text $ "Score: " ++ show s

  return $ player <> targets <> bullets <> score <> particles

main :: IO ()
main = do
  w <- initWorld
  runWith w $ do
    initialize
    play (InWindow "Shmup" (220, 360) (10, 10)) black 60 draw handleEvent step
