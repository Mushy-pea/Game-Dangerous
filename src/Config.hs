-- Game :: Dangerous code by Steven Tinsley.  You are free to use this software and view its source code.
-- If you wish to redistribute it or use it as part of your own work, this is permitted as long as you acknowledge the work is by the abovementioned author.

{-# LANGUAGE DeriveGeneric #-}

module Config where

import Graphics.GL.Core42
import GHC.Generics
import Data.Either
import Data.Aeson

data Map = Map {mapFilePath :: [Char], mapFile :: [Char], mapFileVersion :: [Char]} deriving (Generic, Show)

data Assets = Assets {modelDataDir :: [Char], soundDataDir :: [Char]} deriving (Generic, Show)

data SaveGames = SaveGames {gameSavePath :: [Char], currentSave :: Int} deriving (Generic, Show)

data Controls = Controls {pause :: [Char], forward :: [Char], back :: [Char], strafeLeft :: [Char], strafeRight :: [Char],
                          turnLeft :: [Char], turnRight :: [Char], jump :: [Char], lightTorch :: [Char],
                          switchView :: [Char], rotateView :: [Char], fire :: [Char], menuUp :: [Char], menuDown :: [Char]} deriving (Generic, Show)

data Graphics = Graphics {shaderFile :: [Char], resolutionX :: GLsizei, resolutionY :: GLsizei, minFrameT :: Integer, maxLights :: Int,
                          frustumScale1 :: Float} deriving (Generic, Show)

data Sound = Sound {music :: [Char], musicPeriod :: Int} deriving (Generic, Show)

data Physics = Physics {initU :: Float, initV :: Float, initW :: Float, gravity :: Float, friction :: Float, runPower :: Float, jumpPower :: Float,
                        speedScaling :: Float} deriving (Generic, Show)

data Debug = Debug {verboseMode :: [Char], saveGameTest :: [Char]} deriving (Generic, Show)

data Misc = Misc {splashImage :: [Char], probC :: Int, onScreenMetrics :: [Char], versionString :: [Char], surveyStart :: Int, surveySize :: Int}
                 deriving (Generic, Show)

data EngineConfig = EngineConfig {map :: Map, assets :: Assets, saveGames :: SaveGames, controls :: Controls, graphics :: Graphics, sound :: Sound,
                                  physics :: Physics, debug :: Debug, misc :: Misc} deriving (Generic, Show)

defMap = Map {mapFilePath = "/home/steven/Gem-Mining/Maps/", mapFile = "map1.dan", mapFileVersion = "10"}

defAssets = Assets {modelDataDir = "/home/steven/Gem-Mining-Assets/CookedModels/", soundDataDir = "/home/steven/Gem-Mining-Assets/Sounds/"}

defSaveGames = SaveGames {gameSavePath = "/home/steven/Gem-Mining/saved-games/", currentSave = -1}

defControls = Controls {
  pause = "p",
  forward = "w",
  back = "s",
  strafeLeft = "a",
  strafeRight = "d",
  turnLeft = "k",
  turnRight = "l",
  jump = "u",
  lightTorch = "t",
  switchView = "KeyHome",
  rotateView = "KeyEnd",
  fire = " ",
  menuUp = "KeyUp",
  menuDown = "KeyDown"
}

defGraphics = Graphics {shaderFile = "shaders.glsl", resolutionX = 2560, resolutionY = 1440, minFrameT = 15000000, maxLights = 5, frustumScale1 = 1.25}

defSound = Sound {music = "off", musicPeriod = 15040}

defPhysics = Physics {initU = 24.5, initV = 24.5, initW = 0.2, gravity = -1.125, friction = -0.3, runPower = 180, jumpPower = 1.5, speedScaling = 1.25}

defDebug = Debug {verboseMode = "n", saveGameTest = "n"}

defMisc = Misc {splashImage = "off", probC = 0, onScreenMetrics = "low", versionString = "...", surveyStart = -95, surveySize = 190}

defConfig = EngineConfig {Config.map = defMap, assets = defAssets, saveGames = defSaveGames, controls = defControls, graphics = defGraphics, sound = defSound,
                          physics = defPhysics, debug = defDebug, misc = defMisc}

instance FromJSON Map
instance FromJSON Assets
instance FromJSON SaveGames
instance FromJSON Controls
instance FromJSON Graphics
instance FromJSON Sound
instance FromJSON Physics
instance FromJSON Debug
instance FromJSON Misc
instance FromJSON EngineConfig

validateConfig :: Either [Char] EngineConfig -> EngineConfig
validateConfig conf_reg'
  | isRight conf_reg' = fromRight defConfig conf_reg'
  | otherwise = error ("\nError encountered when parsing configuration file: " ++ fromLeft "" conf_reg')

