module Main where

import Control.Monad.Random
import Data.Map as Map

import Antiqua.Game
import Antiqua.Sound.Audio
import Antiqua.Graphics.Window
import Antiqua.Graphics.Assets
import qualified Antiqua.Input.Controls as C

import Demiurge.Common()
import Demiurge.Utils()
import Demiurge.World
import Demiurge.Input.ControlMap

enterLoop :: IO ()
enterLoop = do
    let cols = 32
    let rows = 32
    let layers = 10
    win <- createWindow 512 512 "Antiqua Prime"
    --let controls = C.Controls [C.mkTriggerAggregate [C.KeyTrigger GLFW.Key'Space]] :: C.Controls C.TriggerAggregate
    let zUp = C.mkTriggerAggregate [C.WheelTrigger 1]
    let zDown = C.mkTriggerAggregate [C.WheelTrigger (-1)]
    let controls = C.Controls (Map.fromList [(CK'ZDown, zDown),
                                             (CK'ZUp, zUp)])
    tex <- loadTexture "../16x16.png"
    let assets = undefined :: Assets
    let state = mkState cols rows layers

    rng <- getStdGen
    gs <- mkUpdater state (controls, assets, win) rng
    loop controls win gs tex rng

main :: IO ()
main = do
    runAudio enterLoop
