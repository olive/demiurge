module Main where

import Control.Monad.Random
import Antiqua.Game
import Antiqua.Graphics.Assets
import Antiqua.Graphics.Window
import Antiqua.Sound.Audio
import qualified Antiqua.Input.Controls as C

import Demiurge.Common()
import Demiurge.Data.Graph()
import Demiurge.Pathing.Dijkstra()
import Demiurge.Data.Coordinate()
import Demiurge.Utils()
import Demiurge.Worker



mainLoop :: IO ()
mainLoop = do
    win <- createWindow 512 512 "Antiqua Prime"
    --let controls = C.Controls [C.mkTriggerAggregate [C.KeyTrigger GLFW.Key'Space]] :: C.Controls C.TriggerAggregate
    let controls = C.Controls [] :: C.Controls C.TriggerAggregate
    tex <- loadTexture "../16x16.png"
    let assets = undefined :: Assets
    let state = mkWorld

    rng <- getStdGen
    gs <- mkUpdater state (controls, assets, win) rng
    loop controls win gs tex rng

main :: IO ()
main = do
    runAudio mainLoop
