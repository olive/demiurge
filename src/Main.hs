module Main where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception.Base hiding (handle)
import Control.Monad.Random
import Antiqua.Game
import Antiqua.Graphics.Renderer
import Antiqua.Graphics.TileRenderer
import qualified Antiqua.Graphics.Animation as A
import Antiqua.Graphics.Tile
import Antiqua.Data.CP437
import Antiqua.Common
import Antiqua.Graphics.Assets
import Antiqua.Graphics.Window
import Antiqua.Sound.Audio
import qualified Antiqua.Input.Controls as C

import Demiurge.Common()
import Demiurge.Data.Graph()
import Demiurge.Pathing.Dijkstra()
import Demiurge.Data.Coordinate()
import Demiurge.Utils()
import Demiurge.Data.Array3d()
import Demiurge.Worker

instance Drawable (WorldState w c s g t) where
    draw (WorldState world schema wks) tex = do
        let ts = Tileset 16 16 16 16
        let ren = Renderer tex ts
        let tr :: TR XY (Tile CP437)
            tr = empty <+ ((0,0), Tile (:☺) black red)
                       <+ ((0,1), Tile C'B red white)
        render ren tr

instance (C.Control a, RandomGen rng, Goal g, Schema s, Task t, World w c) => Game (WorldState w c s g t) (C.Controls a, Assets, Window) rng where
    runFrame g@(WorldState w s wks) (ctrl, _, _) rng = do
        runRand (thing g) rng
        where thing :: (rng' ~ rng) => WorldState w c s g t -> Rand rng' (WorldState w c s g t)
              thing _ = do
                  let isPressed = C.isPressed $ C.firstGet ctrl
                  _ :: Int <- getRandomR (0, 10)
                  return $ update (WorldState w s wks)


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
