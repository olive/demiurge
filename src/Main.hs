{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Monad.Random

import Antiqua.Game
import Antiqua.Sound.Audio
import Antiqua.Graphics.Window
import Antiqua.Graphics.Assets
import qualified Antiqua.Input.Controls as C

import Demiurge.Common()
import Demiurge.Utils()
import Demiurge.World
import Demiurge.Input.ControlMap




instance WindowSettings where
    width = 512
    height = 512
    title = "Antiqua Prime"

enterLoop :: WindowSettings => IO ()
enterLoop = do
    let cols = 32
    let rows = 32
    let layers = 10
    win <- createWindow
    let mk t = C.mkTriggerAggregate [t]
    let ctrl = ControlMap (mk $ C.WheelTrigger 1, mk $ C.WheelTrigger (-1))
    tex <- loadTexture "../16x16.png"
    let assets = undefined :: Assets
    let state = mkState cols rows layers

    rng <- getStdGen
    gs <- mkUpdater state (ctrl, assets, win) rng
    loop ctrl win gs tex rng

main :: IO ()
main = do
    runAudio enterLoop
