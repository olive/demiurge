module Demiurge.Pathing.Dijkstra where

import Demiurge.Data.Graph

import qualified Data.PSQueue as Q
import qualified Data.Map as Map

data Path a = Path (Map.Map a Float) -- dist
                   (Map.Map a a)     -- prev
                   (Q.PSQ a Float)   -- prio

mkPath :: Ord a => a -> Path a
mkPath x = Path (Map.singleton x 0)
                (Map.empty)
                (Q.singleton x 0)

rewindPath :: Ord a => Map.Map a a -> a -> [a] -> [a]
rewindPath path end sofar =
    case Map.lookup end path of
        Just next -> rewindPath path next (end:sofar)
        Nothing -> sofar

infinity :: Float
infinity = 1 / 0

getDist :: Ord a => Map.Map a Float -> a -> Float
getDist m x = Map.findWithDefault infinity x m

dijkstraHelper :: (Ord c, Graph a b c)
               => a b
               -> c
               -> c
               -> Path c
               -> Maybe [c]
dijkstraHelper g start goal (Path dist prev prio) =
    case Q.minView prio of
        Just (current, newPrio) ->
            let node = Q.key current in
            let ns = neighbors g node in
            let newP = foldl (processNeighbor node) (Path dist prev newPrio) ns in
            if node == goal
            then Just $ rewindPath prev goal []
            else dijkstraHelper g start goal newP
        Nothing -> Nothing
    where processNeighbor u p@(Path dist' prev' prio') (v, c)=
            let alt = (getDist dist' u) + c in
            let d = getDist dist' v in
            if alt < d
            then let newDist = Map.insert v alt dist' in
                 let newPrev = Map.insert v u prev' in
                 let newPrio = Q.alter (\_ -> Just alt) v prio' in
                 Path newDist newPrev newPrio
            else p

pfind :: (Ord c, Graph a b c)
      => a b
      -> c
      -> c
      -> Maybe [c]
pfind g start goal = dijkstraHelper g start goal $ mkPath start
