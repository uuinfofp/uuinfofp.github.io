import qualified Data.Map as Map
import qualified Data.List as List

data Platform = Platform Int Int Int String deriving (Show,Eq)
              -- l r y name

platformName (Platform _ _ _ n) = n

platformHeight (Platform _ _ y _) = y

world :: [Platform]
world = [ Platform 0 264 0 "a"
        , Platform 1 48 32 "b"
        , Platform 32 112 48 "c"
        , Platform 80 144 16 "d"
        , Platform 128 192 64 "e"
        , Platform 80 96 80 "f"
        , Platform 16 72 96 "g"
        , Platform 176 240 32 "h"
        , Platform 208 224 16 "i"
        , Platform 160 208 45 "j"
        , Platform 160 224 96 "k"
        ]

queries = [ (56, 64)
          , (128, 40)
          , (176, 56)
          ]

answers = [ "c", "d", "j" ]


type SlabDS     = Map.Map Int Platform

type PointLocDS = Map.Map Int SlabDS

data EventKind = Insert | Delete deriving (Show,Eq)
type Event = (Int,EventKind,Platform)

-- | Compute all x-coordinates where the vertical line starts/stops to
-- intersect a platform.
events :: [Platform] -> [Event]
events = List.sortOn (\(a,b,c) -> a) . concatMap (\p@(Platform l r _ _) -> [(l,Insert,p),(r,Delete,p)])

-- | Build the point location data structure by sweeping a vertical
-- line from left to right over the platforms.
build :: [Platform] -> PointLocDS
build = Map.fromAscList . List.scanl' handle (-10,Map.empty) . events
  where
    handle                    :: (Int,SlabDS) -> Event -> (Int,SlabDS)
    handle (_,m) (x,Insert,p) = (x,Map.insert (platformHeight p) p m)
    handle (_,m) (x,Delete,p) = (x,Map.delete (platformHeight p) m)

-- | finds the ground below (x,y)
query          :: (Int,Int) -> PointLocDS -> Maybe (Int,Platform)
query (x,y) ds = case Map.lookupLE x ds of
                   Nothing    -> Nothing
                   Just (_,m) -> Map.lookupLE y m

-- | The data structure for the input above
myDS :: PointLocDS
myDS = build world

test = map (\q -> query q myDS) queries
