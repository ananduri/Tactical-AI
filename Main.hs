module Main where
import Graphics.Gloss
import qualified Data.Map as Map

type Coord = (Float, Float)

width, height, offset :: Int
width = 1000
height = 1000
offset = 0
       
window :: Display
window = InWindow "Field" (width, height) (offset, offset)

background :: Color
background = white
         
main :: IO ()
main = display window background $ render initialState2


-- | Data type describing the state of the game.
data GameState = GameState
  { myUnits    :: [Unit]
  , enemyUnits :: [Unit]
  } deriving Show

-- this is the one I'm using
-- why have two separate maps? can just store two separate sets of keys/strings/IDs?
-- does that rly solve the problem
-- can put a label in the unit itself to denote who controls it?
data GameState2 = GameState2
  { myUnits2    :: Map.Map String Unit
  , enemyUnits2 :: Map.Map String Unit
  } deriving Show

-- put decisions in a similar Map.Map String (keyed by same values),
-- and can do a mergesort-like walk to produce effects/new unit state


-- | Show type describing the properties and current state of a unit.
-- | Can make this "inherit" from some common values later.
data Unit = Unit
  { name    :: String        -- ^ Name/ID of the unit. Maybe alphanumeric with prefix describing type.
  , health  :: Integer       -- ^ When this reaches <= 0, the unit disappears.
  , speed   :: Float         -- ^ How far the unit moves in 1 time step.
  , attack  :: Integer       -- ^ How much damage the unit does when one attack lands.
  , attackType :: AttackType -- ^ Whether the attack is close-range or long-distance, with the range given in the latter case.
  , position :: Coord        -- ^ Location of this unit.
  } deriving Show

data AttackType = Melee | Ranged Float
  deriving Show

meleeRange :: Float
meleeRange = 1.0

melee :: Unit
melee = Unit
  { name       = "melee"
  , health     = 100
  , speed      = 2.0
  , attack     = 10
  , attackType = Melee
  , position   = (0, 0)
  }
  
-- can make an emacs func to automatically align shit (and multi-level alignment, for types and docstrings? specify all the things to align upon)
-- melee unit has more health, more speed, and more attack.
-- ranged unit range is 3 times its speed.

ranged :: Unit
ranged = Unit
  { name       = "ranged"
  , health     = 60
  , speed      = 1.5
  , attack     = 6
  , attackType = Ranged 4.5
  , position   = (0, 0)
  }


translateUnit :: Unit -> Coord -> Unit
translateUnit unit (x, y) =
  unit { position = translateCoord (x, y) (position unit) }

translateCoord :: Coord -> Coord -> Coord
translateCoord (x, y) (a, b) = (x + a, y + b)

myInitialMeleePositions, enemyInitialMeleePositions :: [Coord]
myInitialMeleePositions = [(-100, -250), (100, -250)]
enemyInitialMeleePositions = [(-100, 250), (100, 250)]
myInitialRangedPositions = [(-100, -300), (100, -300)]
enemyInitialRangedPositions = [(-100, 300), (100, 300)]

myInitialUnits, enemyInitialUnits :: [Unit]
myInitialUnits = [translateUnit melee coord | coord <- myInitialMeleePositions]
  ++ [translateUnit ranged coord | coord <- myInitialRangedPositions]

enemyInitialUnits = [translateUnit melee coord | coord <- enemyInitialMeleePositions]
  ++ [translateUnit ranged coord | coord <- enemyInitialRangedPositions]

initialState :: GameState
initialState = GameState
  { myUnits    = myInitialUnits
  , enemyUnits = enemyInitialUnits
  }

initialState2 :: GameState2
initialState2 = GameState2
  { myUnits2    = myInitialUnits2
  , enemyUnits2 = enemyInitialUnits2
  }  

-- does this work? might be useful
-- newtype Container = Map.Map String

myInitialUnits2, enemyInitialUnits2 :: Map.Map String Unit
myInitialUnits2 = Map.fromList $ map (\unit -> (name unit, unit)) $
  zipWith modifyName (map show [1..]) meleeUnits ++
  zipWith modifyName (map show [1..]) rangedUnits
  where
    meleeUnits = [translateUnit melee coord | coord <- myInitialMeleePositions]
    rangedUnits = [translateUnit ranged coord | coord <- myInitialRangedPositions]

enemyInitialUnits2 = Map.fromList $ map (\unit -> (name unit, unit)) $
  zipWith modifyName (map show [1..]) meleeUnits ++
  zipWith modifyName (map show [1..]) rangedUnits
  where
    meleeUnits = [translateUnit melee coord | coord <- enemyInitialMeleePositions]
    rangedUnits = [translateUnit ranged coord | coord <- enemyInitialRangedPositions]

-- related to a lens
-- all the modifications can be done with a lens
modifyName :: String -> Unit -> Unit
modifyName suffix unit =
  unit { name = (name unit) ++ suffix }
  
  

border :: Picture
border = Color black $
  line [(-480, -420), (-480, 420), (480, 420), (480, -420), (-480, -420)]

render :: GameState2 -> Picture
render gameState = pictures $ [border]
  ++ (Map.elems myPictures)
  ++ (Map.elems enemyPictures)
  where myPictures    = Map.map drawMyUnit (myUnits2 gameState)
        enemyPictures = Map.map drawEnemyUnit (enemyUnits2 gameState)


translate' :: Coord -> Picture -> Picture
translate' coord picture = translate (fst coord) (snd coord) picture


drawMyUnit :: Unit -> Picture
drawMyUnit unit = case (attackType unit) of
  Melee    -> translate' (position unit) $ color myMeleeColor $ rectangleSolid 15 15
  Ranged _ -> translate' (position unit) $ color myRangedColor $ circleSolid 12

drawEnemyUnit :: Unit -> Picture
drawEnemyUnit unit = case (attackType unit) of
  Melee    -> translate' (position unit) $ color enemyMeleeColor $ rectangleSolid 15 15
  Ranged _ -> translate' (position unit) $ color enemyRangedColor $ circleSolid 12

myMeleeColor, myRangedColor, enemyMeleeColor, enemyRangedColor :: Color
myMeleeColor = dark blue
myRangedColor = light $ light blue
enemyMeleeColor = dark red
enemyRangedColor = light $ light red


