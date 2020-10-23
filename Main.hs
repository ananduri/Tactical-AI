module Main where
import Graphics.Gloss

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
main = display window background $ render initialState



-- | Data type describing the state of the game.
data GameState = GameState
  { myUnits    :: [Unit]
  , enemyUnits :: [Unit]
  } deriving Show

-- | Data type describing the properties and current state of a unit.
-- | Can make this "inherit" from some common values later.
data Unit = Unit
  { health  :: Integer       -- ^ When this reaches <= 0, the unit disappears.
  , speed   :: Float         -- ^ How far the unit moves in 1 time step.
  , attack  :: Integer       -- ^ How much damage the unit does when one attack lands.
  , attackType :: AttackType -- ^ Whether the attack is close-range or long-distance, with the range given in the latter case.
  , position :: Coord        -- ^ Location of this unit.
  } deriving Show

data AttackType = Melee | Ranged Float
  deriving Show

melee :: Unit
melee = Unit
  { health     = 100
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
  { health = 60
  , speed = 1.5
  , attack = 6
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
                    

render :: GameState -> Picture
render gameState = pictures $
  myPictures ++ enemyPictures
  where myPictures    = map drawMyUnit (myUnits gameState)
        enemyPictures = map drawEnemyUnit (enemyUnits gameState)


translate' :: Coord -> Picture -> Picture
translate' coord picture = translate (fst coord) (snd coord) picture


drawMyUnit :: Unit -> Picture
drawMyUnit unit = case (attackType unit) of
  Melee    -> translate' (position unit) $ color myMeleeColor $ circleSolid 20
  Ranged _ -> translate' (position unit) $ color myRangedColor $ rectangleSolid 20 20

drawEnemyUnit :: Unit -> Picture
drawEnemyUnit unit = case (attackType unit) of
  Melee    -> translate' (position unit) $ color enemyMeleeColor $ circleSolid 20
  Ranged _ -> translate' (position unit) $ color enemyRangedColor $ rectangleSolid 20 20

myMeleeColor, myRangedColor, enemyMeleeColor, enemyRangedColor :: Color
myMeleeColor = dark blue
myRangedColor = light $ light blue
enemyMeleeColor = dark red
enemyRangedColor = light $ light red


