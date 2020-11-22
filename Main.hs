module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event)
import qualified Data.Map as Map
--import Policy

type Coord = (Float, Float)

width, height, offset :: Int
width = 1000
height = 1000
offset = 0
       
window :: Display
window = InWindow "Field" (width, height) (offset, offset)

background :: Color
background = white

freq :: Int
freq = 15
             
main :: IO ()
--main = display window background $ render initialState
main = play window background freq initialState render handleEvent evolveGame

evolveGame :: Float -> GameState -> GameState
evolveGame _ g = stepUnits g (evalPolicies g)

handleEvent :: Event -> GameState -> GameState
handleEvent event gameState = gameState


-- | Data type describing the state of the game.
data GameState = GameState
  { units :: Map.Map String Unit
  , policies :: Map.Map String Policy
  , controllers :: Map.Map String Controller
  }
  
instance Show GameState where
  show gamestate = show (units gamestate)


type Units = Map.Map String Unit

-- can put a label in the unit itself to denote who controls it?
-- no. instead, making a separate Map to store the labels.
-- can add labels to denote more opponents or neutral units
data Controller = Me | Enemy
  deriving Show

type Controllers = Map.Map String Controller

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

ranged :: Unit
ranged = Unit
  { name       = "ranged"
  , health     = 60
  , speed      = 1.5
  , attack     = 6
  , attackType = Ranged 4.5
  , position   = (0, 0)
  }

-- feel like changing the ordering here
translateUnit :: Direction -> Unit -> Unit
translateUnit (x, y) unit =
  unit { position = translateCoord (x, y) (position unit) }

translateCoord :: Coord -> Coord -> Coord
translateCoord (x, y) (a, b) = (x + a, y + b)

myInitialMeleePositions, enemyInitialMeleePositions :: [Coord]
myInitialMeleePositions = [(-100, -250), (100, -250)]
enemyInitialMeleePositions = [(-100, 250), (100, 250)]
myInitialRangedPositions = [(-100, -300), (100, -300)]
enemyInitialRangedPositions = [(-100, 300), (100, 300)]

initialState :: GameState
initialState = GameState
  { units = initialUnits
  , policies = initialPolicies
  , controllers = initialControllers
  }

initialUnits = Map.union myInitialUnits enemyInitialUnits

initialControllers :: Controllers
initialControllers = Map.union
  (Map.map (\_ -> Me) myInitialUnits)
  (Map.map (\_ -> Enemy) enemyInitialUnits)

-- The keys are melee1, melee2, ..., ranged1, ranged2, etc.
myInitialUnits, enemyInitialUnits :: Map.Map String Unit
myInitialUnits = Map.fromList $ map (\unit -> (name unit, unit)) $
  zipWith modifyName (map show [1..]) meleeUnits ++
  zipWith modifyName (map show [1..]) rangedUnits
  where
    meleeUnits = [translateUnit coord melee | coord <- myInitialMeleePositions]
    rangedUnits = [translateUnit coord ranged | coord <- myInitialRangedPositions]

enemyInitialUnits = Map.fromList $ map (\unit -> (name unit, unit)) $
  zipWith modifyName (map show [100..]) meleeUnits ++
  zipWith modifyName (map show [100..]) rangedUnits
  where
    meleeUnits = [translateUnit coord melee | coord <- enemyInitialMeleePositions]
    rangedUnits = [translateUnit coord ranged | coord <- enemyInitialRangedPositions]

-- related to a lens. all the modifications can be done with a lens
modifyName :: String -> Unit -> Unit
modifyName suffix unit =
  unit { name = (name unit) ++ suffix }
  

border :: Picture
border = Color black $
  line [(-480, -420), (-480, 420), (480, 420), (480, -420), (-480, -420)]

render :: GameState -> Picture
render gameState = pictures $ [border]
  ++ foldr go [] (Map.assocs $ controllers gameState)
  where
    allUnits = units gameState
    go (id, controller) ps = case controller of
          Me    -> (drawMyUnit $ allUnits Map.! id):ps
          Enemy -> (drawEnemyUnit $ allUnits Map.! id):ps

-- translate is a Gloss function with:
-- translate :: Float -> Float -> Picture -> Picture
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






-----------------

type Decisions = Map.Map String Decision

data Decision = Move Direction | Attack Unit
               deriving Show

type Direction = (Float, Float)

-- Use this to implement a dummy policy and see some time evolution.
moveUpward :: Decision
moveUpward = Move (0, 1)

moveDownward :: Decision
moveDownward = Move (0, -1)

stepUnits :: GameState -> Decisions -> GameState
stepUnits gameState decs =
  let unresolved = applyDecisions gameState decs
  in  resolve unresolved

-- The plan is, at every step, use the policies for each unit (the one-to-one relation btw units and policies could change)
-- to come up with a decision for every unit (this /will/ have a one-to-one relationship)
-- and then use stepGame to evolve the gameState.

-- Can I do this conveniently by just transforming the values for each key in the map?
-- Since I have this simple one-to-one relationship for both policies and decisions right now?
-- Is Map a functor?
evalPolicies :: GameState -> Decisions
evalPolicies g = Map.map (\p -> p (units g)) (policies g)



-- The Decisions come from a Policy
-- Loop through every unit, and from its Policy, generate a decision
-- use a separate Map for the Policies, or store them directly in the Units?
type Policies = Map.Map String Policy

-- A policy makes a decision based on the world, and not what the world is thinking
-- (why this is Units and not GameState; GameState includes Policies which is what other units are thinking)
type Policy = Units -> Decision

-- initial policies--move forward for my units, move downward for enemy units
myInitialPolicy :: Policy
myInitialPolicy = \_ -> moveUpward

enemyInitialPolicy :: Policy
enemyInitialPolicy = \_ -> moveDownward

-- make a similar map (for each of my and enemy) with the keys from the two maps of GameState
-- and make the values the above
myInitialPolicies, enemyInitialPolicies :: Map.Map String Policy
myInitialPolicies = Map.fromList $ map (\k -> (k, myInitialPolicy)) $ Map.keys myInitialUnits
enemyInitialPolicies = Map.fromList $ map (\k -> (k, enemyInitialPolicy)) $ Map.keys enemyInitialUnits

initialPolicies = Map.union myInitialPolicies enemyInitialPolicies


-- will end up creating a list of effects
-- that have to be resolved (units may leave)
-- NB: move, then attack, when resolving

-- do i need more information about the world? eg to determine which unit is attacked? is that determined here or before, when resolving the policy? -> before
applyDecision :: Unit -> Decision -> (Effect, Unit)
applyDecision unit dec = case dec of
  Move dir     -> (Nil, moveInDir dir unit)
  Attack unit' -> (Engagement (name unit) (name unit'), unit)

-- the strings here are unit names/identifiers
-- Engagement represents one unit attacking another. And the damage that happens.
data Effect = Engagement String String
            | Nil
            deriving Show


-- this resembles an Applicative computation
-- this func assumes both GameState and Decisions are in a Map.Map String (what if one is in a list?)
-- assocs returns tuples of keys and vals in a map in ascending key order
-- Want the results to be independent of the order in which the apply happens.
applyDecisions :: GameState -> Decisions -> Unresolved
applyDecisions gameState decisions =
  injectGameState $ merge (Map.assocs (units gameState)) (Map.assocs decisions) [] Map.empty
  where
    merge _ [] acc units = (acc, units)
    merge [] _ acc units = (acc, units)
    merge (x:xs) (y:ys) acc units
      | (fst x) == (fst y) = merge xs ys ((fst res):acc) (insert' (snd res) units)
      | (fst x) < (fst y)  = merge xs (y:ys) acc units
      | otherwise          = merge (x:xs) ys acc units
      where  -- can i put this at higher level?
        res = applyDecision (snd x) (snd y)
        insert' u units = Map.insert (name u) u units
    injectGameState (es, us) = (es, gameState { units = us })


-- play with this
type Unresolved = ([Effect], GameState)


-- go through the effects, and apply them one by one to the unit in units,
-- returning a new gamestate each time. This is just looping through this stuff
resolve :: Unresolved -> GameState
resolve (effects, gameState) = foldr applyEffect gameState effects

-- want to turn the effects into animation effects, as well


-- need to write code for how an engagement affects GameState
-- find the unit first in the Engagement
-- get its attack info
-- find the second unit in the Engagement
-- get its defense info
-- find the damage
-- do the damange to the second unit
-- return a new gamestate with the second unit damaged or deleted etc.


-- | This is the meat.
applyEffect :: Effect -> GameState -> GameState
applyEffect effect gameState = case effect of
  Nil -> gameState
  Engagement id1 id2 ->
    let unit1 = (units gameState) Map.! id1 
        unit2 = (units gameState) Map.! id2
        inrange = inRange unit1 unit2
        damage = (attack unit1)
        currhealth = (health unit2)
        allUnits = units gameState
    in  if damage < currhealth
    then gameState { units = Map.insert id2 (unit2 { health = currhealth - damage}) allUnits}
    else gameState { units = Map.delete id2 allUnits}
    
    



inRange :: Unit -> Unit -> Bool
inRange unit1 unit2 = case (attackType unit1) of
  Melee -> distance (position unit1) (position unit2) <= meleeRange
  Ranged r -> distance (position unit1) (position unit2) <= r


  


moveInDir :: Direction -> Unit -> Unit
moveInDir dir unit = translateUnit (stretch (speed unit) dir) unit


stretch :: Float -> Direction -> Direction
stretch u (x, y) = (u * x / sqrt(x^2 + y^2), u * y / sqrt(x^2 + y^2))


distance :: Coord -> Coord -> Float
distance (x1, y1) (x2, y2) = sqrt(x^2 + y^2)
  where x = x2 - x1
        y = y2 - y1
