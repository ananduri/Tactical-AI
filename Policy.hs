module Policy where
import qualified Data.Map as Map
-- import Main

-- data Decisions = Decisions
--   { myDecisions :: Map.Map String Decision
--   , enemyDecisions :: Map.Map String Decision
--   } deriving Show

type Decisions = Map.Map String Decision

data Decision = Move Direction | Attack Unit
               deriving Show

type Direction = (Float, Float)

-- Use this to implement a dummy policy and see some time evolution.
moveUpward :: Decision
moveUpward = Move (0, 1)

moveDownward :: Decision
moveDownward = Move (0, -1)

stepUnits :: Units -> Decisions -> Units
stepUnits units decs =
  let unresolved = applyDecisions units decs
  in  resolve unresolved

-- The plan is, at every step, use the policies for each unit (the one-to-one relation btw units and policies could change)
-- to come up with a decision for every unit (this /will/ have a one-to-one relationship)
-- and then use stepGame to evolve the gameState.

-- Can I do this conveniently by just transforming the values for each key in the map?
-- Since I have this simple one-to-one relationship for both policies and decisions right now?
-- Is Map a functor?
evalPolicies :: GameState -> Decisions
evalPolicies g = map (\p -> p (units g)) (policies g)



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

-- make a similar map (for each of my and enemy) with the keys from the two maps of GameState2
-- and make the values the above
myInitialPolicies, enemyInitialPolicies :: Map.Map String Policy
myInitialPolicies = Map.fromList $ map (\k -> (k, myInitialPolicy)) $ Map.keys myInitialUnits2
enemyInitialPolicies = Map.fromList $ map (\k -> (k, enemyInitialPolicy)) $ Map.keys enemyInitialUnits2

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
applyDecisions :: GameState2 -> Decisions -> Unresolved
applyDecisions gameState decisions =
  merge (assocs gameState) (assocs decisions) [] Map.empty
  where
    merge _ [] acc units = (acc, units)
    merge [] _ acc units = (acc, units)
    merge (x:xs) (y:ys) acc units
      | (fst x) == (fst y) = merge xs ys ((fst res):acc) (insert' (snd res) units)
      | (fst x) < (fst y)  = merge xs (y:ys) acc
      | otherwise          = merge (x:xs) ys acc
      where
        res = applyDecision x y
        insert' u units = Map.insert (name u) u units


-- play with this
type Unresolved = ([Effect], GameState2)


-- go through the effects, and apply them one by one to the unit in units,
-- returning a new gamestate each time. This is just looping through this stuff
resolve :: Unresolved -> GameState2
-- resolve (effects, units) = units
resolve (effects, units) = foldr applyEffect units effects


-- need to write code for how an engagement affects GameState
-- find the unit first in the Engagement
-- get its attack info
-- find the second unit in the Engagement
-- get its defense info
-- find the damage
-- do the damange to the second unit
-- return a new gamestate with the second unit damaged or deleted etc.


-- | This is the meat.
applyEffect :: Effect -> GameState2 -> GameState2
applyEffect effect gamestate = case effect of
  Nil -> gamestate
  Engagement id1 id2 ->
    let unit1 = (myUnits2 gamestate) ! id1  -- getting both from myUnits?
        enemyUnits = enemyUnits2 gamestate
        unit2 = enemyUnits ! id2
        inrange = inRange unit1 unit2
        damage = (attack unit1)
        hp = (health unit2)
    in  if damage < health
    -- return new gameState with unit2 modified in the enemyUnits map
    then gamestate { enemyUnits2 = insert id2 (unit2 { health = hp - damage}) enemyUnits}
    else gamestate { enemyUnits2 = delete id2 enemyUnits}
    
    



inRange :: Unit -> Unit -> Bool
inRange unit1 unit2 = case (attackType unit1) of
  Melee -> distance (position unit1) (position unit2) <= meleeRange
  Ranged r -> distance (position unit1) (position unit2) <= r


  

          

moveInDir :: Direction -> Unit -> Unit
moveInDir dir unit = translate (scale (speed unit) dir) unit


scale :: Float -> Direction -> Direction
scale u (x, y) = (u * x / ((x^2 + y^2)^0.5), u * y / ((x^2 + y^2)^0.5))


distance :: Coord -> Coord -> Float
