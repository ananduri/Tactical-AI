module Policy where
-- import Main

data Decisions = Decisions
  { myDecisions :: Map.Map String Decision
  , enemyDecisions :: Map.Map String Decision
  } deriving Show

data Decision :: Move Direction
               | Attack Unit
               deriving Show

type Direction = (Float, Float)

moveForward :: Decision
moveForward = Move (0, 1)

stepGame :: GameState2 -> Decisions -> GameState2


-- will end up creating a list of effects
-- that have to be resolved (units may leave)
-- NB: move, then attack, when resolving

-- do i need more information about the world? eg to determin which unit is attacked? is that determined here or before, when resolving the policy? -> before
applyDecision :: Unit -> Decision -> (Effect, Unit)
applyDecision unit dec = case dec of
  Move dir     -> (Nil, moveInDir dir unit)
  Attack unit' -> (Engagement (name unit) (name unit'), unit)

-- the strings here are unit names/identifiers
data Effect = Engagement String String
            | Nil
            deriving Show

-- play with this
type Unresolved = ([Effect], GameState2)

-- this resembles an Applicative computation
-- this func assumes both are in a Map.Map String. but what if one is in a list?
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


  
  unzip $ map $ uncurry applyDecision $ zip decisions gameState



resolve :: Unresolved -> GameState2
resolve (effects, units) = units  -- dummy impl



-- c d -> c u -> c (e, u) -> (c e, c u) or ([e], c u)
-- no, just do
-- c d -> c u -> ([e], cu)
-- since ordering of e doesn't matter (it's a monoid)





moveInDir :: Direction -> Unit -> Unit
moveInDir dir unit = translate (scale (speed unit) dir) unit


scale :: Float -> Direction -> Direction
scale u (x, y) = (u * x / ((x^2 + y^2)^0.5), u * y / ((x^2 + y^2)^0.5))
