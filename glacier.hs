main :: IO ()
main = do 
  let history = simulateTime initGlacier
  putStrLn (unlines (map prettyPrintGlacier history))

type Position = Int  -- Index of the block in the glacier (optional but useful)

data IceBlock = IceBlock{ 
  position  :: Position,   -- Position in the glacier (optional, can be removed)
  thickness :: Double,     -- Ice thickness in meters
  elevation :: Double     -- Elevation in meters
} deriving (Show, Eq)

data Climate = Climate{ 
  snowfall    :: Double,  -- Snowfall in meters per day
  temperature :: Double  -- Average temperature for the day in Celsius
} deriving (Show, Eq)

data GlacierState = GlacierState{ 
  dayNumber :: Int,          -- Number of days since simulation start
  iceBlocks :: [IceBlock],   -- List of ice segments representing the glacier
  climate   :: Climate      -- Climate conditions for the day
} deriving (Show, Eq)

initIceBlocks :: Int -> [IceBlock]
-- similar structure to [x for x in list]
-- init iceblock for pos from 0 to n-1
-- fromIntegral type conversion int to double?
initIceBlocks n = [ IceBlock pos 10 (1000 - fromIntegral pos * 50) | pos <- [0..n-1] ]

initClimate :: Climate
initClimate = Climate { snowfall=0.2, temperature=10}

initGlacier :: GlacierState
initGlacier = GlacierState{
  dayNumber = 10,
  iceBlocks = initIceBlocks 20, 
  climate = initClimate
}

adjustElevation :: IceBlock -> Double -> IceBlock
adjustElevation ice 0 = ice
adjustElevation ice iceLost = ice { thickness=thickness ice + iceLost}

snowFallingDown :: [IceBlock] -> Double -> [IceBlock]
snowFallingDown []  _ = [] 
snowFallingDown [lastBlock] iceFalling = [adjustElevation lastBlock (-1*(iceFalling+0.1*thickness lastBlock))]
snowFallingDown (first:rest) iceFalling = 
  let firstAddedSnow = adjustElevation first iceFalling
      fallingIce = 0.1*thickness firstAddedSnow
      firstFallenSnow = adjustElevation firstAddedSnow (-1*fallingIce)
    in firstFallenSnow : snowFallingDown rest fallingIce


glacierYesterday :: GlacierState -> GlacierState
glacierYesterday glacier
    | dayNumber glacier <= 0 = glacier  -- Base case: if it's day 0, just return as is
    | otherwise = 
     let oldBlocks = iceBlocks glacier
         newBlocks = snowFallingDown oldBlocks 0
      in glacier{
        dayNumber = dayNumber glacier - 1,
        iceBlocks = newBlocks
      }
      
simulateTime :: GlacierState -> [GlacierState]
simulateTime glacier
  | dayNumber glacier <= 0 = [glacier]
  | otherwise = 
    let yesterday = glacierYesterday glacier
    in simulateTime yesterday ++ [glacier]
    
prettyPrintGlacier :: GlacierState -> String
prettyPrintGlacier glacier = 
  "Day " ++ show (dayNumber glacier) ++ "\n" ++
  unlines (map prettyPrintIceBlock (iceBlocks glacier))
  
prettyPrintIceBlock :: IceBlock -> String
prettyPrintIceBlock block = 
  "Elevation: " ++ show (elevation block) ++ " Thickness: " ++ show (thickness block)
  
