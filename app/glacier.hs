module Glacier where

-- main :: IO ()
-- main = do 
--   let history = simulateTime initGlacier
--   writeFile "output.txt" (unlines (map prettyPrintGlacier history))

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

fallingIcePercentage :: Double
fallingIcePercentage = 0.1

snowFallingAtTop :: Double
snowFallingAtTop = 100

initIceBlocks :: Int -> [IceBlock]
-- similar structure to [x for x in list]
-- init iceblock for pos from 0 to n-1
-- fromIntegral type conversion int to double?
initIceBlocks n = [ IceBlock pos 10 (1000 - fromIntegral pos * 25) | pos <- [0..n-1] ]

initClimate :: Climate
initClimate = Climate { snowfall=0.2, temperature=10}

initGlacier :: GlacierState
initGlacier = GlacierState{
  dayNumber = 1,
  iceBlocks = initIceBlocks 20, 
  climate = initClimate
}

adjustElevation :: IceBlock -> Double -> IceBlock
adjustElevation ice deltaIce = ice { elevation=max 0 (elevation ice + deltaIce)}

snowFallingThreshold :: Double -> Double
snowFallingThreshold differenceInElevation 
    | differenceInElevation >= 30 = differenceInElevation
    | otherwise = 0

fallingSnowBetweenElevations :: IceBlock -> IceBlock -> Double
fallingSnowBetweenElevations block1 block2 = 
    let elevation1 = elevation block1
        elevation2 = elevation block2
        differenceElevation = elevation1 - elevation2
        equalizingAmountForBothElevations = differenceElevation/2 -- subtract this amount from block and add to other to get same elevation
        amountOfSnowMoving = snowFallingThreshold equalizingAmountForBothElevations
    in amountOfSnowMoving

snowFallingDown :: [IceBlock] -> Double -> [IceBlock]
snowFallingDown []  _ = [] 
snowFallingDown [lastBlock] iceFalling = [adjustElevation lastBlock (-1*(fallingIcePercentage*elevation lastBlock) + iceFalling)]
snowFallingDown (first:second:rest) iceFallingFromAbove = 
  let firstAddedSnow = adjustElevation first iceFallingFromAbove
      fallingIce = fallingSnowBetweenElevations firstAddedSnow second
      firstFallenSnow = adjustElevation firstAddedSnow (-1*fallingIce)
    in firstFallenSnow : snowFallingDown (second:rest) fallingIce


glacierYesterday :: GlacierState -> GlacierState
glacierYesterday glacier
    | dayNumber glacier <= 0 = glacier  -- Base case: if it's day 0, just return as is
    | otherwise = 
     let oldBlocks = iceBlocks glacier
         elevationOfFirstBlock = elevation (head oldBlocks)
         newBlocks = snowFallingDown oldBlocks (0.1*elevationOfFirstBlock) 
      in glacier{
        dayNumber = dayNumber glacier + 1,
        iceBlocks = newBlocks
      }
      
simulateTime :: GlacierState -> [GlacierState]
simulateTime glacier
  | dayNumber glacier >= 10 = [glacier]
  | otherwise = 
    let yesterday = glacierYesterday glacier
    in glacier: simulateTime yesterday
    
prettyPrintGlacier :: GlacierState -> String
prettyPrintGlacier glacier = 
  "Day " ++ show (dayNumber glacier) ++ "\n" ++
  unlines (map prettyPrintIceBlock (iceBlocks glacier))
  
prettyPrintIceBlock :: IceBlock -> String
prettyPrintIceBlock block = 
  "Elevation: " ++ show (elevation block) ++ " Thickness: " ++ show (thickness block)
  
