main :: IO ()
main = putStrLn (visualizeHelper (simulate 5 initialLand))

initialLand :: Landscape
initialLand = Landscape {
    name = "Great Valley",
    elevation = 100.0,
    causes = []
}

data Event = Erosion String
           | Uplift String
           | ClimateChange String
           deriving (Show)

data Landscape = Landscape {
    name :: String,
    elevation :: Double,
    causes :: [Event]
} deriving (Show)

erode :: Landscape -> Landscape
erode land = land {
    elevation = elevation land - 0.5,
    causes = Erosion "River action" : causes land
}

uplift :: Landscape -> Landscape
uplift land = land {
    elevation = elevation land + 1.0,
    causes = Uplift "Tectonic collision" : causes land
}

simulate :: Int -> Landscape -> Landscape
simulate 0 land = land
simulate n land =
    let nextLand = if even n then erode land else uplift land
    in simulate (n - 1) nextLand
    
visualizeHelper :: Landscape -> String
visualizeHelper land = name land ++ "\n" ++ 
                      "Elevation " ++ show (elevation land) ++ "\n" ++ 
                      "\nHistory:\n" ++ 
                      visualize (zip [1..] (causes land))
    
visualize :: [(Int, Event)] -> String
visualize [] = "----------"
visualize ((firstInt, firstEvent):restElements) = 
  "Step " ++ show firstInt ++ " " ++ show firstEvent ++ "\n" ++ visualize restElements
