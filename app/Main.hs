{-# LANGUAGE OverloadedStrings #-}

module Main where

import Glacier

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams

main :: IO ()
main = do 
  let history = simulateTime initGlacier
  writeFile "output.txt" (unlines (map prettyPrintGlacier history))

  mapM_ plotGlacierDay history


plotGlacierDay :: GlacierState -> IO ()
plotGlacierDay glacier = toFile def fileName $ do
    layout_title .= ("Elevation Profile - Day " ++ show (dayNumber glacier))
    plot (line "Elevation" [zip ([0..] :: [Double]) (map elevation (iceBlocks glacier))])
  where
    fileName = "images/Day" ++ show (dayNumber glacier) ++ ".svg"