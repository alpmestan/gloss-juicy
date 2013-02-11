module Main where

import Codec.Picture
import Graphics.Gloss
import Graphics.Gloss.Juicy
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	case args of 
		[filename] -> do
			tryRead <- readImage filename
			case tryRead of
			  Left err -> putStrLn ("error reading png file " ++ filename ++ ": " ++ err)
			  Right im -> tryConvert im

		_          -> putStrLn "usage: gloss-juicy <foo>.png"
 
  where tryConvert img = case fromDynamicImage img of
  	      Just p  -> displayPic p
  	      Nothing -> putStrLn "couldn't convert" 

displayPic :: Picture -> IO ()
displayPic p@(Bitmap width height _ _) = display (InWindow "PNG Viewer" (width, height) (10, 10))
                                                 white
                                                 p
displayPic _ = putStrLn "WHAT"