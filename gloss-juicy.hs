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
			putStrLn "Before reading image"
			tryRead <- readImage filename
			case tryRead of
			  Left err -> putStrLn ("error reading png file " ++ filename ++ ": " ++ err)
			  Right im -> putStrLn "Will try to convert now..." >> tryConvert im

		_          -> putStrLn "usage: gloss-juicy <file> -- displays the image in a gloss window"
 
  where tryConvert img = case fromDynamicImage img of
  	      Just p  -> displayPic p
  	      Nothing -> putStrLn "couldn't convert" 

displayPic :: Picture -> IO ()
displayPic p@(Bitmap width height _ _) = display (InWindow "PNG Viewer" (width, height) (10, 10))
                                                 white
                                                 p
displayPic _ = putStrLn "WHAT"