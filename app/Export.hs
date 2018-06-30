module Main where

import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss

size :: (Int, Int)
size = (2000, 2000)

shorthand     = exportFrameToPNG size white
shorthandAnim = exportFramesToPNG size white -- 2xx = yy

main :: IO ()
main = do
    bmp <- loadBMP "orientation.bmp"
    let pic = Pictures [bmp, Color red $ Polygon [(-80,0), (0,80), (80,0)], Circle 80]
    exportFrameToPNG (400,400) white "orientation.png" pic
    --display (InWindow "" (100,80) (0, 0)) white pic
    shorthand "bmp.png"  (bmp)
    shorthand "circle.png"  (circle 25)
    exportFrameToPNG (500,500) white "circles.png" (Pictures (map circle [0,10..250]))
    {-shorthand "Circle.png"  (Circle 50)
    shorthand "polygon.png" (Polygon [(0,0),(50,0),(0,50)])
    shorthand "polygonStack.png" (Pictures [ Color blue   $ poly 1050
                                           , Color red    $ poly 1000
                                           , Color yellow $ poly  950
                                           , Color green  $ poly  100
                                           , Color white  $ poly   50
                                           ]
                                 )
    
    shorthand "polygons.png" (Pictures [ Color blue $ Polygon [(-100,100),( 100, 100)
                                                               ,(100,-100),(-100,-100)]
                                       , Polygon [(0,0),(50,0),(0,50)]
                                       ])
    --shorthandAnim "growing_polgons%d.png" (Color blue . poly) [400,500,600,700,800,900,1000]
    shorthandAnim "growing_text%d.png" (\t -> Scale t t $ Text "###") textFloats
    shorthand "text.png" (Text "###")
    shorthand "text1.png" (Color white $ Text "###")
    shorthand "text2.png" (Scale 40 40 $ Color white $ Text "###")
    shorthand "text3.png" (Scale 80 80 $ Color white $ Text "###")-}

textFloats :: [Float]
textFloats = [0,1..10]

poly :: Float -> Picture
poly l = Polygon [(-l,l),( l,l),(l,-l),(-l,-l)]
