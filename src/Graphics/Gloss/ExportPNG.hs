-- author: Samuel Gélineau (gelisam)
-- in response to https://www.reddit.com/r/haskell/comments/3u5s4e/is_there_a_way_to_write_the_frames_of_a_gloss/
-- slightly improved

module Graphics.Gloss.ExportPNG
    ( exportFrameToPNG
    , exportFramesToPNG
    ) where

import Codec.Picture.Types (Image(..), PixelRGBA8)
import Codec.Picture.Png (writePng)
import Control.Monad (forM_)
import Data.Vector.Storable (Vector, unsafeFromForeignPtr0)
import qualified Graphics.Gloss as Gloss
import qualified Graphics.Gloss.Rendering as Gloss
--import Graphics.Gloss.Data.Picture
import Graphics.GL -- as GL*
import qualified Graphics.UI.GLFW as GLFW
import Foreign (ForeignPtr, newForeignPtr_)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (mallocArray)
import Text.Printf (printf)
import GHC.Int
import qualified Graphics.UI.GLUT as GLUT


--windowWidth, windowHeight :: Num a => a
--windowWidth = 1000
--windowHeight = 1000

type Size = (Int, Int)


-- let GLFW bother with the OpenGL initialization
initOpenGL :: (Int, Int) -> IO ()        
initOpenGL (windowWidth, windowHeight) = do
    True <- GLFW.init
    Just w <- GLFW.createWindow
                windowWidth windowHeight
                "gloss-to-file demo"
                Nothing Nothing
    GLFW.makeContextCurrent (Just w)

drawFrame :: Size -> Gloss.Color -> Gloss.State -> Gloss.Picture -> IO ()
drawFrame size bg s p = Gloss.withClearBuffer bg
            $ Gloss.withModelview size   
            $ do
    glColor3f 0 0 0
    Gloss.renderPicture s 1 p

initialize :: Size -> IO Gloss.State
initialize size = do
    s <- Gloss.initState
    initOpenGL size
    return s

-- exported picture will be upside down, no idea why
saveFrameImpl :: Size -> Gloss.Color -> Gloss.State -> FilePath -> Gloss.Picture -> IO ()
saveFrameImpl (windowWidth, windowHeight) c s f p = do
    glDrawBuffer GL_BACK
    drawFrame (windowWidth, windowHeight) c s p
    glReadBuffer GL_BACK 
    imageData <- mallocArray (windowWidth * windowHeight * 4)
    let wW = fromIntegral (windowWidth)  :: GHC.Int.Int32
    let wH = fromIntegral (windowHeight) :: GHC.Int.Int32
    glReadPixels 0 0 wW wH GL_RGBA GL_UNSIGNED_BYTE imageData 
    foreignPtr <- newForeignPtr_ imageData
    let vector = unsafeFromForeignPtr0 foreignPtr (windowWidth * windowHeight * 4)
    let image :: Image PixelRGBA8
        image = Image windowWidth windowHeight vector
    writePng f (Image windowWidth windowHeight vector :: Image PixelRGBA8)
    
    free imageData


-- | Save a gloss Picture to a png file.
--   size is (width, length) these are NOT the dimensions of the gloss Picture but probably need to be higher
exportFrameToPNG :: Size -> Gloss.Color -> FilePath -> Gloss.Picture -> IO ()
exportFrameToPNG size c f p = do
    _ <- GLUT.exit                     -- otherwise 'illegal reinitialization'
    (_,_) <- GLUT.getArgsAndInitialize -- needed for text  https://github.com/elisehuard/game-in-haskell/pull/3
    s <- initialize size
    saveFrameImpl size c s f p


type Animation = Float -> Gloss.Picture

-- | Save a gloss Picture to a png file.
--   size is (width, length) these are NOT the dimensions of the gloss Picture but probably need to be higher
--   FilePath must contain "%d", will be replaced by frame number
--   List of Floats represents the timestamps at which a frame is exported
-- FilePath must contain "%d", will be replaced by frame number
exportFramesToPNG :: Size -> Gloss.Color -> FilePath -> Animation -> [Float] -> IO ()
exportFramesToPNG size c f anim ts = do
    _ <- GLUT.exit
    (_,_) <- GLUT.getArgsAndInitialize 
    s <- initialize size
    forM_ (zip [1..] ts) $ \(n, t) -> do
      let filename = printf f (n :: Int)
      let picture = anim t
      saveFrameImpl size c s filename picture
