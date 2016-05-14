module Graphics.Gloss.Juicy
    (
    -- * Conversion from JuicyPixels' types to gloss' Picture
      fromDynamicImage
	, fromImageRGBA8
	, fromImageRGB8
	, fromImageY8
	, fromImageYA8
	, fromImageYCbCr8

    -- * Loading a gloss Picture from a file through JuicyPixels
    , loadJuicy
    , loadJuicyJPG
    , loadJuicyPNG

    -- * From gloss, exported here for convenience
    , loadBMP
	)
where

import Codec.Picture
    ( readImage
    , readJpeg
    , readPng
    )
import Codec.Picture.Types
    ( convertImage
    , DynamicImage
        ( ImageRGBF
        , ImageYF
        , ImageY8
        , ImageYA8
        , ImageRGB8
        , ImageRGBA8
        , ImageYCbCr8
        )
    , Image
        ( imageWidth
        , imageHeight
        , imageData
        , Image
        )
    , Pixel8
    , PixelYA8
    , PixelRGB8
    , PixelRGBA8
        ( PixelRGBA8
        )
    , PixelYCbCr8
    , Pixel
        ( pixelAt
        )
    , ColorConvertible
        ( promoteImage
        )
    , generateImage
    )
import Graphics.Gloss.Data.Picture
    ( Picture
    )
import Graphics.Gloss.Data.Bitmap
    ( bitmapOfForeignPtr
    , BitmapFormat ( BitmapFormat )
    , PixelFormat ( PxABGR )
    , RowOrder ( BottomToTop )
    , loadBMP
    )
import Data.Vector.Storable        (unsafeToForeignPtr)

-- | Tries to convert a 'DynamicImage' from JuicyPixels to a gloss 'Picture'.  All formats except RGBF and YF should successfully
--   yield a 'Picture'.
fromDynamicImage :: DynamicImage -> Maybe Picture
fromDynamicImage (ImageY8 img)     = Just $ fromImageY8 img
fromDynamicImage (ImageYA8 img)    = Just $ fromImageYA8 img
fromDynamicImage (ImageRGB8 img)   = Just $ fromImageRGB8 img
fromDynamicImage (ImageRGBA8 img)  = Just $ fromImageRGBA8 img
fromDynamicImage (ImageYCbCr8 img) = Just $ fromImageYCbCr8 img
fromDynamicImage (ImageRGBF _)     = Nothing
fromDynamicImage (ImageYF _)       = Nothing

-- Courtesy of Vincent Berthoux, JuicyPixels' author
-- bmp (and thus gloss) starts by the lines at the bottom
-- JuicyPixels does the converse
horizontalSwap :: Image PixelRGBA8 -> Image PixelRGBA8
horizontalSwap img@(Image { imageWidth = w, imageHeight = h }) =
    generateImage swapper w h
      where swapper x y = PixelRGBA8 a b g r
                where PixelRGBA8 r g b a = pixelAt img x (h - y - 1)
{-# INLINE horizontalSwap #-}

-- | O(N) conversion from 'PixelRGBA8' image to gloss 'Picture', where N is the number of pixels.
fromImageRGBA8 :: Image PixelRGBA8 -> Picture
fromImageRGBA8 img@(Image { imageWidth = w, imageHeight = h, imageData = _ }) =
  bitmapOfForeignPtr w h (BitmapFormat BottomToTop PxABGR) ptr True
    where swapedImage  = horizontalSwap img
          (ptr, _, _)  = unsafeToForeignPtr $ imageData swapedImage
{-# INLINE fromImageRGBA8 #-}

-- | Creation of a gloss 'Picture' by promoting (through 'promoteImage') the 'PixelRGB8' image to 'PixelRGBA8' and calling 'fromImageRGBA8'.
fromImageRGB8 :: Image PixelRGB8 -> Picture
fromImageRGB8 = fromImageRGBA8 . promoteImage
{-# INLINE fromImageRGB8 #-}

-- | Creation of a gloss 'Picture' by promoting (through 'promoteImage') the 'PixelY8' image to 'PixelRGBA8' and calling 'fromImageRGBA8'.
fromImageY8 :: Image Pixel8 -> Picture
fromImageY8 = fromImageRGBA8 . promoteImage
{-# INLINE fromImageY8 #-}

-- | Creation of a gloss 'Picture' by promoting (through 'promoteImage') the 'PixelYA8' image to 'PixelRGBA8' and calling 'fromImageRGBA8'.
fromImageYA8 :: Image PixelYA8 -> Picture
fromImageYA8 = fromImageRGBA8 . promoteImage
{-# INLINE fromImageYA8 #-}

-- | Creation of a gloss 'Picture' by promoting (through 'promoteImage') the 'PixelYCbCr8' image to 'PixelRGBA8' and calling 'fromImageRGBA8'.
fromImageYCbCr8 :: Image PixelYCbCr8 -> Picture
fromImageYCbCr8 = fromImageRGB8 . convertImage
{-# INLINE fromImageYCbCr8 #-}

-- | Tries to load an image file into a Picture using 'readImage' from JuicyPixels.
--   It means it'll try to successively read the content as an image in the following order,
--   until it succeeds (or fails on all of them): jpeg, png, bmp, gif, hdr (the last two are not supported)
--   This is handy when you don't know what format the image contained in the file is encoded with.
--   If you know the format in advance, use 'loadBMP', 'loadJuicyJPG' or 'loadJuicyPNG'
loadJuicy :: FilePath -> IO (Maybe Picture)
loadJuicy = loadWith readImage
{-# INLINE loadJuicy #-}

loadJuicyJPG :: FilePath -> IO (Maybe Picture)
loadJuicyJPG = loadWith readJpeg
{-# INLINE loadJuicyJPG #-}

loadJuicyPNG :: FilePath -> IO (Maybe Picture)
loadJuicyPNG = loadWith readPng
{-# INLINE loadJuicyPNG #-}

loadWith :: (FilePath -> IO (Either String DynamicImage)) -> FilePath -> IO (Maybe Picture)
loadWith reader fp = do
    eImg <- reader fp
    return $ either (const Nothing) fromDynamicImage eImg
