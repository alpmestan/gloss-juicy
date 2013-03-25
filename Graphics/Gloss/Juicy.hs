module Graphics.Gloss.Juicy ( fromDynamicImage
	                        , fromImageRGBA8
	                        , fromImageRGB8
	                        , fromImageY8
	                        , fromImageYA8
	                        , fromImageYCbCr8
	                        ) 
where

import Codec.Picture
import Codec.Picture.Types
import Graphics.Gloss.Data.Picture
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
  bitmapOfForeignPtr w h ptr False
    where swapedImage = horizontalSwap img
          (ptr, _, _) = unsafeToForeignPtr $ imageData swapedImage
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
