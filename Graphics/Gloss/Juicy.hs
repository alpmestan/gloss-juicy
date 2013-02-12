module Graphics.Gloss.Juicy where

import qualified Data.ByteString.Lazy as L

import Codec.Picture
import Codec.Picture.Types
import Graphics.Gloss.Data.Picture
import Data.Vector.Storable( unsafeToForeignPtr )

-- FOR DEBUGGING
import Debug.Trace (trace)

fromDynamicImage :: DynamicImage -> Maybe Picture
fromDynamicImage (ImageY8 img)     = fromImageY8     img
fromDynamicImage (ImageYF img)     = fromImageYF     img
fromDynamicImage (ImageYA8 img)    = fromImageYA8    img
fromDynamicImage (ImageRGB8 img)   = fromImageRGB8   img
fromDynamicImage (ImageRGBF img)   = fromImageRGBF   img
fromDynamicImage (ImageRGBA8 img)  = fromImageRGBA8  img
fromDynamicImage (ImageYCbCr8 img) = fromImageYCbCr8 img

fromImageRGBA8 :: Image PixelRGBA8 -> Maybe Picture
fromImageRGBA8 img@(Image { imageWidth = w, imageHeight = h, imageData = rawData }) =
  Just $ bitmapOfByteString w h ptr False
    where -- only if needed
          -- swapedImage = pixelMap (\(PixelRGBA8 r g b a) -> PixelRGBA8 a b g r) img 
          (ptr, _, _) = unsafeToForeignPtr rawData -- $ imageData swapedImage
{-# INLINE fromImageRGBA8 #-}

fromImageRGB8 :: Image PixelRGB8 -> Maybe Picture
fromImageRGB8 img = fromImageRGBA8 . promoteImage
{-# INLINE fromImageRGB8 #-}

fromImageY8 :: Image Pixel8 -> Maybe Picture
fromImageY8 = fromImageRGBA8 . promoteImage
{-# INLINE fromImageY8 #-}

fromImageYA8 :: Image PixelYA8 -> Maybe Picture
fromImageYA8 = fromImageRGBA8 . promoteImage
{-# INLINE fromImageYA8 #-}

fromImageYCbCr8 :: Image PixelYCbCr8 -> Maybe Picture
fromImageYCbCr8 = fromImageRGB8 . convertImage
{-# INLINE fromImageYCbCr8 #-}

fromImageYF :: Image PixelF -> Maybe Picture
fromImageYF _ = Nothing
{-# INLINE fromImageYF #-}

fromImageRGBF :: Image PixelRGBF -> Maybe Picture
fromImageRGBF _ = Nothing
{-# INLINE fromImageRGBF #-}
