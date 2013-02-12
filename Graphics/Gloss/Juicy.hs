module Graphics.Gloss.Juicy where

import qualified Data.ByteString.Lazy as L

import Codec.Picture
import Codec.Picture.Types
import Graphics.Gloss.Data.Picture

fromDynamicImage :: DynamicImage -> Maybe Picture
fromDynamicImage (ImageY8 img)     = fromImageY8     img
fromDynamicImage (ImageYF img)     = fromImageYF     img
fromDynamicImage (ImageYA8 img)    = fromImageYA8    img
fromDynamicImage (ImageRGB8 img)   = fromImageRGB8   (pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 b g r) img)
fromDynamicImage (ImageRGBF img)   = fromImageRGBF   img
fromDynamicImage (ImageRGBA8 img)  = fromImageRGBA8  (pixelMap (\(PixelRGBA8 r g b a) -> PixelRGBA8 b g r a) img)
fromDynamicImage (ImageYCbCr8 img) = fromImageYCbCr8 img

fromImageRGBA8 :: Image PixelRGBA8 -> Maybe Picture
fromImageRGBA8 img = Just $ bitmapOfByteString (imageWidth img) 
                                               (imageHeight img) 
                                               (L.toStrict $ encodeBitmap img) 
                                               True
{-# INLINE fromImageRGBA8 #-}

fromImageRGB8 :: Image PixelRGB8 -> Maybe Picture
fromImageRGB8 = fromImageRGBA8 . promoteImage
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