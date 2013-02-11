module Graphics.Gloss.Juicy where

import qualified Data.ByteString.Lazy as L

import Codec.Picture
import Codec.Picture.Types
import Graphics.Gloss.Data.Picture

import Debug.Trace (trace)

fromDynamicImage :: DynamicImage -> Maybe Picture
fromDynamicImage (ImageY8 img)     = trace "Y8" $ fromImageY8     img
fromDynamicImage (ImageYF img)     = trace "YF" $ fromImageYF     img
fromDynamicImage (ImageYA8 img)    = trace "YA8" $ fromImageYA8    img
fromDynamicImage (ImageRGB8 img)   = trace "RGB8" $ fromImageRGB8   img
fromDynamicImage (ImageRGBF img)   = trace "RGBF" $ fromImageRGBF   img
fromDynamicImage (ImageRGBA8 img)  = trace "RGBA8" $ fromImageRGBA8  img
fromDynamicImage (ImageYCbCr8 img) = trace "YCbCr8" $ fromImageYCbCr8 img

fromImageRGBA8 :: Image PixelRGBA8 -> Maybe Picture
fromImageRGBA8 img = Just $ bitmapOfByteString (imageWidth img) 
                                               (imageHeight img) 
                                               (L.toStrict $ encodeBitmap img) 
                                               True
{-# INLINE fromImageRGBA8 #-}

fromImageRGB8 :: Image PixelRGB8 -> Maybe Picture
fromImageRGB8 img = Just $ bitmapOfByteString (imageWidth img) 
                                              (imageHeight img) 
                                              (L.toStrict $ encodeBitmap img) 
                                              True 
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