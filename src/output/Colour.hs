module Colour where

import Codec.Picture

data Colour = Colour { r :: {-# UNPACK #-} !Double
                     , g :: {-# UNPACK #-} !Double
                     , b :: {-# UNPACK #-} !Double
                     } deriving (Show)

instance Num Colour where
    (+) s t = Colour (r s + r t) (g s + g t) (b s + b t) 
    (*) s t = Colour (r s * r t) (g s * g t) (b s * b t) 
    (-) s t = Colour (r s - r t) (g s - g t) (b s - b t)
    abs a = Colour (abs $ g a) (abs $ b a) (abs $ g a)
    fromInteger a = Colour v v v where v = fromIntegral a
    signum a = 0*a

pixel :: Colour -> PixelRGB8
pixel a = PixelRGB8 (round $ r a * 255) (round $ g a * 255) (round $ b a * 255)
{-# INLINE pixel #-}

scaleColour :: Colour -> Double -> Colour
scaleColour col fac = Colour (r col * fac) (g col * fac) (b col * fac)
{-# INLINE scaleColour #-}

-- sqrt is for an approximate gamma adjustment
averageColours :: [Colour] -> Colour
averageColours list = Colour (sqrt (sum (map r list) / (fromIntegral $ length list)))
                             (sqrt (sum (map g list) / (fromIntegral $ length list)))
                             (sqrt (sum (map b list) / (fromIntegral $ length list)))