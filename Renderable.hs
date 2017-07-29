module Renderable where
import Ray

class Renderable a where
    collides :: a -> Ray -> Double
    --onCollision :: Renderable -> Ray -> Ray
