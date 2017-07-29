module Sphere where
import Vector
import Renderable
import Ray
import Transforms
import Constants

--Initial position (according to global frame), radius, velocity
data Sphere = Sphere {position :: Vector4, radius :: Double, velocity :: Vector4}

instance Renderable Sphere where
    collides (Sphere {position=pos, radius=r, velocity=vel}) (Ray {start=r0, direction=dir})
        | and [cond >= 0, (d + sqrt(cond)) <= 0] = convert $ -(d + sqrt cond)/n**2 --if the ray is outside the sphere and intersects
        | cond >= 0 = convert $ (-d + (2 * sqrt cond))/n**2 --ray is inside the sphere
        | otherwise = -1 --Negative times are ignored, so no collision shall be -ve
        where
            convert :: Double -> Double
            convert param = getT $ sub pos (lorentz (-v) (rel_pos `add` (param `mult` rel_dir))) --convert times back to reference frame times

            v = light_speed * (tanh . asinh $ ((norm . getSpatial $ vel)/light_speed)) --recover speed from velocity
            rel_pos = rotFrom vel $ lorentz v $ rotTo vel $ r0 `sub` pos --calculate relative position of sphere in sphere's frame
            rel_dir = rotFrom vel $ lorentz v $ rotTo vel dir --calculate direction of ray in sphere's frame
            spat_pos = getSpatial rel_pos
            spat_dir = getSpatial rel_dir
            d = dot spat_pos spat_dir
            n = norm spat_dir
            cond = d**2 - n**2 * ((norm spat_pos)**2 - r**2)
