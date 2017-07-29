module Transforms where
import Vector
import Constants

--Effects of aligning vector1 to the +ve x-axis
rotTo :: Vector4 -> Vector4 -> Vector4
rotTo (Vector4 t1 x1 y1 z1) (Vector4 t2 x2 y2 z2) = Vector4 t2 x' y' z'
    where
        theta = atan2 y1 x1
        phi = atan2 z1 (sqrt(x1**2 + y1**2))
        x' = cos(theta)*cos(phi)*x2 + sin(theta)*cos(phi)*y2 + sin(phi)*z2
        y' = -sin(theta) * x2 + cos(theta) * y2
        z' = -cos(theta)*sin(phi)*x2 - sin(theta)*sin(phi)*y2 + cos(phi)*z2

--Undo rotation
rotFrom :: Vector4 -> Vector4 -> Vector4
rotFrom (Vector4 t1 x1 y1 z1) (Vector4 t2 x2 y2 z2) = Vector4 t2 x' y' z'
    where
        theta = atan2 y1 x1
        phi = atan2 z1 (sqrt(x1**2 + y1**2))
        x' = cos(theta)*cos(phi)*x2 - sin(theta)*cos(phi)*y2 - sin(phi)*z2
        y' = sin(theta) * x2 + cos(theta) * y2
        z' = cos(theta)*sin(phi)*x2 - sin(theta)*sin(phi)*y2 + cos(phi)*z2

lorentz :: Double -> Vector4 -> Vector4
lorentz v (Vector4 t x y z) = Vector4 t' x' y z
    where
        w = atanh(v / light_speed)
        t' = cosh(w) * t - sinh(w) * x
        x' = -sinh(w) * t + cosh(w) * x
