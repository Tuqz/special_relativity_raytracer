module Vector where

class Vector a where
    add :: a -> a -> a
    sub :: a -> a -> a
    mult :: Double -> a -> a
    divide :: a -> Double -> a
    dot :: a -> a -> Double
    norm :: a -> Double

data Vector3 = Vector3 Double Double Double
instance Vector Vector3 where 
    add (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)

    sub (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)

    mult a (Vector3 x y z) = Vector3 (a * x) (a * y) (a * z)

    divide (Vector3 x y z) a = Vector3 (x / a) (y / a) (z / a)

    dot (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

    norm v = sqrt (dot v v)

data Vector4 = Vector4 Double Double Double Double
instance Vector Vector4 where 
    add (Vector4 t1 x1 y1 z1) (Vector4 t2 x2 y2 z2) = Vector4 (t1 + t2) (x1 + x2) (y1 + y2) (z1 + z2)

    sub (Vector4 t1 x1 y1 z1) (Vector4 t2 x2 y2 z2) = Vector4 (t1 - t2) (x1 - x2) (y1 - y2) (z1 - z2)

    mult a (Vector4 t x y z) = Vector4 (a * t) (a * x) (a * y) (a * z)

    divide (Vector4 t x y z) a = Vector4 (t / a) (x / a) (y / a) (z / a)

    dot (Vector4 t1 x1 y1 z1) (Vector4 t2 x2 y2 z2) = (t1 * t2) - (x1 * x2) - (y1 * y2) - (z1 * z2)

    norm v = sqrt . abs $ dot v v

getSpatial :: Vector4 -> Vector3
getSpatial (Vector4 _ x y z) = Vector3 x y z

getT :: Vector4 -> Double
getT (Vector4 t _ _ _) = t

getX :: Vector4 -> Double
getX (Vector4 _ x _ _) = x

getY :: Vector4 -> Double
getY (Vector4 _ _ y _) = y

getZ :: Vector4 -> Double
getZ (Vector4 _ _ _ z) = z
