module Main where
import Vector
import Transforms
import Constants
import Sphere
import Ray
import Renderable

beta = 0.4
gamma = cosh . atanh $ beta

dirs = map (\(x, y) -> Vector3 x (-1) y) [(j, i) | i <- [-1, -0.998 .. 1], j <- [-1, -0.998 .. 1]]
norms = map (\v -> divide v (norm v)) dirs
rays = map rayify norms
	where
	rayify (Vector3 x y z) = Ray {start = (Vector4 0 (-beta) 2 0), direction = light_speed `mult` (Vector4 (-1) x y z)}

intscts = map (collides (Sphere {position = (Vector4 0 0 0 0), radius = 1, velocity = (Vector4 (gamma * light_speed) (beta * gamma * light_speed) 0 0)})) rays

main :: IO ()
main = do
	putStrLn "P2"
	putStrLn "1001 1001 255"
	mapM_ (\r -> if r > 0 then print . round $ (255 / (r ** 2)) else print 0) intscts
