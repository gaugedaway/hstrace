import qualified Data.ByteString.Lazy as B

-- VECTOR
data Vec a = Vec a a a deriving (Eq, Show)

instance (Num a) => Num (Vec a) where
    (Vec x1 y1 z1) + (Vec x2 y2 z2) = Vec (x1 + x2) (y1 + y2) (z1 + z2)
    (Vec x1 y1 z1) - (Vec x2 y2 z2) = Vec (x1 - x2) (y1 - y2) (z1 - z2)
    negate (Vec x y z) = Vec (-x) (-y) (-z)

dot (Vec x1 y1 z1) (Vec x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

cross (Vec x1 y1 z1) (Vec x2 y2 z2) = Vec (y1 * z2 - z1 * y2)
                                          (z1 * x2 - x1 * z2)
                                          (x1 * y2 - y1 * x2)

len v = sqrt $ dot v v


-- LIGHT RAY
data Ray a = StartDir (Vec a) (Vec a) deriving (Show)


-- PRIMITIVES
data Primitive a = Sphere (Vec a) a deriving (Show)

intersect (StartDir start dir) (Sphere center radius)
    | det < 0 = Nothing
    | otherwise = Just $ ((-b) - sqrt det) / (2 * a) * len dir
    where a = dot dir dir
          h = start - center
          b = 2 * (dot dir h)
          c = dot h h - radius^2
          det = b^2 - 4*a*c


-- TRACING
type Color = [Int]

getDistance [] ray = Nothing
getDistance (x:xs) ray = let int = intersect ray x
                   in case int of Just d -> Just (x, d)
                                  Nothing -> getDistance xs ray

-- Rendering
distanceToColor (Just _) = [255, 0, 0]
distanceToColor Nothing = [0, 0, 0]

getColorFromRay prims ray = distanceToColor $ getDistance prims ray

camHeight = 1.0
camWidth = 1.0

screenWidth = 100
screenHeight = 100

screenToSpace (x, y) = let xRat = x / screenWidth
                           yRat = 1.0 - (y / screenHeight)
                       in ((xRat - 0.5) * camWidth, (yRat - 0.5) * camHeight)

getRay (x, y) = StartDir (Vec 0 0 0) (Vec x y (-2))

getColor prims pos = getColorFromRay prims $ getRay $ screenToSpace pos


-- Sample scene
sphere = Sphere (Vec 0 0 (-5)) 1

primitives = [sphere]


image = map (getColor primitives) [(x, y) | x <- [0..(screenWidth-1)], y <- [0..(screenHeight-1)]]
imageData = B.pack $ concat image
main = B.writeFile "out.data" imageData
