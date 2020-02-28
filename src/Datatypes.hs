module Datatypes
  ( datatypesMain
  )
where

data Price = Price Integer deriving (Eq, Show)

data Size = Size Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsRUs | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline Size deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir (Size 1500)

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> Bool
areCars = all isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu

datatypesMain :: IO ()
datatypesMain = do
  putStrLn "Datatypes:"
  putStrLn $ "isCar for 'myCar': " ++ show (isCar myCar)
  putStrLn $ "isCar for 'doge': " ++ show (isCar doge)
  putStrLn $ "isPlane for 'myCar': " ++ show (isPlane myCar)
  putStrLn $ "isPlane for 'doge': " ++ show (isPlane doge)
  putStrLn $ "areCars for '[myCar, urCar]': " ++ show (areCars [myCar, urCar])
  putStrLn $ "areCars for '[myCar, doge]': " ++ show (areCars [myCar, doge])
  putStrLn $ "getManu for 'myCar': " ++ show (getManu myCar)
  putStrLn "\n"
