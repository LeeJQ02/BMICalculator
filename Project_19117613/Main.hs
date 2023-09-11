import Text.Printf

data BMI = Underweight | Normal | Overweight | Obese | ExtremelyObese 
instance Show BMI where
  show Underweight = "You are Underweight"
  show Normal = "You are Normal"
  show Overweight = "You are Overweight"
  show Obese = "You are Obese"
  show ExtremelyObese = "You are Extremely Obese"

metricBMI :: Fractional a => a -> a -> a
metricBMI weightKg heightCm = weightKg / (heightCm / 100)^2

imperialBMI :: Fractional a => a -> a -> a
imperialBMI weightLbs heightIn = (weightLbs * 703) / heightIn^2

result :: (Ord a, Fractional a) => a -> a -> IO()
result x y  | bmi < 18.5 = print Underweight
            | bmi < 24.9 = print Normal
            | bmi < 29.9 = print Overweight
            | bmi < 34.9 = print Obese
            | otherwise = print ExtremelyObese
            where bmi = metricBMI x y

bmiChart :: [String]
bmiChart = ["Underweight     - < 18.5","Normal          - 18.5 ~ 24.9", "Overweight      - 25 ~ 29.9", "Obese           - 30 ~ 34.9", "Extremely Obese - > 35"]

welcomeMenu :: IO String
welcomeMenu = do
  putStrLn "\n============================================"
  putStrLn "        Welcome to BMI Calculator"
  putStrLn "============================================"
  putStrLn "1. Calculate BMI"
  putStrLn "2. View BMI Chart"
  putStrLn "0. Exit"
  putStrLn "============================================\n"
  getLine

unitMenu :: IO String
unitMenu = do
  putStrLn "\n============================================"
  putStrLn "1. Imperial unit system (lbs/in)"
  putStrLn "2. Metric unit system (kg/cm)"
  putStrLn "3. Back"
  putStrLn "0. Exit"
  putStrLn "============================================\n"
  getLine

continueMenu :: IO String
continueMenu = do
  putStrLn "\n============================================"
  putStrLn "Do you wish to continue?"
  putStrLn "1. Continue"
  putStrLn "0. Exit"
  putStrLn "============================================\n"
  getLine

welcomeSelection :: String -> IO ()
welcomeSelection "1" = unitMenu >>= unitSelection 
welcomeSelection "2" = do
  mapM_ print bmiChart
  welcomeMenu >>= welcomeSelection

welcomeSelection "0" = putStrLn "Program ends..."
welcomeSelection _ = putStrLn "Input Error" >> welcomeMenu >>= welcomeSelection

unitSelection :: String -> IO ()
unitSelection "1" = do
  putStrLn "\n============================================"
  putStrLn "Please enter your weight in pounds(lbs)"
  weightString <- getLine 
  let weight = read weightString :: Double
  putStrLn "Please enter your height in inches(in)"
  heightString <- getLine
  let height = read heightString :: Double
  let bmi = imperialBMI weight height
  let bmiString = show bmi
  putStrLn $ "Your weight is " ++ weightString ++ "lbs and height is " ++ heightString ++ "in and your BMI is " ++ printf "%.4s" bmiString

  if bmi < 18.5
    then putStrLn "You are Underweight"
  else if bmi >= 18.5 && bmi <= 24.9
    then putStrLn "You are Normal"
  else if bmi >= 25 && bmi <= 29.9
    then putStrLn "You are Overweight"
  else if bmi >= 30 && bmi <= 34.9
    then putStrLn "You are Obesity"
  else
    putStrLn "You are Extereme Obesity"

  continueMenu >>= continueSelection

unitSelection "2" = do
  putStrLn "\n============================================"
  putStrLn "Please enter your weight in kilogram(kg)"
  weightString <- getLine
  let weight = read weightString :: Double
  putStrLn "Please enter your height in centimeter(cm)"
  heightString <- getLine
  let height = read heightString :: Double
  let bmi = metricBMI weight height
  let bmiString = show bmi
  putStrLn $ "Your weight is " ++ weightString ++ "kg and height is " ++ heightString ++ "cm and your BMI is " ++ printf "%.4s" bmiString
  result weight height
  continueMenu >>= continueSelection

unitSelection "3" = welcomeMenu >>= welcomeSelection
unitSelection "0" = putStrLn "Program ends..."
unitSelection _ = putStrLn "Input Error" >> unitMenu >>= unitSelection

continueSelection :: String -> IO()
continueSelection "1" = unitMenu >>= unitSelection
continueSelection "0" = putStrLn "Program ends..."
continueSelection _= putStrLn "Input Error" >> continueMenu >>= continueSelection

main :: IO()
main = welcomeMenu >>= welcomeSelection