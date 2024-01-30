-- Function to split a string on a specific character
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c (x:xs)
  | x == c    = "" : rest
  | otherwise = (x : head rest) : tail rest
  where
    rest = splitOn c xs

-- Function to parse a CSV row
parseCSVRow :: String -> [String]
parseCSVRow = splitOn '\t'

-- Function to parse CSV lines
parseCSV :: String -> [[String]]
parseCSV = map parseCSVRow . lines

getProbGivenS condition column listCount list = 
  let count = length $ filter (\row -> row !! (column-1) == condition) list 
  in fromIntegral count + 1 / fromIntegral listCount + 1

getColumn column list = map (\row -> row !! (column-1)) (filter (\row -> row !! (column-1) /= "NA") list)

mean column list values =
  sum values / fromIntegral (length values)

stdDev column list mean values =
  sqrt $ sum squaredDifferences / fromIntegral (length squaredDifferences)
  where
    squaredDifferences = map (\x -> (x - mean) ^ 2) values

gaussianPDF x column list =
  (1 / (sqrt (2 * pi) * sigma)) * exp ((-1) * ((x - mu) ** 2) / (2 * sigma ** 2))
 where
   pi = 3.14159265358979323846
   values = map read (getColumn column list)
   mu = mean column list values 
   sigma = stdDev column list mu values

main :: IO ()
main = do
  -- Read the content of the file
  content <- readFile "data_set.txt"
  
  putStrLn "Enter the Sex ('M' or 'F'):"
  sex <- getLine

  putStrLn "Enter age in days (estimate if needed):"
  age <- getLine

  putStrLn "Enter the Drug used (e.g. 'D-penicillamine'):"
  drug <- getLine

  putStrLn "Has Ascitites ('Y' or 'N'):"
  hasAscitites <- getLine

  putStrLn "Has Hepatomegaly ('Y' or 'N'):"
  hasHepatomegaly <- getLine

  putStrLn "Has Spiders ('Y' or 'N'):"
  hasSpiders <- getLine

  putStrLn "Has Edema ('Y', 'S', or 'N'):"
  hasEdema <- getLine

  putStrLn "Enter Bilirubin (mg/dl):"
  bilirubin <- getLine

  putStrLn "Enter Cholesterol (mg/dl):"
  cholesterol <- getLine

  putStrLn "Enter Albumin (gm/dl):"
  albumin <- getLine

  putStrLn "Enter Copper (ug/day):"
  copper <- getLine

  putStrLn "Enter Alkaline Phosphatase (U/liter):"
  alk_Phos <- getLine

  putStrLn "Enter SGOT (U/ml):"
  sgot <- getLine

  putStrLn "Enter Tryglicerides:"
  tryglicerides <- getLine

  putStrLn "Enter Platelets Per Cubic (ml/1000):"
  platelets <- getLine

  putStrLn "Enter prothrombin time (s):"
  prothrombin <- getLine

  putStrLn "Stage (1 to 4):"
  stage <- getLine 

  -- Parse the CSV data
  let csvData = parseCSV content
      totalCount = length csvData
      deathList = filter (\row -> row !! 2 == "D") csvData
      deathCount = length deathList
      aliveList = filter (\row -> row !! 2 /= "D") csvData
      aliveCount = totalCount - deathCount

  -- Get the probability of death for the specific condition
  let probabilityDeath = fromIntegral deathCount / fromIntegral totalCount
      probDGivenAge = gaussianPDF (read age) 5 deathList
      probDGivenSex = getProbGivenS sex 6 deathCount deathList
      probDGivenDrug = getProbGivenS drug 4 deathCount deathList
      probDGivenAscitites = getProbGivenS hasAscitites 7 deathCount deathList
      probDGivenHepatomegaly = getProbGivenS hasHepatomegaly 8 deathCount deathList
      probDGivenSpiders = getProbGivenS hasSpiders 9 deathCount deathList
      probDGivenEdema = getProbGivenS hasEdema 10 deathCount deathList
      probDGivenbilirubin = gaussianPDF (read bilirubin) 11 deathList
      probDGivenCholesterol = gaussianPDF (read cholesterol) 12 deathList
      probDGivenAlbumin = gaussianPDF (read albumin) 13 deathList
      probDGivenCopper = gaussianPDF (read copper) 14 deathList
      probDGivenalk_phos = gaussianPDF (read alk_Phos) 15 deathList
      probDGivenSGOT = gaussianPDF (read sgot) 16 deathList
      probDGivenTryglicerides = gaussianPDF (read tryglicerides) 17 deathList
      probDGivenPlatelets = gaussianPDF (read platelets) 18 deathList
      probDGivenProthrombin = gaussianPDF (read prothrombin) 19 deathList
      probDGivenStage = getProbGivenS stage 20 deathCount deathList

  let probabilityAlive = fromIntegral aliveCount / fromIntegral totalCount
      probAGivenAge = gaussianPDF (read age) 5 aliveList
      probAGivenSex = getProbGivenS sex 6 aliveCount aliveList
      probAGivenDrug = getProbGivenS drug 4 aliveCount aliveList
      probAGivenAscitites = getProbGivenS hasAscitites 7 aliveCount aliveList
      probAGivenHepatomegaly = getProbGivenS hasHepatomegaly 8 aliveCount aliveList
      probAGivenSpiders = getProbGivenS hasSpiders 9 aliveCount aliveList
      probAGivenEdema = getProbGivenS hasEdema 10 aliveCount aliveList
      probAGivenbilirubin = gaussianPDF (read bilirubin) 11 aliveList
      probAGivenCholesterol = gaussianPDF (read cholesterol) 12 aliveList
      probAGivenAlbumin = gaussianPDF (read albumin) 13 aliveList
      probAGivenCopper = gaussianPDF (read copper) 14 aliveList
      probAGivenalk_phos = gaussianPDF (read alk_Phos) 15 aliveList
      probAGivenSGOT = gaussianPDF (read sgot) 16 aliveList
      probAGivenTryglicerides = gaussianPDF (read tryglicerides) 17 aliveList
      probAGivenPlatelets = gaussianPDF (read platelets) 18 aliveList
      probAGivenProthrombin = gaussianPDF (read prothrombin) 19 aliveList
      probAGivenStage = getProbGivenS stage 20 aliveCount aliveList

  let likelihoodDeath = log probDGivenSex + log probDGivenAge + log probDGivenAscitites + log probDGivenHepatomegaly + log probDGivenSpiders + log probDGivenEdema + log probDGivenbilirubin + log probDGivenCholesterol + log probDGivenAlbumin + log probDGivenCopper + log probDGivenalk_phos + probDGivenSGOT + log probDGivenTryglicerides + log probDGivenPlatelets + log probDGivenProthrombin + log probDGivenStage + log probabilityDeath
      likelihoodAlive = log probAGivenSex + log probAGivenAge + log probAGivenAscitites + log probAGivenHepatomegaly + log probAGivenSpiders + log probAGivenEdema + log probAGivenbilirubin + log probAGivenCholesterol + log probAGivenAlbumin + log probAGivenCopper + log probAGivenalk_phos + probAGivenSGOT + log probAGivenTryglicerides + log probAGivenPlatelets + log probAGivenProthrombin + log probAGivenStage + log probabilityAlive

  putStrLn $ "Will Survive: " ++ show (likelihoodDeath < likelihoodAlive)

  putStrLn $ "Likelihood of death: " ++ show (likelihoodDeath)
  putStrLn $ "Likelihood of survival: " ++ show (likelihoodAlive)

