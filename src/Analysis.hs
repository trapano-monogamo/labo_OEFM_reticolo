module Analysis
( calcTheta0
, calcTheta0Error
, calcLatticeStep
, calcLatticeStepError
, calcLambdas
, calcLambdaErrors
, covariance
, linRegPoints
) where

import LabParameters



getRadians :: Float -> Float -> Float
getRadians degs mins = 2 * pi * ((degs / 360) + (mins / (60 * 360)))


covariance :: [(Float,Float)] -> Float
covariance l = (1.0 / (fromIntegral $ length l)) * cov
  where (xs,ys) = unzip l
        xMean = (sum xs) / (fromIntegral $ length xs)
        yMean = (sum ys) / (fromIntegral $ length ys)
        cov = sum $ zipWith (*) (map (\xi -> xi - xMean) xs) (map (\yi -> yi - yMean) ys)



calcTheta0 :: [[Float]] -> [Float]
calcTheta0 = map op
  where op (degs:mins:[]) = getRadians degs mins
        op _ = 0

calcTheta0Error :: [[Float]] -> [Float]
calcTheta0Error = map op
  where op _ = angleError



calcLatticeStep :: Float -> [[Float]] -> [Float]
calcLatticeStep theta0 = map (op theta0)
  where op t0 (m:lambda:degs:mins:[]) = m * lambda / (sin angle)
          where angle = abs $ (getRadians degs mins) - t0
        op _ _ = 0

calcLatticeStepError :: Float -> [[Float]] -> [Float]
calcLatticeStepError theta0 = map (op theta0)
  where op t0 (m:lambda:degs:mins:[]) = abs $ m * lambda * ((cos angle) / ((sin angle)**2)) * (sqrt 2) * angleError
          where angle = abs $ (getRadians degs mins) - t0
        op _ _ = 0



calcLambdas :: Float -> Float -> [[Float]] -> [Float]
calcLambdas step theta0 = map (op step theta0)
  where op d t0 (m:degs:mins:[]) = (d/m) * (sin angle)
          where angle = abs $ (getRadians degs mins) - t0
        op _ _ _ = 0

calcLambdaErrors :: Float -> Float -> Float -> [[Float]] -> [Float]
calcLambdaErrors step stepError theta0 = map (op step theta0)
  where op d t0 (m:degs:mins:[]) = sqrt $ ((sin angle) * stepError / m)**2 + ((cos angle) * d * (sqrt 2) * angleError / m)**2
          where angle = abs $ (getRadians degs mins) - t0
        op _ _ _ = 0


linRegPoints :: Float -> Float -> Float -> [[Float]] -> [(Float,Float,Float,Float)]
linRegPoints d dErr t0 = map op
  where op (m:degs:mins:[]) = ( 1 / (sin theta)
                              , d / m
                              , (sqrt 2) * angleError * (cos theta) / ((sin theta)**2)
                              , dErr / m
                              )
          where theta = abs $ (getRadians degs mins) - t0
        op _  = (0,0,0,0)
