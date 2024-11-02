module Analysis
( calcTheta0
, calcTheta0Error
, calcLatticeStep
, calcLatticeStepError
, calcLambdas
, calcLambdaErrors
) where

import LabParameters



getRadians :: Float -> Float -> Float
getRadians degs mins = 2 * pi * ((degs / 360) + (mins / (60 * 360)))



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
  where op t0 (m:lambda:degs:mins:[]) = abs $ m * lambda * angleError * (cos angle) / ((sin angle)**2)
          where angle = abs $ (getRadians degs mins) - t0
        op _ _ = 0



calcLambdas :: Float -> Float -> [[Float]] -> [Float]
calcLambdas step theta0 = map (op step theta0)
  where op d t0 (m:degs:mins:[]) = (d/m) * (sin angle)
          where angle = abs $ (getRadians degs mins) - t0
        op _ _ _ = 0

calcLambdaErrors :: Float -> Float -> [[Float]] -> [Float]
calcLambdaErrors step theta0 = map (op step theta0)
  where op d t0 (m:degs:mins:[]) = sqrt $ ((sin angle) * angleError / m)**2 + ((cos angle) * d * angleError / m)**2
          where angle = abs $ (getRadians degs mins) - t0
        op _ _ _ = 0
