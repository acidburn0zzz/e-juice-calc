{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Control.Monad
import Data.Aeson
import GHC.Generics

version :: String
version = "0.1.0.0"

type Milliliter = Double
type Milligram = Double

type PgAmount = Milliliter
type VgAmount = Milliliter

type BatchSize = Milliliter
type NicotineStrength = Int
type Percentage = Int

data LiquidRatio = LiquidRatio
    { liquidRatioPg :: Percentage
    , liquidRatioVg :: Percentage
    } deriving (Generic, Show)
instance ToJSON LiquidRatio where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON LiquidRatio

data Liquid = Liquid
    { liquidName :: String
    , liquidAmount :: Maybe Milliliter
    , liquidRatio :: LiquidRatio
    , liquidNicotine :: Milligram
    } deriving (Generic, Show)
instance ToJSON Liquid where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Liquid

data SubLiquid = SubLiquid
    { pgAmount :: Milliliter
    , vgAmount :: Milliliter
    } deriving (Generic, Show)
instance ToJSON SubLiquid where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SubLiquid

data Flavor = Flavor
    { flavorName :: String
    , flavorPercentage :: Percentage
    , flavorIsVg :: Bool
    } deriving (Generic, Show)
instance ToJSON Flavor where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Flavor

newtype BaseLiquid = BaseLiquid Liquid
    deriving (Generic, Show, ToJSON, FromJSON)
newtype TargetLiquid = TargetLiquid Liquid
    deriving (Generic, Show, ToJSON, FromJSON)
newtype FlavorLiquid = FlavorLiquid Liquid
    deriving (Generic, Show, ToJSON, FromJSON)

data InputData = InputData
    { inputDataBatchSize :: BatchSize
    , inputDataBaseLiquid :: BaseLiquid
    , inputDataFlavors :: [Flavor]
    , inputDataTargetLiquid :: TargetLiquid
    } deriving (Generic, Show)
instance ToJSON InputData where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON InputData


defaultInputData :: InputData
defaultInputData = InputData {
      inputDataBatchSize = batchSize
    , inputDataBaseLiquid = baseLiquid
    , inputDataFlavors = flavors
    , inputDataTargetLiquid = targetLiquid
    }
    where
        batchSize = 10
        baseLiquid = BaseLiquid Liquid {
              liquidName = "Nicotine Base"
            , liquidAmount = Nothing
            , liquidRatio = LiquidRatio
                { liquidRatioPg = 100
                , liquidRatioVg = 0
                }
            , liquidNicotine = 20
            }
        targetLiquid = TargetLiquid Liquid {
              liquidName = "Target Liquid"
            , liquidAmount = Nothing
            , liquidRatio = LiquidRatio
                { liquidRatioPg = 60
                , liquidRatioVg = 40
                }
            , liquidNicotine = 3
            }
        flavors = []

data RecipeLiquid = RecipeLiquid
    { recipeLiquidName :: String
    , recipeLiquidAmount :: Milliliter
    } deriving (Generic, Show)
instance ToJSON RecipeLiquid where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON RecipeLiquid

data Recipe = Recipe 
    { recipeLiquids :: [RecipeLiquid]
    , recipeSum :: Milliliter
    } deriving (Generic, Show)
instance ToJSON Recipe where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Recipe

flavorToFlavorLiquid :: BatchSize -> Flavor -> FlavorLiquid
flavorToFlavorLiquid batchSize flavor = FlavorLiquid Liquid
    { liquidName = flavorName flavor
    , liquidAmount = Just amount
    , liquidRatio = ratio
    , liquidNicotine = 0
    }
    where
        amount = (batchSize / 100 * (fromIntegral $ flavorPercentage flavor))
        ratio = LiquidRatio
            { liquidRatioPg = if flavorIsVg flavor then 0 else 100
            , liquidRatioVg = if flavorIsVg flavor then 100 else 0
            }

flavorsToFlavorLiquids :: BatchSize -> [Flavor] -> [FlavorLiquid]
flavorsToFlavorLiquids batchSize flavors = map (\f -> flavorToFlavorLiquid batchSize f) flavors

calcSubLiquid :: LiquidRatio -> Milliliter -> SubLiquid
calcSubLiquid lr la = SubLiquid
    { pgAmount = pg
    , vgAmount = vg
    }
    where
        pgPercent = fromIntegral $ liquidRatioPg lr
        vgPercent = fromIntegral $ liquidRatioVg lr
        pg = la / 100 * pgPercent
        vg = la / 100 * vgPercent

sumSubLiquids :: [SubLiquid] -> SubLiquid
sumSubLiquids xs = foldr (\(SubLiquid a1 a2) (SubLiquid b1 b2) -> (SubLiquid (a1+b1) (a2+b2))) (SubLiquid 0 0) xs

calcTargetSubLiquid :: BatchSize -> TargetLiquid -> SubLiquid
calcTargetSubLiquid batchSize (TargetLiquid targetLiquid) = calcSubLiquid (liquidRatio targetLiquid) batchSize

calcBaseSubLiquid :: BatchSize -> TargetLiquid -> BaseLiquid -> SubLiquid
calcBaseSubLiquid batchSize (TargetLiquid targetLiquid) (BaseLiquid baseLiquid) = calcSubLiquid (liquidRatio baseLiquid) baseAmount
    where
        baseAmount = batchSize / (liquidNicotine baseLiquid) * (liquidNicotine targetLiquid)

calcFlavorSubLiquid :: BatchSize -> [FlavorLiquid] -> [(FlavorLiquid, SubLiquid)]
calcFlavorSubLiquid batchSize flavors = map (\(FlavorLiquid f) -> (FlavorLiquid f, calcSubLiquid (liquidRatio f) (safeLiquidAmount f))) flavors
    where
        safeLiquidAmount f = case liquidAmount f of
            Just x  -> x
            Nothing -> 0

flavorLiquidToRecipeLiquid :: FlavorLiquid -> SubLiquid -> RecipeLiquid
flavorLiquidToRecipeLiquid (FlavorLiquid flavorLiquid) (SubLiquid pgAmount vgAmount) = RecipeLiquid
    { recipeLiquidName = liquidName flavorLiquid
    , recipeLiquidAmount = pgAmount + vgAmount
    }

createResultFlavors :: [(FlavorLiquid, SubLiquid)] -> [RecipeLiquid]
createResultFlavors [] = []
createResultFlavors xs = map (\(flavor, amounts) -> flavorLiquidToRecipeLiquid flavor amounts) xs

calc :: InputData -> Recipe
calc inputData = result
    where
        batchSize = inputDataBatchSize inputData
        baseLiquid = inputDataBaseLiquid inputData
        flavors = inputDataFlavors inputData
        targetLiquid = inputDataTargetLiquid inputData

        -- calculate target pg/vg
        (SubLiquid targetPg targetVg) = calcTargetSubLiquid batchSize targetLiquid
        
        -- calculate base pg/vg
        (SubLiquid basePg baseVg) = calcBaseSubLiquid batchSize targetLiquid baseLiquid
        
        -- calculate the pg/vg amounts for the flavors
        flavorLiquids = flavorsToFlavorLiquids batchSize flavors
        calculatedFlavors = calcFlavorSubLiquid batchSize flavorLiquids
        

        (SubLiquid flavorPg flavorVg) = sumSubLiquids $ map (\(flavor, subliquid) -> subliquid) calculatedFlavors
        
        -- calculate pg/vg which needs to be added
        (SubLiquid addPg addVg) = SubLiquid (targetPg - basePg - flavorPg) (targetVg - baseVg - flavorVg)

        -- build recipe
        resultBase = RecipeLiquid
            { recipeLiquidName = "Nicotine base"
            , recipeLiquidAmount = basePg + baseVg
            }
        resultFlavors = createResultFlavors calculatedFlavors
        resultPg = RecipeLiquid
            { recipeLiquidName = "PG"
            , recipeLiquidAmount = addPg
            }
        resultVg = RecipeLiquid
            { recipeLiquidName = "VG"
            , recipeLiquidAmount = addVg
            }

        resultLiquids = [resultBase] ++ resultFlavors ++ [resultPg] ++ [resultVg]

        result = Recipe
            { recipeLiquids = resultLiquids
            , recipeSum = sum $ map (\x -> recipeLiquidAmount x) resultLiquids
            }

printRecipe :: Recipe -> IO ()
printRecipe recipe = do
    forM_ (recipeLiquids recipe) (\rl -> putStrLn $ (recipeLiquidName rl) ++ ": " ++ (show $ recipeLiquidAmount rl) ++ "ml")
    putStrLn $ "Sum: " ++ (show $ recipeSum recipe)
