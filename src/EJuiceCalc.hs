{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EJuiceCalc where

import Control.Monad
import Data.Aeson
import GHC.Generics

-- |The version of the program.
version :: String
version = "0.1.0.0"

-- |Type alias for milliliters.
type Milliliter = Double
-- |Type alias for milligrams.
type Milligram = Double

-- |Type alias for the amount of propylene glycol in milliliters.
type PgAmount = Milliliter
-- |Type alias for the amount of vegetable glycerin in milliliters.
type VgAmount = Milliliter

-- |Type alias for the amount of juice in milliliters to be made.
type BatchSize = Milliliter
-- |Type alias for the nicotine strength in milligrams
type NicotineStrength = Milligram
-- |Type alias for a percentage.
type Percentage = Int

-- |Propylene glycol / vegetable glycerin ratio of a liquid in percent.
data LiquidRatio = LiquidRatio
    { liquidRatioPg :: Percentage -- ^ Propylene glycol percentage of the liquid.
    , liquidRatioVg :: Percentage -- ^ Vegetable glycerin percentage of the liquid.
    } deriving (Generic, Show)
instance ToJSON LiquidRatio where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON LiquidRatio

-- |Represents a liquid which can be the base liquid, target liquid or flavors.
data Liquid = Liquid
    { liquidName :: String              -- ^ Name of the liquid.
    , liquidAmount :: Maybe Milliliter  -- ^ Amount of liquid in milliliters.
    , liquidRatio :: LiquidRatio        -- ^ Propylene glycol / vegetable glycerin ratio.
    , liquidNicotine :: Milligram       -- ^ Nicotine content per milliliter.
    } deriving (Generic, Show)
instance ToJSON Liquid where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Liquid

-- |Represents the calculated propylen glycol / vegetable glycerin amount in milliliters in a liquid.
data SubLiquid = SubLiquid
    { pgAmount :: Milliliter -- ^ Amount of propylene glycol in milliliters.
    , vgAmount :: Milliliter -- ^ Amount of vegetable glycerin in milliliters.
    } deriving (Generic, Show)
instance ToJSON SubLiquid where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON SubLiquid

-- |Represents a flavor.
data Flavor = Flavor
    { flavorName :: String           -- ^ Name of the flavor.
    , flavorPercentage :: Percentage -- ^ How much flavor should be used in percent in the targeted liquid.
    , flavorIsVg :: Bool             -- ^ If the flavor is propylene glycol (PG) or vegetable glycerin (VG) based.
    } deriving (Generic, Show)
instance ToJSON Flavor where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Flavor

-- |Represents the base liquid.
newtype BaseLiquid = BaseLiquid Liquid
    deriving (Generic, Show, ToJSON, FromJSON)
newtype TargetLiquid = TargetLiquid Liquid
    deriving (Generic, Show, ToJSON, FromJSON)
newtype FlavorLiquid = FlavorLiquid Liquid
    deriving (Generic, Show, ToJSON, FromJSON)

-- |Represents the required data for the calculation.
data InputData = InputData
    { inputDataBatchSize :: BatchSize       -- ^ Amount of liquid to make.
    , inputDataBaseLiquid :: BaseLiquid     -- ^ Base liquid.
    , inputDataFlavors :: [Flavor]          -- ^ List of flavors to be used.
    , inputDataTargetLiquid :: TargetLiquid -- ^ The desired output liquid.
    } deriving (Generic, Show)
instance ToJSON InputData where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON InputData

-- |The default input data, when none is given.
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

-- |A Liquid in the recipe.
data RecipeLiquid = RecipeLiquid
    { recipeLiquidName :: String       -- ^ Name of the liquid.
    , recipeLiquidAmount :: Milliliter -- ^ Amount of this liquid that has to be used.
    } deriving (Generic, Show)
instance ToJSON RecipeLiquid where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON RecipeLiquid

-- |The calculated recipe of the desired liquid.
data Recipe = Recipe 
    { recipeLiquids :: [RecipeLiquid] -- ^ List of recipe liquids that have to be used.
    , recipeSum :: Milliliter         -- ^ The sum of all the liquids' amounts in milliliters.
    } deriving (Generic, Show)
instance ToJSON Recipe where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Recipe

-- |Creates a flavor liquid givin a batch size and a flavor.
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

-- |Given the batch size and a list of flavors it returns a list of flavor liquids.
flavorsToFlavorLiquids :: BatchSize -> [Flavor] -> [FlavorLiquid]
flavorsToFlavorLiquids batchSize flavors = map (\f -> flavorToFlavorLiquid batchSize f) flavors

-- |Calculates a sub-liquid given a liquid ratio and an amount in milliliters.
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

-- |Sums up the amounts of a list of sub-liquids.
sumSubLiquids :: [SubLiquid] -> SubLiquid
sumSubLiquids xs = foldr (\(SubLiquid a1 a2) (SubLiquid b1 b2) -> (SubLiquid (a1+b1) (a2+b2))) (SubLiquid 0 0) xs

-- |Calculates the sub-liquid of the target liquid.
calcTargetSubLiquid :: BatchSize -> TargetLiquid -> SubLiquid
calcTargetSubLiquid batchSize (TargetLiquid targetLiquid) = calcSubLiquid (liquidRatio targetLiquid) batchSize

-- |Calculates the sub-liquid of the base liquid.
calcBaseSubLiquid :: BatchSize -> TargetLiquid -> BaseLiquid -> SubLiquid
calcBaseSubLiquid batchSize (TargetLiquid targetLiquid) (BaseLiquid baseLiquid) = calcSubLiquid (liquidRatio baseLiquid) baseAmount
    where
        baseAmount = batchSize / (liquidNicotine baseLiquid) * (liquidNicotine targetLiquid)

-- |Calculates the sub-liquid of a flavor liquid and returns them in a tuple.
calcFlavorSubLiquid :: BatchSize -> [FlavorLiquid] -> [(FlavorLiquid, SubLiquid)]
calcFlavorSubLiquid batchSize flavors = map (\(FlavorLiquid f) -> (FlavorLiquid f, calcSubLiquid (liquidRatio f) (safeLiquidAmount f))) flavors
    where
        safeLiquidAmount f = case liquidAmount f of
            Just x  -> x
            Nothing -> 0

-- |Converts a flavor liquid to a recipe liquid.
flavorLiquidToRecipeLiquid :: FlavorLiquid -> SubLiquid -> RecipeLiquid
flavorLiquidToRecipeLiquid (FlavorLiquid flavorLiquid) (SubLiquid pgAmount vgAmount) = RecipeLiquid
    { recipeLiquidName = liquidName flavorLiquid
    , recipeLiquidAmount = pgAmount + vgAmount
    }

-- |Creates recipe liquids given a list of flavor liquid and their corresponding sub-liquid.
createResultFlavors :: [(FlavorLiquid, SubLiquid)] -> [RecipeLiquid]
createResultFlavors [] = []
createResultFlavors xs = map (\(flavor, amounts) -> flavorLiquidToRecipeLiquid flavor amounts) xs

-- |Calculates a recipe given the required input data.
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
        (BaseLiquid bl) = baseLiquid
        blPg = liquidRatioPg $ liquidRatio bl
        blVg = liquidRatioVg $ liquidRatio bl
        resultBase = RecipeLiquid
            { recipeLiquidName = "Nicotine base (" ++ (show blVg) ++ " VG / " ++  (show blPg) ++ " PG)"
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
