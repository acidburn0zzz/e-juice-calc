{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

module EJuiceCalc where

import Data.Aeson
import GHC.Generics

-- |The version of E-Juice-Calc.
version :: String
version = "1.0.1"

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
    { pg :: Percentage -- ^ Propylene glycol percentage of the liquid.
    , vg :: Percentage -- ^ Vegetable glycerin percentage of the liquid.
    } deriving (Generic, Show)
instance ToJSON LiquidRatio where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON LiquidRatio

-- |Represents a liquid which can be the base liquid, target liquid or flavors.
data Liquid = Liquid
    { name :: String              -- ^ Name of the liquid.
    , amount :: Maybe Milliliter  -- ^ Amount of liquid in milliliters.
    , ratio :: LiquidRatio        -- ^ Propylene glycol / vegetable glycerin ratio.
    , nicotine :: Milligram       -- ^ Nicotine content per milliliter.
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
    { name :: String           -- ^ Name of the flavor.
    , percentage :: Percentage -- ^ How much flavor should be used in percent in the targeted liquid.
    , isVg :: Bool             -- ^ If the flavor is propylene glycol (PG) or vegetable glycerin (VG) based.
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
    { batchSize :: BatchSize       -- ^ Amount of liquid to make.
    , baseLiquid :: BaseLiquid     -- ^ Base liquid.
    , flavors :: [Flavor]          -- ^ List of flavors to be used.
    , targetLiquid :: TargetLiquid -- ^ The desired output liquid.
    } deriving (Generic, Show)
instance ToJSON InputData where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON InputData

-- |The default input data, when none is given.
defaultInputData :: InputData
defaultInputData = InputData {
      batchSize = batchSize
    , baseLiquid = baseLiquid
    , flavors = flavors
    , targetLiquid = targetLiquid
    }
    where
        batchSize = 10
        baseLiquid = BaseLiquid Liquid {
              name = "Nicotine Base"
            , amount = Nothing
            , ratio = LiquidRatio
                { pg = 100
                , vg = 0
                }
            , nicotine = 20
            }
        targetLiquid = TargetLiquid Liquid {
              name = "Target Liquid"
            , amount = Nothing
            , ratio = LiquidRatio
                { pg = 60
                , vg = 40
                }
            , nicotine = 3
            }
        flavors = []

-- |A Liquid in the recipe.
data RecipeLiquid = RecipeLiquid
    { name :: String       -- ^ Name of the liquid.
    , amount :: Milliliter -- ^ Amount of this liquid that has to be used.
    } deriving (Generic, Show)
instance ToJSON RecipeLiquid where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON RecipeLiquid

-- |The calculated recipe of the desired liquid.
data Recipe = Recipe 
    { liquids :: [RecipeLiquid] -- ^ List of recipe liquids that have to be used.
    , sum :: Milliliter         -- ^ The sum of all the liquids' amounts in milliliters.
    } deriving (Generic, Show)
instance ToJSON Recipe where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Recipe

-- |Creates a flavor liquid givin a batch size and a flavor.
flavorToFlavorLiquid :: BatchSize -> Flavor -> FlavorLiquid
flavorToFlavorLiquid batchSize flavor = FlavorLiquid Liquid
    { name = (name :: Flavor -> String) flavor
    , amount = Just amount
    , ratio = ratio
    , nicotine = 0
    }
    where
        amount = (batchSize / 100 * (fromIntegral $ percentage flavor))
        ratio = LiquidRatio
            { pg = if isVg flavor then 0 else 100
            , vg = if isVg flavor then 100 else 0
            }

-- |Given the batch size and a list of flavors it returns a list of flavor liquids.
flavorsToFlavorLiquids :: BatchSize -> [Flavor] -> [FlavorLiquid]
flavorsToFlavorLiquids batchSize = map (flavorToFlavorLiquid batchSize)

-- |Calculates a sub-liquid given a liquid ratio and an amount in milliliters.
calcSubLiquid :: LiquidRatio -> Milliliter -> SubLiquid
calcSubLiquid lr la = SubLiquid
    { pgAmount = cPg
    , vgAmount = cVg
    }
    where
        pgPercent = fromIntegral $ pg lr
        vgPercent = fromIntegral $ vg lr
        cPg = la / 100 * pgPercent
        cVg = la / 100 * vgPercent

-- |Sums up the amounts of a list of sub-liquids.
sumSubLiquids :: [SubLiquid] -> SubLiquid
sumSubLiquids = foldr (\(SubLiquid a1 a2) (SubLiquid b1 b2) -> (SubLiquid (a1+b1) (a2+b2))) (SubLiquid 0 0)

-- |Calculates the sub-liquid of the target liquid.
calcTargetSubLiquid :: BatchSize -> TargetLiquid -> SubLiquid
calcTargetSubLiquid batchSize (TargetLiquid targetLiquid) = calcSubLiquid (ratio targetLiquid) batchSize

-- |Calculates the sub-liquid of the base liquid.
calcBaseSubLiquid :: BatchSize -> TargetLiquid -> BaseLiquid -> SubLiquid
calcBaseSubLiquid batchSize (TargetLiquid targetLiquid) (BaseLiquid baseLiquid) = calcSubLiquid (ratio baseLiquid) baseAmount
    where
        baseAmount = batchSize / (nicotine baseLiquid) * (nicotine targetLiquid)

-- |Calculates the sub-liquid of a flavor liquid and returns them in a tuple.
calcFlavorSubLiquid :: BatchSize -> [FlavorLiquid] -> [(FlavorLiquid, SubLiquid)]
calcFlavorSubLiquid batchSize = map (\(FlavorLiquid f) -> (FlavorLiquid f, calcSubLiquid (ratio f) (safeLiquidAmount f)))
    where
        safeLiquidAmount f = case (amount :: Liquid -> Maybe Milliliter) f of
            Just x  -> x
            Nothing -> 0

-- |Converts a flavor liquid to a recipe liquid.
flavorLiquidToRecipeLiquid :: FlavorLiquid -> SubLiquid -> RecipeLiquid
flavorLiquidToRecipeLiquid (FlavorLiquid flavorLiquid) (SubLiquid pgAmount vgAmount) = RecipeLiquid
    { name = (name :: Liquid -> String) flavorLiquid
    , amount = pgAmount + vgAmount
    }

-- |Creates recipe liquids given a list of flavor liquid and their corresponding sub-liquid.
createResultFlavors :: [(FlavorLiquid, SubLiquid)] -> [RecipeLiquid]
createResultFlavors [] = []
createResultFlavors xs = map (\(flavor, amounts) -> flavorLiquidToRecipeLiquid flavor amounts) xs

-- |Calculates a recipe given the required input data.
calc :: InputData -> Recipe
calc inputData = result
    where
        bs = batchSize inputData
        bl = baseLiquid inputData
        fs = flavors inputData
        tl = targetLiquid inputData

        -- calculate target pg/vg
        (SubLiquid targetPg targetVg) = calcTargetSubLiquid bs tl
        
        -- calculate base pg/vg
        (SubLiquid basePg baseVg) = calcBaseSubLiquid bs tl bl
        
        -- calculate the pg/vg amounts for the flavors
        fls = flavorsToFlavorLiquids bs fs
        calculatedFlavors = calcFlavorSubLiquid bs fls
        

        (SubLiquid flavorPg flavorVg) = sumSubLiquids $ map snd calculatedFlavors
        
        -- calculate pg/vg which needs to be added
        (SubLiquid addPg addVg) = SubLiquid (targetPg - basePg - flavorPg) (targetVg - baseVg - flavorVg)

        -- build recipe
        (BaseLiquid bll) = bl
        blPg = pg $ ratio bll
        blVg = vg $ ratio bll
        resultBase = RecipeLiquid
            { name = "Nicotine base (" ++ (show blVg) ++ " VG / " ++  (show blPg) ++ " PG)"
            , amount = basePg + baseVg
            }
        resultFlavors = createResultFlavors calculatedFlavors
        resultPg = RecipeLiquid
            { name = "PG"
            , amount = addPg
            }
        resultVg = RecipeLiquid
            { name = "VG"
            , amount = addVg
            }

        resultLiquids = [resultBase] ++ resultFlavors ++ [resultPg] ++ [resultVg]

        result = Recipe
            { liquids = resultLiquids
            , sum = Prelude.sum $ map (amount :: RecipeLiquid -> Milliliter) resultLiquids
            }
