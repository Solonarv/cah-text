{-# LANGUAGE
    OverloadedStrings,
    RecordWildCards,
    ViewPatterns,
    ExplicitNamespaces,
    DeriveGeneric
    #-}

module CaHLike.Cards (
    type HoleCard,
    FillerCard(..),
    mkHoleCard,
    combineCards
    ) where

import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as T

import CaHLike.Util.SimpleFormatter

data HoleCard = MkHoleCard {
    hc_raw    :: Text,
    hc_text   :: [Token],
    hc_draw   :: Int,
    hc_blanks :: Int
    } deriving Show

instance Eq HoleCard where (==) = (==) `on` hc_raw

newtype FillerCard = FillerCard { fc_text :: Text }

mkHoleCard :: Text -> Int -> HoleCard
mkHoleCard txt draw = MkHoleCard{..}
  where
    hc_raw    = txt
    hc_draw   = draw
    hc_text   = parse txt
    hc_blanks = holeCount hc_text
    

combineCards :: HoleCard -> [FillerCard] -> Maybe Text
combineCards MkHoleCard{..}
            (map fc_text -> fcts)
    = if length fcts == hc_blanks
        then Just $ combine hc_text fcts
        else Nothing