{-# LANGUAGE
    OverloadedStrings,
    DeriveGeneric
    #-}

module CaHLike.Util.SimpleFormatter where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid

data Token = Lit Text | Wild deriving (Eq, Show)

parseWithWild :: (Char -> Bool) -> Text -> [Token]
parseWithWild w = T.foldr prepend []
  where
    prepend c ts           | w c = Wild : ts
    prepend c (Lit t : ts)       = Lit (c `T.cons` t ) : ts
    prepend c ts                 = Lit (   T.pack [c]) : ts

parse :: Text -> [Token]
parse = parseWithWild (== '_')

joinWilds :: [Token] -> [Token]
joinWilds (Wild : Wild : ts) = joinWilds (Wild : ts)
joinWilds (Lit t : ts)       = Lit t : joinWilds ts
joinWilds []                 = []

combine_ :: [Token] -> [Text] -> Text
combine_ []             _        = ""
combine_ (Lit t : toks) vs       = t <> combine_ toks vs
combine_ (Wild  : toks) (v : vs) = v <> combine_ toks vs

combine :: [Token] -> [Text] -> Text
combine toks vs = combine_ toks (vs ++ repeat "")
{-# INLINE combine #-}

infix 5 %, %%
(%), (%%) :: Text -> [Text] -> Text
(%) = combine . parse
(%%) = combine . joinWilds . parse
{-# INLINE (%) #-}

holeCount :: [Token] -> Int
holeCount = length . filter (== Wild)