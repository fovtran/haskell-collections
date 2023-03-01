{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module DeepFlat where


class DeepFlat a b | a -> b where
    dflat :: [a] -> [b]

-- If we flatten a list of lists
instance DeepFlat a b => DeepFlat [a] b where
    dflat = concatMap dflat

-- If we are given a list of non-lists
instance a ~ b => DeepFlat a b where
    dflat = id

test1 = dflat "abracadabra"
-- "abracadabra"

test2 = dflat ["abra","cadabra"]

test3 = dflat [["ab","ra"],["cad","abra"]]
test4 = dflat [[["a","b"],["ra"]],[["cad","abra"]]]