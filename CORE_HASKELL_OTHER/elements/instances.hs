{-# LANGUAGE FlexibleInstances #-}

foo :: FooType a => a
foo = bar (return ())

class FooType a where
    bar :: IO () -> a

instance FooType (IO ()) where
    bar = id

instance (Show x, FooType r) => FooType (x -> r) where
    bar s x = bar (s >> print x)


class Testable a
instance Testable Bool
instance (Arbitrary x, Testable r) => Testable (x -> r)

import qualified Data.Map as Map

lookup 1 [(1,2), (3,4)]
Map.lookup 1 Map.empty