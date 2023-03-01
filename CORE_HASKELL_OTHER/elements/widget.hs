-- | The foozle function takes a widget and turns it into an
--   infinite list of widgets which alternate between red and
--   yellow.
--
--   <<dummy#diagram=foozleDia&width=300>>
--
foozle :: Widget -> [Widget]
foozle = ...

-- > widget =
-- >   (  stroke (circle 1.25 <> circle 0.75 # reversePath)
-- >   <> mconcat (iterateN 10 (rotateBy (1/10)) (square 0.5 # translateX 1.3))
-- >   )
-- >   # lw 0
-- >
-- > foozleDia =
-- >   hcat' with {sep = 2}
-- >   [ widget # fc black
-- >   , hrule 4 # alignR <> triangle 1 # rotateBy (-1/4) # fc black
-- >   , hcat' with {sep = 0.5} (zipWith fc (cycle [red, yellow]) (replicate 6 widget))
-- >   ]
