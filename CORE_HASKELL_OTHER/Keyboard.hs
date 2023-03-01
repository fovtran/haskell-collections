-- Centralize all user input interaction
inputActionMap :: InputMap World
inputActionMap = inputMapFromList [
     (Press 'k' , rotate xdir   5)
    ,(Press 'i' , rotate xdir (-5))
    ,(Press 'j' , rotate ydir   5)
    ,(Press 'l' , rotate ydir (-5))
    ,(Press 'o' , rotate zdir   5)
    ,(Press 'u' , rotate zdir (-5))
    ,(Press 'f' , translate xdir   0.1)
    ,(Press 's' , translate xdir (-0.1))
    ,(Press 'e' , translate ydir   0.1)
    ,(Press 'd' , translate ydir (-0.1))
    ,(Press 'z' , translate zdir   0.1)
    ,(Press 'r' , translate zdir (-0.1))
    ,(Press '+' , zoom    1.1)
    ,(Press '-' , zoom (1/1.1))
    ,(Press 'h' , resize    1.2)
    ,(Press 'g' , resize (1/1.2))
    ]

-- I prefer to set my own name for these types
data World = World {
      angle       :: Point3D
    , scale       :: Scalar
    , position    :: Point3D
    , shape       :: Scalar -> Function3D
    , box         :: Box3D
    , told        :: Time -- last frame time
    }