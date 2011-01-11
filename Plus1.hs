{-# LANGUAGE TemplateHaskell , TypeOperators, RecordWildCards,
ScopedTypeVariables, TypeFamilies #-}
module Plus1 where
import CLasH.HardwareTypes
--type Word = SizedInt D8
type Word = Signed D8

{-# ANN plus1 TopEntity #-}
plus1 :: Word -> State Word -> (State Word , Word)
plus1 x state = (state, p)
              where p = x+1


{- # ANN program TestInput #-}
program :: [Word]
program =
  [1,3,7]
  

simulate' _ _ [] = []
simulate' arch state (i : input) = o : out
  where
    (state' , o) = arch i state
    out = simulate' arch state' input

