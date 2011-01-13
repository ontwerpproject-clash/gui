{-# LANGUAGE TemplateHaskell , TypeOperators, RecordWildCards,
ScopedTypeVariables, TypeFamilies #-}
module Plus1 where
import CLasH.HardwareTypes
--type Word = SizedInt D8
type Word = Signed D8

{-# ANN tupleTest TopEntity #-}
tupleTest :: (Word,Word) -> State (Word,Word) -> (State (Word,Word) , (Word,Word))
tupleTest (x,y) state = (state, (y,x))


{- # ANN program TestInput #-}
program :: [(Word,Word)]
program =
  [(1,2),(3,1),(7,2)]
  

simulate' _ _ [] = []
simulate' arch state (i : input) = o : out
  where
    (state' , o) = arch i state
    out = simulate' arch state' input

