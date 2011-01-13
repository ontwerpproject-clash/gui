

{-# LANGUAGE TemplateHaskell , TypeOperators, RecordWildCards,
ScopedTypeVariables, TypeFamilies #-}
module MuxTest where
import CLasH.HardwareTypes
--type Word = SizedInt D8
type Word = Signed D8

{-# ANN muxTest TopEntity #-}
muxTest :: Word -> State Word -> (State Word , Word)
muxTest x state |x==0     = (state, 10)
                |otherwise= (state, p)
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

