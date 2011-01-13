{-# LANGUAGE TemplateHaskell , TypeOperators, RecordWildCards,
ScopedTypeVariables, TypeFamilies #-}
module Accumulate where
import CLasH.HardwareTypes
--type Word = SizedInt D8
type Word = Signed D8

{-# ANN acc TopEntity #-}
{-# ANN acc (InitState 'initAccum) #-}
acc :: Word -> State Word -> (State Word , Word )
acc x (State accState) = (State u, u)
  where u = accState + x

initAccum :: Word
initAccum = 0

