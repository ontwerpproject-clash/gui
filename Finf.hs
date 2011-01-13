{-# LANGUAGE TemplateHaskell , TypeOperators, RecordWildCards,
ScopedTypeVariables, TypeFamilies #-}
module Finf where
import CLasH.HardwareTypes
--type Word = SizedInt D8
type Word = Signed D8

{-# ANN finf TopEntity #-}
finf :: Word -> Word -> State Word -> (State Word, Word)
finf x y state = (newstate,res+1)
	where
		(newstate,res) = telop (x+5) y state

telop :: Word -> Word -> State Word -> (State Word, Word)
telop x y state = (state, (x+y))