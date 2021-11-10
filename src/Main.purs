module Main where

import Prelude
import Effect (Effect)
import Thermite.DOM (defaultMain)
import Game (spec, initialState)

main :: Effect Unit
main = defaultMain spec (const initialState) "TicTacToe" {}
