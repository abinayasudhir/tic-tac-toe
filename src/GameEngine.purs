module GameEngine (Player(..), Tile(..), Game, Result(..), GameState(..), initialGame, gameState) where

import Prelude
import Data.Tuple (Tuple(..))
import Data.Array (replicate, groupAll, (!!), findMap, notElem)
import Data.Array.NonEmpty as NE
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)

data Player
  = X
  | O

derive instance eqPlayer :: Eq Player

derive instance ordPlayer :: Ord Player

derive instance genericPlayer :: Generic Player _

instance showPlayer :: Show Player where
  show = genericShow

data Tile
  = MarkedBy Player
  | Empty

derive instance eqTile :: Eq Tile

derive instance ordTile :: Ord Tile

derive instance genericTile :: Generic Tile _

instance showTile :: Show Tile where
  show = genericShow

type Game
  = Array Tile

data Result
  = Winner Player
  | Tie

derive instance eqResult :: Eq Result

derive instance ordResult :: Ord Result

derive instance genericResult :: Generic Result _

instance showResult :: Show Result where
  show = genericShow

data GameState
  = Result Result
  | Playing

derive instance eqGameState :: Eq GameState

derive instance genericGameState :: Generic GameState _

instance shwGameState :: Show GameState where
  show = genericShow

initialGame :: Array Tile
initialGame = replicate 9 Empty

-- initialGame = [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
-- initialGame = [MarkedBy O,MarkedBy O,MarkedBy O, Empty, Empty, Empty, Empty, Empty, Empty]
-- initialGame = [MarkedBy X,MarkedBy X,MarkedBy X, Empty, Empty, Empty, Empty, Empty, Empty]
-- initialGame = [MarkedBy X,MarkedBy O,MarkedBy O, MarkedBy O, MarkedBy X, MarkedBy X, MarkedBy X, Empty, MarkedBy O]
winScenarios :: Array (Array Int)
winScenarios =
  [ [ 0, 1, 2 ]
  , [ 3, 4, 5 ]
  , [ 6, 7, 8 ]
  , [ 0, 3, 6 ]
  , [ 1, 4, 7 ]
  , [ 2, 5, 8 ]
  , [ 0, 4, 8 ]
  , [ 2, 4, 6 ]
  ]

scenarioState :: Array (Tuple Tile Int) -> GameState
scenarioState [ Tuple (MarkedBy player) 3 ] = Result $ Winner player

scenarioState [ Tuple Empty 3 ] = Playing

scenarioState [ Tuple (MarkedBy _) _, Tuple Empty _ ] = Playing

scenarioState [ Tuple Empty _, Tuple (MarkedBy _) _ ] = Playing

scenarioState _ = Result Tie

mapper :: Game -> Array Int -> GameState
mapper game ary =
  (game !! _) -- get the value from specific index
    >>> fromMaybe Empty -- convert Maybe Tile to Tile, default to Empty
    <$> ary -- map over the array
    # groupAll -- groupBy Tiles
    <#> (\a -> Tuple (NE.head a) (NE.length a)) -- Get Tile count
    # scenarioState -- get Scenario GameState

gameState :: Game -> GameState
gameState game = case winner of
  Just winningResult -> winningResult
  Nothing -> if tie then Result Tie else Playing
  where
  scenarioStates = (mapper game) <$> winScenarios

  winner =
    findMap
      ( case _ of
          w@(Result (Winner _)) -> Just w
          _ -> Nothing
      )
      scenarioStates

  tie = winner == Nothing && notElem Playing scenarioStates
