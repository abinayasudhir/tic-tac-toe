module Game where

import Prelude
import Data.Array (mapWithIndex, (!!), updateAt, (:), groupAll)
import Data.Array.NonEmpty as NE
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import GameEngine (Player(..), Game, Result(..), Tile(..), initialGame, gameState, GameState(..))
import React (ReactElement) as R
import React.DOM (br', button, div, div', text) as R
import React.DOM.Props as RP
import Thermite as T

data Action
  = Mark Int
  | NewGame
  | NoOp

type State
  = { game :: Game, results :: Array Result, player :: Player }

initialState :: State
initialState = { game: initialGame, results: [], player: X }

togglePlayer :: Player -> Player
togglePlayer X = O

togglePlayer O = X

updateGame :: Int -> State -> State
updateGame index state = case (state.game !! index) of
  Just Empty ->
    let
      updatedGame = updateAt index (MarkedBy state.player) state.game # fromMaybe state.game

      status = gameState updatedGame

      newResults = case status of
        Playing -> state.results
        Result (Winner player) -> (Winner player) : state.results
        _ -> Tie : state.results
    in
      state
        { game = updateAt index (MarkedBy state.player) state.game # fromMaybe state.game
        , player = togglePlayer state.player
        , results = newResults
        }
  _ -> state

performAction :: T.PerformAction State _ Action
performAction NewGame _ _ = void (T.cotransform (\state -> state { game = initialGame, player = X }))

performAction (Mark index) _ _ = void (T.cotransform (updateGame index))

performAction (NoOp) _ _ = void (T.cotransform identity)

render :: T.Render State _ Action
render dispatch _ state _ =
  let
    gameStatus = gameState state.game

    displayStatus :: GameState -> R.ReactElement
    displayStatus Playing = R.text $ "Player " <> show state.player <> "'s turn"

    displayStatus (Result Tie) = R.text "Game Tie"

    displayStatus (Result (Winner player)) = R.text $ "Player " <> show player <> " won!"

    renderTile :: Int -> Tile -> R.ReactElement
    renderTile index tile =
      R.div
        [ RP.className "square"
        , RP.onClick \_ -> dispatch $ if gameStatus == Playing then Mark index else NoOp
        ]
        [ showTile tile
        ]
  in
    [ R.div [ RP.className "game" ] $ mapWithIndex renderTile state.game
    , R.br'
    , R.button [ RP.className "button", RP.onClick \_ -> dispatch NewGame ] [ R.text $ if gameStatus == Playing then "Restart" else "New Game" ]
    , R.div [ RP.className "game-status" ] [ displayStatus gameStatus ]
    , R.br'
    , R.text "Game Stats"
    , gameStats state.results
    ]

showTile :: Tile -> R.ReactElement
showTile Empty = R.text ""

showTile (MarkedBy X) = R.text "X"

showTile (MarkedBy O) = R.text "O"

gameStats :: Array Result -> R.ReactElement
gameStats ary = R.div [] $ showStat <$> results
  where
  results = groupAll ary <#> (\a -> Tuple (NE.head a) (NE.length a))

  showStat :: Tuple Result Int -> R.ReactElement
  showStat (Tuple (Winner player) no) = R.div' [ R.text $ show player <> " : " <> show no ]

  showStat (Tuple (Tie) no) = R.div' [ R.text $ "Tie : " <> show no ]

spec :: T.Spec State (T.WithChildren ()) Action
spec = T.Spec { performAction, render }
