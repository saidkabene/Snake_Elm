module Main exposing (..)

import Random exposing (int, generate)
import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Time exposing (Posix)
import Setters
import Update
import Json.Decode as Decode
import Json.Decode exposing (int)


{-| Got from JS side, and Model to modify -}
type alias Flags = { now : Int }
type alias Model =
  { gameStarted : Bool
  , lastUpdate : Int
  , time : Int
  , sizeboard : Int
  , coloredSquare : Int
  , fruit : Int
  , score : Int
  }

init : Flags -> ( Model, Cmd Msg )
init { now } =
  now
  |> \time -> Model False time time 40 0 -1 0
  |> Update.none

{-| All your messages should go there -}
type Key = ArrowUp | ArrowRight | ArrowDown | ArrowLeft | Space
type Msg
  = NextFrame Posix
  | ToggleGameLoop
  | KeyDown Key
  | RandomFruit Int

{-| Manage all your updates here, from the main update function to each
 -|   subfunction. You can use the helpers in Update.elm to help construct
 -|   Cmds. -}
updateSquare : Model -> Model
updateSquare ({ coloredSquare } as model) =
  coloredSquare + 1
  |> modBy model.sizeboard
  |> Setters.setColoredSquareIn model


updateFruit : Model -> (Model, Cmd Msg)
updateFruit ({fruit} as model) = 
      (model, generate RandomFruit (Random.int 0 1559))

randomFruit : Int -> Model ->( Model, Cmd Msg)
randomFruit position model = 
            ({model
            | fruit = position
            }, Cmd.none)
toggleGameLoop : Model -> ( Model, Cmd Msg )
toggleGameLoop ({ gameStarted } as model) =
  if  gameStarted then 
    not gameStarted
    |> Setters.setGameStartedIn model
    |> Update.none
  else 
    not gameStarted
    |> Setters.setGameStartedIn model
    |> updateFruit
keyDown : Key -> Model -> ( Model, Cmd Msg )
keyDown key model =
  case Debug.log "key" key of
    Space -> update ToggleGameLoop model
    _ -> Update.none model

nextFrame : Posix -> Model -> ( Model, Cmd Msg )
nextFrame time model =
  let time_ = Time.posixToMillis time in
  if time_ - model.lastUpdate >= 1000 then
    updateSquare model
    |> Setters.setTime time_
    |> Setters.setLastUpdate time_
    |> mangerFruit
  else
    time_
    |> Setters.setTimeIn model
    |> Update.none

{-| Main update function, mainly used as a router for subfunctions -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    ToggleGameLoop -> toggleGameLoop model
    KeyDown key -> keyDown key model
    NextFrame time -> nextFrame time model
    RandomFruit fruit -> randomFruit fruit model 

{-| Manage all your view functions here. -}
cell :  Int -> Int -> Int -> Html msg
cell index active fruit =
  let class = if active == index then "cell active" else "cell" 
      classfruit = if fruit == index then (class ++ " fruit") else class
  in
  Html.div [ Attributes.class classfruit ] []

movingSquare : Model -> Html msg
movingSquare model =
  Html.div [ Attributes.class "grid" ]
    (listeCellule model) 


listeCellule : Model -> List(Html msg)
listeCellule model =
  listeCelluleHelp model 0 []

listeCelluleHelp : Model -> Int -> List(Html msg) -> List(Html msg)
listeCelluleHelp model acc acc2 =

  if(acc==model.sizeboard*model.sizeboard) then acc2
  else let cellule = cell acc model.coloredSquare model.fruit in
      listeCelluleHelp model (acc+1) (cellule::acc2)


mangerFruit: Model -> ( Model, Cmd Msg)
mangerFruit ({ coloredSquare, fruit , score } as model) =
      if fruit == coloredSquare then 
        score + 100
        |> Setters.setScoreIn model
        |> updateFruit
      else
        score
        |> Setters.setScoreIn model
        |> Update.none

actualTime : Model -> Html msg
actualTime { time } =
  Html.div [ Attributes.class "actual-time" ]
    [ Html.text "Actual time"
    , time
      |> String.fromInt
      |> Html.text
      |> List.singleton
      |> Html.code []
    ]

explanations : Model -> Html Msg
explanations ({ gameStarted } as model) =
  let word = if gameStarted then "Stop" else "Start" in
  Html.div [ Attributes.class "separator" ]
    [ Html.h1 []
      [ Html.text "Welcome to the snake project!" ]
    , actualTime model
    , Html.button
      [ Events.onClick ToggleGameLoop, Attributes.class "btn" ]
      [ Html.text (String.join " " [word, "game loop"]) ]
    ]

    

{-| Main view functions, composing all functions in one -}
view : Model -> Html Msg
view model =
  Html.main_ []
    [ Html.img [ Attributes.src "/logo.svg" ] []
    , explanations model
    , movingSquare model
    ]

{-| Parts for the runtime. Get key presses and subscribe to
 -|   requestAnimationFrame for the game loop. You don't have to bother with
 -|   this. -}
decodeArrow : String -> Decode.Decoder Key
decodeArrow value =
  case value of
    "ArrowUp" -> Decode.succeed ArrowUp
    "ArrowLeft" -> Decode.succeed ArrowLeft
    "ArrowRight" -> Decode.succeed ArrowRight
    "ArrowDown" -> Decode.succeed ArrowDown
    " " -> Decode.succeed Space
    _ -> Decode.fail "Not an arrow"

decodeKey : Decode.Decoder Msg
decodeKey =
  Decode.field "key" Decode.string
  |> Decode.andThen decodeArrow
  |> Decode.map KeyDown

subscriptions : Model -> Sub Msg
subscriptions { gameStarted } =
  let aF = Browser.Events.onAnimationFrame NextFrame
      base = Browser.Events.onKeyDown decodeKey :: [] in
    Sub.batch (if gameStarted then aF :: base else base)

{-| Entrypoint of your program -}
main : Program Flags Model Msg
main =
  Browser.element
    { view = view
    , init = init
    , update = update
    , subscriptions = subscriptions
    }
