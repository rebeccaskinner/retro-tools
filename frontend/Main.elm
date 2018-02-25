module Main exposing (..)

{-| documentation -}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import List.Extra as L
import Debug exposing (log)
import Mouse exposing (Position)

type Msg = AddNote
    | DragStart Int Position
    | DragAt Position
    | DragStop Position

type alias DragNote =
    { start : Position
    , current : Position
    }

type alias Model =
    { notes : List Notecard
    , noteIndex : Int
    , drag : Maybe DragNote
    , selected : Maybe Int
    }

type alias Notecard =
    { position : Position
    , contents : String
    }

type IO = IO

subscriptions model =
    case model.selected of
        Nothing -> Sub.none
        Just idx ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragStop ]
emptyModel : Model
emptyModel = Model [] 0 Nothing Nothing

init : ( Model, Cmd Msg )
init = ( emptyModel, Cmd.none )
-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( updateHelp msg model, Cmd.none )

updateHelp : Msg -> Model -> Model
updateHelp msg ({ notes, noteIndex, drag, selected } as model) =
    case msg of
        AddNote ->
            let
                idx_ = noteIndex + 1
                pos = Position (100 * idx_) (100 * idx_)
                note = Notecard pos (toString idx_)
            in Model (note :: notes) idx_ drag selected
        DragStart idx pos -> { model | selected = Just idx}
        DragAt pos -> model
        DragStop pos -> { model | selected = Nothing }

-- VIEW
(=>) = (,)
view : Model -> Html Msg
view ({ notes, noteIndex } as model) =
    let render_ = \idx c -> renderNotecard c [mvNotecardAttr idx]
        boxes = div [] (List.indexedMap render_ notes)
        _ = debugModel model
    in div [] [ boxes, addBtn]

logIO : String -> a -> b -> b
logIO s a b = Debug.log ((++) s <| toString a) b

isJust : Maybe a -> Bool
isJust m =
    case m of
        Nothing -> False
        _ -> True

debugModel : Model -> IO
debugModel ({notes, noteIndex, drag, selected}) =
    logIO "note count: " (List.length notes) IO |>
    logIO "note index: " noteIndex |>
    logIO "dragging: " (isJust drag) |>
    logIO "selected: " selected

mvNotecardAttr : Int -> Attribute Msg
mvNotecardAttr idx =
     on "mousedown" (Decode.map (DragStart idx) Mouse.position)

renderNotecard : Notecard -> List (Attribute Msg) -> Html Msg
renderNotecard ({ position, contents } as nc) attrs =
    let x = px position.x
        y = px position.y
        css = ("left" => x) :: ("top" => y) :: defaultNotecardCSS
    in div (style css :: attrs) [ text contents ]

defaultNotecardCSS =
    [ "background-color" => "#fff051"
    , "cursor" => "move"
    , "width" => "100px"
    , "height" => "100px"
    , "border-radius" => "4px"
    , "position" => "absolute"
    , "color" => "white"
    , "display" => "flex"
    , "align-items" => "center"
    , "justify-content" => "center"
    ]

addBtn : Html Msg
addBtn =
    let circleSize = "50px"
        css = style
                [ "background-color" => "#dff2ff"
                , "width" => circleSize
                , "height" => circleSize
                , "line-height" => circleSize
                , "border-radius" => "50%"
                , "position" => "absolute"
                , "left" => "50px"
                , "top" => "50px"
                , "display" => "flex"
                , "justify-content" => "center"
                ]
        t = text "+"
    in div [ onClick AddNote, css ] [ t ]

px : Int -> String
px number = toString number ++ "px"

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }