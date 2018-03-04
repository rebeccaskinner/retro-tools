module Main exposing (..)

{-| documentation -}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Mouse exposing (Position)
import List.Extra exposing (updateAt)

type Msg = AddNote
    | DragStart Int Position
    | DragAt Position
    | DragStop Position
--    | ChangeText Int

type alias Model =
    { notes : List Notecard -- todo; use a zipper to speed up dragging
    , noteIndex : Int
    , drag : Maybe DragState
    }

type alias DragState =
    { start : Position
    , current : Position
    , selected : Int
    }

type alias Notecard =
    { position : Position
    , contents : String
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing -> Sub.none
        Just drag ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragStop ]
emptyModel : Model
emptyModel = Model [] 0 Nothing

init : ( Model, Cmd Msg )
init = ( emptyModel, Cmd.none )

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( updateHelp msg model, Cmd.none )

updateHelp : Msg -> Model -> Model
updateHelp msg ({ notes, noteIndex, drag } as model) =
    case msg of
        AddNote ->
            let idx_ = noteIndex + 1
                pos = Position (100 * idx_) (100 * idx_)
                note = Notecard pos (toString idx_)
            in Model (note :: notes) idx_ Nothing
        DragStart idx pos ->
            let drag_ = DragState pos pos idx
            in {model | drag = Just drag_}
        DragAt pos ->
            let drag_ = Maybe.map (\d -> {d | current = pos}) drag
            in {model | drag = drag_}
        DragStop _ ->
            let notes_ = updateNoteList_ notes drag
            in {model | notes = notes_, drag = Nothing}

-- VIEW
(=>) : a -> a -> (a,a)
(=>) = (,)

view : Model -> Html Msg
view ({ notes, noteIndex, drag } as model) =
    let notes_ = updateNoteList_ notes drag
        rendered =  List.indexedMap renderNotecard_ notes_
    in div [] (addBtn :: rendered)

updateNoteList_ : List Notecard -> Maybe DragState -> List Notecard
updateNoteList_ nc d =
    case d of
        Nothing -> nc
        Just d -> updateNoteList nc d

updateNoteList : List Notecard -> DragState -> List Notecard
updateNoteList notes drag =
    updateAt drag.selected (\n -> dragNotecard n drag) notes

dragNotecard : Notecard -> DragState -> Notecard
dragNotecard  nc offset =
    {nc | position = updatePosition nc.position offset}

mvNotecardAttr : Int -> Attribute Msg
mvNotecardAttr idx =
     on "mousedown" (Decode.map (DragStart idx) Mouse.position)

renderNotecard_ : Int -> Notecard -> Html Msg
renderNotecard_ idx nc =
    renderNotecard nc [mvNotecardAttr idx]

renderNotecard : Notecard -> List (Attribute Msg) -> Html Msg
renderNotecard nc attrs =
    let x = px nc.position.x
        y = px nc.position.y
        css = ("left" => x) :: ("top" => y) :: defaultNotecardCSS
    in div (style css :: attrs) [ text nc.contents ]

dragOffset : DragState -> Position
dragOffset drag =
    let x = drag.current.x - drag.start.x
        y = drag.current.y - drag.start.y
    in Position x y

updatePosition : Position -> DragState -> Position
updatePosition ({x,y}) drag =
    let off = dragOffset drag in
    Position (x + off.x) (y + off.y)

defaultNotecardCSS : List (String, String)
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
