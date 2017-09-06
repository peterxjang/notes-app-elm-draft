module Main exposing (..)

import Date
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task
import Time


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Note =
    { id : Int, body : String, timestamp : Float }


type alias Model =
    { notes : List Note, selectedNoteId : Int }


init : ( Model, Cmd Msg )
init =
    ( { notes =
            [ { id = 1, body = "First note...", timestamp = 0 }
            , { id = 2, body = "Second note...", timestamp = 0 }
            , { id = 3, body = "Third note...", timestamp = 0 }
            ]
      , selectedNoteId = 1
      }
    , Time.now |> Task.perform InitializeTimestamps
    )



-- UPDATE


type Msg
    = InitializeTimestamps Time.Time
    | SelectNote Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitializeTimestamps time ->
            ( { model
                | notes = model.notes |> List.map (\note -> { note | timestamp = time })
              }
            , Cmd.none
            )

        SelectNote id ->
            ( { model | selectedNoteId = id }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ div [ class "toolbar" ]
            [ button [ class "toolbar-button" ] [ text "New" ]
            , button [ class "toolbar-button" ] [ text "Delete" ]
            , input [ class "toolbar-search", type_ "text", placeholder "Search..." ] []
            ]
        , div [ class "note-container" ]
            [ viewNoteSelectors model
            , viewNoteEditor model
            ]
        ]


viewNoteSelectors : Model -> Html Msg
viewNoteSelectors model =
    div [ class "note-selectors" ]
        (model.notes
            |> List.sortBy .timestamp
            |> List.reverse
            |> List.map (\note -> viewNoteSelector note model.selectedNoteId)
        )


viewNoteSelector : Note -> Int -> Html Msg
viewNoteSelector note selectedNoteId =
    div [ classList [ ( "note-selector", True ), ( "active", note.id == selectedNoteId ) ], onClick (SelectNote note.id) ]
        [ p [ class "note-selector-title" ] [ note.body |> formatTitle |> text ]
        , p [ class "note-selector-timestamp" ] [ note.timestamp |> formatTimestamp |> text ]
        ]


viewNoteEditor : Model -> Html Msg
viewNoteEditor model =
    case model.notes |> List.filter (\note -> note.id == model.selectedNoteId) |> List.head of
        Nothing ->
            div [ class "note-editor" ] []

        Just selectedNote ->
            div [ class "note-editor" ]
                [ p [ class "note-editor-info" ] [ selectedNote.timestamp |> formatTimestamp |> text ]
                , textarea [ class "note-editor-input" ] [ selectedNote.body |> formatTitle |> text ]
                ]


formatTimestamp : Float -> String
formatTimestamp timestamp =
    timestamp |> Date.fromTime |> toString


formatTitle : String -> String
formatTitle body =
    let
        maxLength =
            20

        length =
            String.length body
    in
    if length > maxLength then
        String.left (maxLength - 3) body ++ "..."
    else if length == 0 then
        "New note"
    else
        body
