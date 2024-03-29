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
    { notes : List Note, selectedNoteId : Int, searchNoteText : String }


init : ( Model, Cmd Msg )
init =
    ( { notes =
            [ { id = 1, body = "First note...", timestamp = 0 }
            , { id = 2, body = "Second note...", timestamp = 0 }
            , { id = 3, body = "Third note...", timestamp = 0 }
            ]
      , selectedNoteId = 1
      , searchNoteText = ""
      }
    , Time.now |> Task.perform InitializeTimestamps
    )



-- UPDATE


type Msg
    = InitializeTimestamps Time.Time
    | SelectNote Int
    | InputNoteBody String
    | UpdateNoteTimeStamp String Time.Time
    | ClickNew
    | NewNote Time.Time
    | ClickDelete
    | InputSearch String


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

        InputNoteBody newText ->
            ( model, Time.now |> Task.perform (UpdateNoteTimeStamp newText) )

        UpdateNoteTimeStamp newText newTimestamp ->
            case getSelectedNote model of
                Nothing ->
                    ( model, Cmd.none )

                Just selectedNote ->
                    let
                        updateSelectedNote note =
                            if note.id == model.selectedNoteId then
                                { note | body = newText, timestamp = newTimestamp }
                            else
                                note
                    in
                    ( { model | notes = List.map updateSelectedNote model.notes }
                    , Cmd.none
                    )

        ClickNew ->
            ( model, Time.now |> Task.perform NewNote )

        NewNote newTimestamp ->
            let
                newId =
                    round (Time.inMilliseconds newTimestamp)
            in
            ( { model
                | notes = [ { id = newId, body = "", timestamp = newTimestamp } ] ++ model.notes
                , selectedNoteId = newId
              }
            , Cmd.none
            )

        ClickDelete ->
            let
                newNotes =
                    List.filter (\note -> note.id /= model.selectedNoteId) model.notes

                firstVisibleNote =
                    newNotes |> transformNotes model.searchNoteText |> List.head
            in
            case firstVisibleNote of
                Nothing ->
                    ( { model | notes = newNotes }, Cmd.none )

                Just availableNote ->
                    ( { model | notes = newNotes, selectedNoteId = availableNote.id }, Cmd.none )

        InputSearch searchNoteText ->
            let
                firstVisibleNote =
                    model.notes |> transformNotes searchNoteText |> List.head
            in
            case firstVisibleNote of
                Nothing ->
                    ( { model | searchNoteText = searchNoteText, selectedNoteId = -1 }, Cmd.none )

                Just availableNote ->
                    ( { model | searchNoteText = searchNoteText, selectedNoteId = availableNote.id }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ div [ class "toolbar" ]
            [ button [ class "toolbar-button", onClick ClickNew ] [ text "New" ]
            , button [ class "toolbar-button", onClick ClickDelete ] [ text "Delete" ]
            , input [ class "toolbar-search", type_ "text", placeholder "Search...", onInput InputSearch ] []
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
            |> transformNotes model.searchNoteText
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
    case getSelectedNote model of
        Nothing ->
            div [ class "note-editor" ] []

        Just selectedNote ->
            div [ class "note-editor" ]
                [ p [ class "note-editor-info" ] [ selectedNote.timestamp |> formatTimestamp |> text ]
                , textarea [ class "note-editor-input", onInput InputNoteBody, value selectedNote.body ] []
                ]


transformNotes : String -> List Note -> List Note
transformNotes searchNoteText notes =
    notes
        |> List.filter (\note -> String.contains (String.toLower searchNoteText) (String.toLower note.body))
        |> List.sortBy .timestamp
        |> List.reverse


getSelectedNote : Model -> Maybe Note
getSelectedNote model =
    model.notes
        |> transformNotes model.searchNoteText
        |> List.filter (\note -> note.id == model.selectedNoteId)
        |> List.head


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
