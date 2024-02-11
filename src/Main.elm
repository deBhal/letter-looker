module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, input, node, text)
import Html.Attributes exposing (class, href, rel, value)
import Html.Events exposing (onClick, onInput)
import SongNames exposing (containsLetters, missingLetters, removeLetters, sanitize)



-- MODEL
-- It's convenient to keep the id from the preexisting TODO/Task code as it
-- avoids concerns with both duplication and case.


type alias SelectedPhrase =
    { id : Int, name : String }


type alias Model =
    { songs : List SelectedPhrase, nextPhraseId : Int, letters : String, missingLetters : String, phrases : List String }


init : Model
init =
    { nextPhraseId = 3
    , letters = "123lgb"
    , missingLetters = ""
    , songs = []
    , phrases = SongNames.songs
    }



-- UPDATE


type Msg
    = SaveAvailableLetters String
    | AddSong String
    | RemoveSong SelectedPhrase


doesNotMatch : SelectedPhrase -> SelectedPhrase -> Bool
doesNotMatch phraseToDelete phrase =
    not (phraseToDelete.id == phrase.id)


update : Msg -> Model -> Model
update msg model =
    case msg of
        SaveAvailableLetters newPhrase ->
            { model | letters = newPhrase }

        AddSong song ->
            { model
                | songs =
                    { id = model.nextPhraseId, name = song } :: model.songs
                , nextPhraseId = model.nextPhraseId + 1
            }

        RemoveSong phraseToDelete ->
            let
                missingLetters =
                    removeLetters phraseToDelete.name model.letters

                freedUpLetters =
                    removeLetters phraseToDelete.name missingLetters
            in
            { model
                | songs = List.filter (doesNotMatch phraseToDelete) model.songs
            }



-- VIEW


css : String -> Html never
css path =
    node "link" [ rel "stylesheet", href path ] []


view : Model -> Html Msg
view model =
    let
        usedLetters =
            List.foldl (++) "" <| List.map (\song -> sanitize song.name) model.songs

        remainingLetters =
            removeLetters model.letters usedLetters

        matchedPhrases =
            List.filter (\phrase -> containsLetters phrase remainingLetters) model.phrases

        unmatchedPhrases =
            List.filter (\phrase -> not <| containsLetters phrase model.letters) model.phrases

        missingLetters =
            removeLetters usedLetters model.letters
    in
    div [ class "MainContainer" ]
        [ css "style.css"
        , h1 [] [ text "Find Phrases for Bracelets" ]
        , h2 [] [ text "Insert the letters you have here:" ]
        , div [ class "input-container" ]
            [ input [ class "letters-input", value model.letters, onInput SaveAvailableLetters ] []
            ]
        , h2 [] [ text "Remaining letters:" ]
        , div [ class "remaining-letters" ] [ text remainingLetters ]
        , h2 [] [ text "Selected Songs:" ]
        , div [ class "songs" ]
            (List.map
                (\song ->
                    div [ class "phrase" ]
                        [ button [ onClick (RemoveSong song) ] [ text song.name ]
                        ]
                )
                model.songs
            )
        , h2 [] [ text "Available Songs:" ]
        , div [ class "songs" ] (List.map (viewPhrase remainingLetters) matchedPhrases)
        , h2 [] [ text "Used letters:" ]
        , div [ class "remaining-letters" ] [ text usedLetters ]
        , div
            [ class <|
                if missingLetters == "" then
                    "hidden"

                else
                    "missing-letters-container"
            ]
            [ div
                []
                [ h2 [] [ text "Missing Letters:" ]
                , text missingLetters
                ]
            ]
        , h2 [] [ text "Unavailable Songs:" ]
        , div [ class "songs" ] (List.map (viewPhrase remainingLetters) unmatchedPhrases)
        , div [ class "footer" ] [ text "o/" ]
        ]


viewPhrase : String -> String -> Html Msg
viewPhrase letters phrase =
    let
        available =
            containsLetters (sanitize phrase) letters
    in
    div [ class "phrases" ]
        [ button
            [ class
                (if available then
                    "available"

                 else
                    "unavailable"
                )
            , onClick (AddSong phrase)
            ]
            [ text
                (phrase
                    ++ (if available then
                            ""

                        else
                            let
                                missingLetters =
                                    removeLetters phrase letters
                            in
                            " | " ++ missingLetters
                       )
                )
            ]
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
