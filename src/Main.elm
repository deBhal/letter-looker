module Main exposing (..)

import Browser
import Html exposing (Html, button, div, h1, h2, input, node, span, text)
import Html.Attributes exposing (class, href, rel, value)
import Html.Events exposing (onClick, onInput)
import SongNames exposing (containsLetters, missingLetters, removeLetters, sanitize)



-- MODEL
-- It's convenient to keep the id from the preexisting TODO/Task code as it
-- avoids concerns with both duplication and case.


type alias SelectedPhrase =
    { id : Int, name : String }


type alias Model =
    { songs : List SelectedPhrase
    , nextPhraseId : Int
    , letters : String
    , phrases : List String
    }


init : Model
init =
    { nextPhraseId = 3
    , letters = ""
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
            List.filter (\phrase -> not <| containsLetters phrase remainingLetters) model.phrases

        missingLetters =
            removeLetters usedLetters model.letters
    in
    div [ class "MainContainer" ]
        [ css "style.css"
        , h1 [] [ text "Find Phrases for Bracelets" ]
        , div [ class "input-container" ]
            [ input [ class "letters-input", value model.letters, onInput SaveAvailableLetters ] []
            ]
        , div [] [ text "Insert the letters you have above." ]
        , div [] [ text "Click on (un)available phrases to add them to selected songs or click on selected songs to unselect them." ]
        , div [] [ text "Click on selected songs to unselect them." ]
        , div [] [ text "Missing letters used in selected songs are highlighted in red." ]
        , h2 [] [ text "Remaining letters:" ]
        , div [ class "remaining-letters" ] [ text remainingLetters ]
        , h2 [] [ text "Selected Songs:" ]
        , div [ class "songs" ]
            (List.map
                {- passing in the missing letters here means that a single
                   missing letter is highlighted in every word it's used in
                   (i.e. if you're only one 'e' letter short, every instance
                   will still be red
                -}
                (\song ->
                    viewRemoveButton missingLetters song
                )
                model.songs
            )
        , h2 [] [ text "Available Songs:" ]
        , div [ class "songs" ] (List.map (viewAddButton remainingLetters) matchedPhrases)
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
        , div [ class "songs" ] (List.map (viewAddButton remainingLetters) unmatchedPhrases)
        , div [ class "footer" ] [ text "o/" ]
        ]



{-
   Show a button that removes the specified song

   Missing letters are hightlighted in red.
-}


viewRemoveButton : String -> SelectedPhrase -> Html Msg
viewRemoveButton missingLetters song =
    let
        phrase =
            song.name
    in
    div [ class "phrase remove" ]
        [ button
            [ onClick (RemoveSong song)
            ]
            (viewPhraseWithMissingLetters phrase missingLetters)
        ]


viewAddButton : String -> String -> Html Msg
viewAddButton letters phrase =
    let
        available =
            containsLetters (sanitize phrase) letters

        -- potential optimization: skip if no missing letters
        missingLetters =
            removeLetters phrase letters
    in
    div [ class "phrase add" ]
        [ button
            [ class
                (if available then
                    "available"

                 else
                    "unavailable"
                )
            , onClick (AddSong phrase)
            ]
            (viewPhraseWithMissingLetters phrase missingLetters)
        ]


viewAddPhraseButton : String -> String -> Html Msg
viewAddPhraseButton letters phrase =
    let
        available =
            containsLetters (sanitize phrase) letters

        -- potential optimization: skip if no missing letters
        missingLetters =
            removeLetters phrase letters
    in
    div [ class "phrase" ]
        [ button
            [ class
                (if available then
                    "available"

                 else
                    "unavailable"
                )
            , onClick (AddSong phrase)
            ]
            -- potential optimization: skip if no missing letters
            (viewPhraseWithMissingLetters phrase missingLetters)
        ]


viewPhrase : String -> String -> Html Msg
viewPhrase letters phrase =
    let
        available =
            containsLetters (sanitize phrase) letters

        -- potential optimization: skip if no missing letters
        missingLetters =
            removeLetters phrase letters
    in
    div [ class "phrase" ]
        (viewPhraseWithMissingLetters phrase missingLetters)



{- Wrap missing letters in span elements to target them for styling.
   There must be a better way to do it, but this implementation breaks
   down every character into it's own text element
-}


viewPhraseWithMissingLetters :
    String
    -> String
    -> List (Html Msg)
viewPhraseWithMissingLetters phrase missingLetters =
    case String.uncons phrase of
        Nothing ->
            []

        Just ( c, rest ) ->
            let
                currentLetter =
                    String.fromChar c

                missing =
                    sanitize missingLetters
            in
            if String.contains (sanitize currentLetter) (sanitize missingLetters) && missing /= "" then
                span [ class "red" ] [ text currentLetter ] :: viewPhraseWithMissingLetters rest (removeLetters missingLetters currentLetter)

            else
                text currentLetter :: viewPhraseWithMissingLetters rest missingLetters



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }
