module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required, resolve)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> ( Model, Cmd Msg )
init _ =
    let
        translations =
            [ Translation "UST" 2 "UST"
            , Translation "ETCBC+BHSA" 10 "ETCBC+BHSA"
            , Translation "NET" 5 "NET"
            ]

        model =
            NoResults translations Nothing
    in
    ( model, Cmd.none )



-- TYPES


type Model
    = NoResults (List Translation) (Maybe Http.Error)
    | GettingDocs (List Translation)
    | Loaded (List Translation) (List Text)


type alias Text =
    { parallelId : Int
    , moduleId : Int
    , rid : Int
    , text : String
    }


type alias Translation =
    { name : String
    , moduleId : Int
    , shortName : String
    }


type alias HebrewWord =
    { wid : Int
    , text : String
    , trailer : String
    }


type Msg
    = RequestNewDocs
    | GotText (Result Http.Error (List Text))


getText : List Translation -> Cmd Msg
getText translations =
    let
        modules =
            translations |> List.map (\trans -> trans.shortName) |> List.intersperse "%2C" |> List.foldl (++) ""

        reference =
            "Genesis+1"

        url =
            "https://dev.parabible.com/api/v2/text?modules=" ++ modules ++ "&reference=" ++ reference
    in
    Http.get
        { url = url
        , expect = Http.expectJson GotText matchTextDecoder
        }


hebrewModules =
    [ 10 ]



-- DECODERS


matchTextDecoder : JD.Decoder (List Text)
matchTextDecoder =
    -- basic decoder that matches root field and then calls another decoder on the value of that field
    JD.field "matchingText" (JD.list parallelDecoder)



-- parallelDecoder2 : JD.Decoder Text
-- parallelDecoder2 =
--     JD.succeed Text
--         |> required "parallelId" JD.int
--         |> required "moduleId" JD.int
--         |> required "rid" JD.int
--         |> required "text" (JD.oneOf [ hebrewTextDecoder, englishDecoder ]
-- This would work if the value of the hebrew text wasn't a string.
-- I haven't figured out a way to decode an additional string of JSON mid-decode
-- One possible solution would be to attempt to decode the hebrew after the initial decode,
-- but that seems against the spirit of Elm's json decoding
-- https://korban.net/posts/elm/2021-05-10-generating-json-decoders-with-json-decoders/
-- has an example of decoding from a string straight into the view, so that's what I ended up doing.
-- I removed all supporting code for this example, but kept it here as a reminder that Json.Decode.oneOf exists and would be useful.


parallelDecoder : JD.Decoder Text
parallelDecoder =
    -- this decoder uses the Pipeline model we could do this with the more basic one too
    JD.succeed Text
        |> required "parallelId" JD.int
        |> required "moduleId" JD.int
        |> required "rid" JD.int
        |> required "text" JD.string


hebrewTextDecoder : JD.Decoder (List HebrewWord)
hebrewTextDecoder =
    -- another example of using the list decoder and another decoder for the elements in the list
    JD.list hebrewDecoder


hebrewDecoder : JD.Decoder HebrewWord
hebrewDecoder =
    -- Here's an example of decoding a value that has 3 keys using one of the map functions
    JD.map3 HebrewWord
        (JD.field "wid" JD.int)
        (JD.field "text" JD.string)
        (JD.field "trailer" JD.string)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        translations =
            getTranslationsFromModel model
    in
    case msg of
        RequestNewDocs ->
            ( GettingDocs translations, getText translations )

        GotText (Ok value) ->
            ( Loaded translations value, Cmd.none )

        GotText (Err error) ->
            ( NoResults translations (Just error), Cmd.none )



-- HELPER FUNCTIONS


getTranslationsFromModel : Model -> List Translation
getTranslationsFromModel model =
    case model of
        NoResults t _ ->
            t

        GettingDocs t ->
            t

        Loaded t _ ->
            t


getTextByModuleId : Int -> List Text -> List Text
getTextByModuleId id texts =
    List.filter (\t -> t.moduleId == id) texts


getTranslationsNamesAsString : List Translation -> String
getTranslationsNamesAsString translations =
    case translations of
        x :: xs ->
            if List.length xs > 0 then
                x.name ++ ", " ++ getTranslationsNamesAsString xs

            else
                x.name

        [] ->
            ""



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        NoResults translations err ->
            viewNoResults translations err

        GettingDocs translations ->
            div [ style "margin" "1em" ]
                [ text ("Getting Results from" ++ getTranslationsNamesAsString translations) ]

        Loaded translations docs ->
            div []
                [ div [ style "margin" "1em", style "margin-bottom" "10px" ] [ text "Results - just dumping the text from each parallel, in columns based on how many translations are in the results." ]
                , div [ style "margin" "1em", style "margin-bottom" "10px" ] [ text (getTranslationsNamesAsString translations) ]
                , div [] (viewDisplayTexts translations docs)
                ]


viewNoResults : List Translation -> Maybe Http.Error -> Html Msg
viewNoResults translations error =
    let
        errorstring =
            case error of
                Nothing ->
                    ""

                Just err ->
                    case err of
                        Http.BadUrl str ->
                            " - Bad Url " ++ str

                        Http.Timeout ->
                            " - There was a timeout"

                        Http.NetworkError ->
                            " - There was a network error"

                        Http.BadStatus int ->
                            " - There was a bad status code: " ++ String.fromInt int

                        Http.BadBody body ->
                            " - There was a bad body response: " ++ body
    in
    div [ style "margin" "1em" ]
        [ div [] [ text ("No Results" ++ errorstring) ]
        , div [] (List.map (\trans -> span [] [ text (trans.name ++ " ") ]) translations)
        , button [ onClick RequestNewDocs ] [ text "Request Docs" ]
        ]


viewDisplayTexts : List Translation -> List Text -> List (Html Msg)
viewDisplayTexts translations texts =
    let
        numberoftranslations =
            List.length translations

        columnwidth =
            viewTextColumnWidthAttribute numberoftranslations
    in
    translations
        |> List.map
            (\trans ->
                getTextByModuleId trans.moduleId texts
            )
        |> List.map (viewText columnwidth)


viewTextColumnWidthAttribute : Int -> String
viewTextColumnWidthAttribute numberofcolumns =
    (100 // numberofcolumns)
        |> (\n -> n - 1)
        |> String.fromInt
        |> (\width -> width ++ "%")


viewText : String -> List Text -> Html msg
viewText columnwidth textsegment =
    let
        outerDiv =
            div [ style "width" columnwidth, style "display" "block", style "margin" "0.5%", style "float" "left" ]
    in
    textsegment
        |> List.map
            (\t ->
                if List.member t.moduleId hebrewModules then
                    viewHebrewText t

                else
                    span [] [ text t.text ]
            )
        |> outerDiv


viewHebrewText : Text -> Html msg
viewHebrewText t =
    case JD.decodeString hebrewTextDecoder t.text of
        Err e ->
            span [] [ text "Failed to load" ]
                |> Debug.log "Hebrew Decoder errored"

        Ok result ->
            span [] (viewHebrewWord result)


viewHebrewWord : List HebrewWord -> List (Html msg)
viewHebrewWord words =
    words
        |> List.map (\w -> span [] [ text (w.text ++ w.trailer) ])
