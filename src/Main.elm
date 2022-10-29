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


type Model
    = NoResults (List Translation) (Maybe Http.Error)
    | GettingDocs (List Translation)
    | Loaded (List Translation) (List Text)


type alias Text =
    { parallelId : Int
    , moduleId : Int
    , rid : Int
    , text : TextActual
    }


type TextActual
    = EnglishText String
    | HebrewText (List HebrewWord)


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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        translations =
            --     [ Translation "ETCBC+BHSA" 10 "ETCBC+BHSA"
            --     ]
            [ Translation "UST" 2 "UST"
            , Translation "ETCBC+BHSA" 10 "ETCBC+BHSA"
            , Translation "NET" 5 "NET"
            ]
    in
    ( NoResults
        translations
        Nothing
    , Cmd.none
    )


type Msg
    = RequestNewDocs
    | GotText (Result Http.Error (List Text))


getText : List Translation -> Cmd Msg
getText translations =
    let
        url1 =
            "https://dev.parabible.com/api/v2/text?modules=NET&reference=Genesis+1"

        url2 =
            "https://dev.parabible.com/api/v2/text?modules=ETCBC+BHSA%2CNET%2CNestle1904&reference=Genesis+1"

        modules =
            translations |> List.map (\trans -> trans.shortName) |> List.intersperse "%2C" |> List.foldl (++) ""

        reference =
            "Genesis+1"

        url3 =
            "https://dev.parabible.com/api/v2/text?modules=" ++ modules ++ "&reference=" ++ reference
    in
    Http.get
        { url = url3
        , expect = Http.expectJson GotText matchTextDecoder
        }


matchTextDecoder : JD.Decoder (List Text)
matchTextDecoder =
    JD.field "matchingText" (JD.list parallelDecoder2)



-- parallelDecoder : JD.Decoder Text
-- parallelDecoder =
--     JD.map4 Text
--         (JD.field "parallelId" JD.int)
--         (JD.field "moduleId" JD.int)
--         (JD.field "rid" JD.int)
--         (JD.field "text" JD.string)


parallelDecoder2 : JD.Decoder Text
parallelDecoder2 =
    JD.succeed Text
        |> required "parallelId" JD.int
        |> required "moduleId" JD.int
        |> required "rid" JD.int
        -- This would work if the value of the hebrew text wasn't a string.
        -- I haven't figured out a way to decode an additional string mid-decode
        -- One possible solution would be to attempt to decode the hebrew after the initial decode,
        -- but that seems against the spirit of Elm's json decoding
        |> required "text" (JD.oneOf [ hebrewTextDecoder, englishDecoder ])


englishDecoder : JD.Decoder TextActual
englishDecoder =
    JD.map EnglishText
        JD.string


hebrewTextDecoder : JD.Decoder TextActual
hebrewTextDecoder =
    JD.map HebrewText
        (JD.list hebrewDecoder)


hebrewDecoder : JD.Decoder HebrewWord
hebrewDecoder =
    JD.map3 HebrewWord
        (JD.field "wid" JD.int)
        (JD.field "text" JD.string)
        (JD.field "trailer" JD.string)


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
            let
                r =
                    Debug.log "results" value
            in
            ( Loaded translations value, Cmd.none )

        GotText (Err error) ->
            ( NoResults translations (Just error), Cmd.none )


getTranslationsFromModel : Model -> List Translation
getTranslationsFromModel model =
    case model of
        NoResults t _ ->
            t

        GettingDocs t ->
            t

        Loaded t _ ->
            t


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        NoResults translations err ->
            let
                errorstring =
                    case err of
                        Nothing ->
                            ""

                        Just error ->
                            case error of
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
            div []
                [ div [] [ text ("No Results" ++ errorstring) ]
                , div [] (List.map (\trans -> span [] [ text (trans.name ++ " ") ]) translations)
                , button [ onClick RequestNewDocs ] [ text "Request Docs" ]
                ]

        GettingDocs translations ->
            div []
                [ text ("Getting Results from" ++ getTranslationsNamesAsString translations) ]

        Loaded translations docs ->
            div []
                [ div [ style "margin-bottom" "10px" ] [ text "Results - just dumping the text from each parallel, in columns based on how many translations are in the results." ]
                , div [ style "margin-bottom" "10px" ] [ text (getTranslationsNamesAsString translations) ]

                -- , div [] (List.map (\d -> div [] [ text (d.text ++ String.fromInt d.rid) ]) docs)
                , div [] (viewDisplayTexts translations docs)
                ]


viewDisplayTexts : List Translation -> List Text -> List (Html Msg)
viewDisplayTexts translations texts =
    let
        w =
            100
                // List.length translations
                |> (\n -> n - 1)
                |> String.fromInt
                |> Debug.log "division for width"
                |> (\width -> width ++ "%")
                |> Debug.log "string percent for width"
    in
    translations
        |> List.map
            (\trans ->
                getTextByModuleId trans.moduleId texts
            )
        |> List.map
            (\trans ->
                div [ style "width" w, style "display" "block", style "float" "left" ] (List.map (\t -> span [] [ text (getTextActual t.text) ]) trans)
            )


getTextActual : TextActual -> String
getTextActual textactual =
    case textactual of
        HebrewText hw ->
            "list of hebrew words"

        EnglishText s ->
            s


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
