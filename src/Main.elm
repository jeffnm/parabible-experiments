module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick)
import Http
import Json.Decode as JD


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = NoResults (Maybe Http.Error)
    | GettingDocs
    | Loaded (List Text)


type alias Docs =
    { text : String }


type alias Text =
    { parallelId : Int
    , moduleId : Int
    , rid : Int
    , text : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( NoResults Nothing, Cmd.none )


type Msg
    = RequestNewDocs
    | GotText (Result Http.Error (List Text))


getText : Cmd Msg
getText =
    let
        url1 =
            ""

        url2 =
            ""
    in
    Http.get
        { url = url2
        , expect = Http.expectJson GotText matchTextDecoder
        }


matchTextDecoder =
    JD.field "matchingText" (JD.list parallelDecoder)


parallelDecoder =
    JD.map4 Text
        (JD.field "parallelId" JD.int)
        (JD.field "moduleId" JD.int)
        (JD.field "rid" JD.int)
        (JD.field "text" JD.string)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RequestNewDocs ->
            ( GettingDocs, getText )

        GotText (Ok value) ->
            ( Loaded value, Cmd.none )

        GotText (Err error) ->
            ( NoResults (Just error), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    case model of
        NoResults err ->
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
                , button [ onClick RequestNewDocs ] [ text "Request Docs" ]
                ]

        GettingDocs ->
            div []
                [ text "Getting Results..." ]

        Loaded docs ->
            div []
                [ div [] [ text "Results - just dumping the stringified json here for now" ]
                , div [] (List.map (\d -> div [] [ text (d.text ++ String.fromInt d.rid) ]) docs)
                ]
