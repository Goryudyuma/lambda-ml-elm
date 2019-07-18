module Main exposing (Binding(..), Context, Info(..), Model, Msg(..), Term(..), init, main, printtm, update, view)

import Browser
import Html exposing (Html, div, h1, img, option, text)
import Html.Attributes exposing (src)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


type Info
    = NoData


type Term
    = TmVar Info Int Int
    | TmAbs Info String Term
    | TmApp Info Term Term


type alias Context =
    List ( String, Binding )


type Binding
    = NameBind


printtm : Context -> Term -> String
printtm ctx t =
    let
        ctxlength =
            List.length

        index2name : Info -> Context -> Int -> String
        index2name fi ctx2 x =
            List.drop (x - 1) ctx2
                |> List.head
                |> Maybe.map (\( a, _ ) -> a)
                |> Maybe.withDefault ""

        pickFreshName : Context -> String -> ( Context, String )
        pickFreshName ctx2 x =
            let
                x2 =
                    x ++ "1"
            in
            if List.any (\( s, _ ) -> s == x2) ctx2 then
                pickFreshName ctx2 x2

            else
                ( ( x2, NameBind ) :: ctx, x2 )
    in
    case t of
        TmAbs fi x t1 ->
            let
                ( ctx2, x2 ) =
                    pickFreshName ctx x
            in
            "(lambda" ++ x2 ++ "." ++ printtm ctx2 t1 ++ ")"

        TmApp fi t1 t2 ->
            "(" ++ printtm ctx t1 ++ " " ++ printtm ctx t2 ++ ")"

        TmVar fi x n ->
            if ctxlength ctx == n then
                index2name fi ctx x

            else
                "[bad index]"
