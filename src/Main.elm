port module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text, button)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput, onClick)

-- MODEL

type alias Model =
    { red : Int
    , green : Int
    , blue : Int
    , hue : Int
    , saturation : Int
    , value : Int
    }

init : Model
init =
    { red = 128
    , green = 128
    , blue = 128
    , hue = 0
    , saturation = 0
    , value = 50
    }

-- UPDATE

type Msg
    = UpdateRed String
    | UpdateGreen String
    | UpdateBlue String
    | UpdateHue String
    | UpdateSaturation String
    | UpdateValue String
    | CopyToClipboard String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateRed value ->
            let
                newRed = String.toInt value |> Maybe.withDefault 0
                (h, s, v) = rgbToHsv newRed model.green model.blue
            in
            ( { model | red = newRed, hue = h, saturation = round s, value = v }, Cmd.none )

        UpdateGreen value ->
            let
                newGreen = String.toInt value |> Maybe.withDefault 0
                (h, s, v) = rgbToHsv model.red newGreen model.blue
            in
            ( { model | green = newGreen, hue = h, saturation = round s, value = v }, Cmd.none )

        UpdateBlue value ->
            let
                newBlue = String.toInt value |> Maybe.withDefault 0
                (h, s, v) = rgbToHsv model.red model.green newBlue
            in
            ( { model | blue = newBlue, hue = h, saturation = round s, value = v }, Cmd.none )

        UpdateHue value ->
            let
                newHue = String.toInt value |> Maybe.withDefault 0
                (r, g, b) = hsvToRgb (toFloat newHue) (toFloat model.saturation / 100) (toFloat model.value / 100)
            in
            ( { model | hue = newHue, red = r, green = g, blue = b }, Cmd.none )

        UpdateSaturation value ->
            let
                newSaturation = String.toInt value |> Maybe.withDefault 0
                (r, g, b) = hsvToRgb (toFloat model.hue) (toFloat newSaturation / 100) (toFloat model.value / 100)
            in
            ( { model | saturation = newSaturation, red = r, green = g, blue = b }, Cmd.none )

        UpdateValue value ->
            let
                newValue = String.toInt value |> Maybe.withDefault 0
                (r, g, b) = hsvToRgb (toFloat model.hue) (toFloat model.saturation / 100) (toFloat newValue / 100)
            in
            ( { model | value = newValue, red = r, green = g, blue = b }, Cmd.none )

        CopyToClipboard value ->
            ( model, copyToClipboard value )

-- VIEW

view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "height" "100vh"
        , style "background-color" "#f0f0f0"
        ]
        [ div
            [ style "background-color" "white"
            , style "border-radius" "8px"
            , style "box-shadow" "0 4px 6px rgba(0, 0, 0, 0.1)"
            , style "padding" "20px"
            , style "display" "flex"
            , style "flex-direction" "column"
            , style "align-items" "center"
            , style "width" "340px"
            ]
            [ colorSlider "Red" (String.fromInt model.red) "0" "255" "1" UpdateRed
            , colorSlider "Green" (String.fromInt model.green) "0" "255" "1" UpdateGreen
            , colorSlider "Blue" (String.fromInt model.blue) "0" "255" "1" UpdateBlue
            , colorSlider "Hue" (String.fromInt model.hue) "0" "360" "1" UpdateHue
            , colorSlider "Saturation" (String.fromInt model.saturation) "0" "100" "1" UpdateSaturation
            , colorSlider "Value" (String.fromInt model.value) "0" "100" "1" UpdateValue
            , div
                [ style "width" "100%"
                , style "height" "100px"
                , style "background-color" (rgbToHex model)
                , style "margin-top" "20px"
                , style "border" "1px solid #ccc"
                ]
                []
            , colorInfoBox "RGB (int)" (rgbToIntString model)
            , colorInfoBox "RGB (hex)" (rgbToHex model)
            , colorInfoBox "HSV (int)" (hsvToIntString model)
            , colorInfoBox "HSV (hex)" (hsvToHexString model)
            ]
        ]

colorSlider : String -> String -> String -> String -> String -> (String -> Msg) -> Html Msg
colorSlider label value min max step msg =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "margin-bottom" "10px"
        , style "width" "300px"
        ]
        [ div
            [ style "flex" "1"
            , style "text-align" "left"
            ]
            [ text label ]
        , div
            [ style "flex" "1"
            , style "text-align" "right"
            , style "margin-right" "10px"
            ]
            [ text value ]
        , div
            [ style "flex" "2"
            ]
            [ input
                [ type_ "range"
                , Html.Attributes.min min
                , Html.Attributes.max max
                , Html.Attributes.step step
                , Html.Attributes.value value
                , onInput msg
                , style "width" "100%"
                ]
                []
            ]
        ]

colorInfoBox : String -> String -> Html Msg
colorInfoBox label value =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "margin-top" "10px"
        , style "width" "100%"
        ]
        [ div
            [ style "flex" "1"
            , style "text-align" "left"
            ]
            [ text label ]
        , div
            [ style "flex" "2"
            , style "display" "flex"
            , style "align-items" "center"
            ]
            [ input
                [ type_ "text"
                , Html.Attributes.value value
                , Html.Attributes.readonly True
                , style "flex" "1"
                , style "margin-right" "5px"
                ]
                []
            , button
                [ onClick (CopyToClipboard value)
                , style "cursor" "pointer"
                ]
                [ text "Copy" ]
            ]
        ]

decimalToHex : Int -> String
decimalToHex decimal =
    let
        hexChars = "0123456789ABCDEF"
        
        toHexHelper n acc =
            if n == 0 then
                acc
            else
                toHexHelper (n // 16) (String.cons (String.slice (modBy 16 n) (modBy 16 n + 1) hexChars |> String.uncons |> Maybe.map Tuple.first |> Maybe.withDefault '0') acc)
    in
    if decimal == 0 then
        "00"
    else
        String.padLeft 2 '0' (toHexHelper decimal "")

rgbToHex : Model -> String
rgbToHex model =
    "#" ++ decimalToHex model.red ++ decimalToHex model.green ++ decimalToHex model.blue

rgbToHsv : Int -> Int -> Int -> (Int, Float, Int)
rgbToHsv r g b =
    let
        rf = toFloat r / 255
        gf = toFloat g / 255
        bf = toFloat b / 255
        cmax = max (max rf gf) bf
        cmin = min (min rf gf) bf
        delta = cmax - cmin
        hue =
            if delta == 0 then
                0
            else if cmax == rf then
                (60 * ((gf - bf) / delta) + 360) |> round |> modBy 360
            else if cmax == gf then
                (60 * ((bf - rf) / delta) + 120) |> round |> modBy 360
            else
                (60 * ((rf - gf) / delta) + 240) |> round |> modBy 360
        saturation =
            if cmax == 0 then
                0
            else
                delta / cmax
        value = cmax
    in
    ( hue
    , saturation * 100
    , round (value * 100)
    )

hsvToRgb : Float -> Float -> Float -> (Int, Int, Int)
hsvToRgb h s v =
    let
        hi = floor (h / 60)
        f = h / 60 - toFloat hi
        p = v * (1 - s)
        q = v * (1 - f * s)
        t = v * (1 - (1 - f) * s)
        (r, g, b) =
            case hi of
                0 -> (v, t, p)
                1 -> (q, v, p)
                2 -> (p, v, t)
                3 -> (p, q, v)
                4 -> (t, p, v)
                _ -> (v, p, q)
    in
    ( round (r * 255)
    , round (g * 255)
    , round (b * 255)
    )

rgbToIntString : Model -> String
rgbToIntString model =
    String.fromInt model.red ++ ", " ++ String.fromInt model.green ++ ", " ++ String.fromInt model.blue

hsvToIntString : Model -> String
hsvToIntString model =
    String.fromInt model.hue ++ ", " ++ String.fromInt model.saturation ++ ", " ++ String.fromInt model.value

hsvToHexString : Model -> String
hsvToHexString model =
    "#" ++ decimalToHex model.hue ++ decimalToHex model.saturation ++ decimalToHex model.value

-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }

port copyToClipboard : String -> Cmd msg
