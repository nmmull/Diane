module View exposing (..)

import Html exposing (Html, text, button, textarea, div, pre)
import Html.Events exposing (onClick, onInput, onMouseDown, preventDefaultOn)
import Html.Attributes exposing (id, value, placeholder, disabled, style, spellcheck, autocomplete)
import Dict
import Diane exposing (..)
import Model exposing (..)
import Update exposing (..)

editorWindow : Model -> Html Msg
editorWindow m =
    div [ id "editor-window" ]
        [ textarea
            [ id "editor"
            , placeholder "Write your program here..."
            , spellcheck False
            , autocomplete False
            , value m.config.program
            , disabled (m.going)
            , onInput Change
            ]
            []
        , button
            [ id "save-button"
            , onClick Save
            , disabled (m.savedProgram == m.config.program)
            ]
            [ text "save" ]
        ]

buttons : Model -> Html Msg
buttons m =
    div [ id "buttons" ]
        [ button
            [ onClick Step
            , disabled (done m.config)
            ]
            [ text "step" ]
        , button
            [ onClick Undo
            , disabled (List.isEmpty m.history)
            ]
            [ text "undo" ]
        , button
            [ onClick Eval
            , disabled (done m.config)
            ]
            [ text "run" ]
        , button
            [ onClick Reset
            , disabled (List.isEmpty m.history)
            ] [ text "reset" ]
        ]

percent f = String.fromFloat (max 5.0 (min (100 * f) 95.0)) ++ "%"
pEventX m =
    case m.dragX of
        Static _ -> "auto"
        Moving _ -> "none"
pEventY m =
    case m.dragY of
        Static _ -> "auto"
        Moving _ -> "none"

console : Model -> Html Msg
console m =
    let
        traceHtml =
            div [] [ text (String.join " " (List.reverse m.trace)) ]
    in
    -- let line s = div [] [ text s ] in
    -- let traceHtml = div [] (List.map line (List.reverse m.trace)) in
    div
        [ id "console-pane"
        , style "pointer-events" (pEventY m)
        , style "user-select" (pEventY m)
        , style "height" (percent (1 - fracY m))
        ]
        [ div [ id "console-window" ]
            [ div [ id "console" ] [ traceHtml ] ]
        ,  button
            [ id "clear-console"
            , disabled (List.isEmpty m.trace)
            , onClick ClearConsole
            ]
            [ text "clear" ]
        ]

funHtml : String -> Prog -> Html Msg
funHtml name body =
    div
        [ id "function" ]
        [ div [ id "function-placeholder" ] [ text (name ++  " ↦ <function>") ]
        , pre [ id "function-body" ] [ text body ]
        ]

intHtml : String -> Int -> Html Msg
intHtml name val =
    div [] [ text (name ++ " ↦ " ++ String.fromInt val) ]

bindHtml : (String, Value) -> Html Msg
bindHtml (name, val) =
    case val of
        Number v -> intHtml name v
        Subroutine f -> funHtml name f

bindsHtmls : Bindings -> List (Html Msg)
bindsHtmls bs =
    List.map (\b -> Html.li [] [ bindHtml b ]) (Dict.toList bs)

envHtmls : Env -> List (Html Msg)
envHtmls e =
    List.map (\bs -> Html.ul [] (bindsHtmls bs)) (List.reverse (envToList e))

header : Int -> Html Msg
header i =
    case i of
        0 -> Html.b [] [ text "GLOBAL" ]
        _ -> Html.b [] [ text ("LOCAL " ++ String.fromInt i) ]

viz : Model -> Html Msg
viz m =
    div
        [ id "viz-window"
        , style "pointer-events" (pEventY m)
        , style "user-select" (pEventY m)
        , style "height" (percent (fracY m))
        ]
        [ div [ id "viz" ]
            [ Html.h3 [] [ text "Stack" ]
            , div [] [ text (stackStr m.config.stack) ]
            , Html.h3 [] [ text "Environment" ]
            , Html.ul [] (List.indexedMap (\i x -> Html.li [] [ header i, x ]) (envHtmls m.config.env))
            ]
        -- , button
        --     [ id "clear-data"
        --     , disabled (List.isEmpty m.config.stack && Dict.isEmpty m.config.env)
        --     , onClick ClearData
        --     ]
        --     [ text "clear" ]
        ]

vsplit : Model -> Html Msg
vsplit m =
    let
        adjust =
            if m.adjustable
            then onMouseDown DragStartX
            else style "cursor" "auto"
    in
    div (adjust :: [ id "vsplit" , style "left" (percent (fracX m))])
        [ div [ id "vline" ] [] ]

hsplit : Model -> Html Msg
hsplit m =
    let
        adjust =
            if m.adjustable
            then onMouseDown DragStartY
            else style "cursor" "auto"
    in
    div (adjust :: [ id "right-split", style "top" (percent (fracY m)) ])
        [ div [ id "hline" ] [] ]

view : Model -> Html Msg
view m =
    div
        [ id "view"
        , preventDefaultOn "keydown" shortcuts
        ]
        [ div
            [ id "left-pane"
            , style "pointer-events" (pEventX m)
            , style "user-select" (pEventX m)
            , style "width" (percent (fracX m))
            ]
            [ editorWindow m
            , div [ id "left-split" ] []
            , buttons m
            ]
        , vsplit m
        , div
            [ id "right-pane"
            , style "pointer-events" (pEventX m)
            , style "user-select" (pEventX m)
            , style "width" (percent (1 - fracX m))
            ]
            ((viz m) :: (if m.hasTrace then [hsplit m, console m] else []))
        ]
