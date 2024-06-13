module View exposing (..)

import Html exposing (Html, text, button, textarea, div)
import Html.Events exposing (onClick, onInput, onMouseDown, preventDefaultOn)
import Html.Attributes exposing (id, value, placeholder, disabled, style)
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

envHtmls : Env -> List (Html Msg)
envHtmls e =
    let
        go bs =
            case bs of
                [] -> []
                (x, val) :: rest -> (x ++ " ↦ " ++ valString val) :: go rest
    in
    List.map (\s -> div [] [ text s ]) (go (Dict.toList e))

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
            , Html.ul [] (List.map (\x -> Html.li [] [ x ]) (envHtmls m.config.env))
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
