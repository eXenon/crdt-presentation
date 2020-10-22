module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import CRDT
import Dict exposing (Dict)
import Html exposing (Html, br, button, div, h1, input, span, text)
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode as JD
import Set exposing (Set)
import Svg
import Svg.Attributes exposing (fill, height, r, stroke, textAnchor, width, x, x1, x2, y, y1, y2)
import TreeDiagram
import TreeDiagram.Svg



-- MSG


type Msg
    = KeyDown Int String
    | MoveCursor Int Int
    | ToggleConnection ( Int, Int )
    | ShowTree Int
    | ToggleShowAtoms
    | AddHost
    | NOP



-- EDITOR


initial : String
initial =
    "Hello, reader !"


vertexAtCursor : CRDT.Vertex -> Int -> List CRDT.Vertex -> CRDT.Vertex
vertexAtCursor root idx l =
    case l of
        h :: t ->
            if idx == 0 then
                h

            else if idx < 0 then
                root

            else
                vertexAtCursor root (idx - 1) t

        [] ->
            root


keyToModelTransformation : String -> Host -> ( Host, Maybe CRDT.Operation )
keyToModelTransformation key model =
    case key of
        "Dead" ->
            ( model, Nothing )

        "Shift" ->
            ( model, Nothing )

        "AltGrp" ->
            ( model, Nothing )

        "Control" ->
            ( model, Nothing )

        "ArrowRight" ->
            ( { model
                | cursor = min (model.cursor + 1) (List.length model.content.weave - 1)
              }
            , Nothing
            )

        "ArrowLeft" ->
            ( { model
                | cursor = max (model.cursor - 1) 0
              }
            , Nothing
            )

        "Backspace" ->
            let
                ( c, o ) =
                    CRDT.remove
                        model.content
                        (vertexAtCursor model.content.root model.cursor (CRDT.read model.content))

                new_cursor =
                    model.cursor - 1
            in
            ( { model | content = c, cursor = new_cursor }, Just o )

        "Enter" ->
            ( model, Nothing )

        s ->
            let
                ( c, o ) =
                    CRDT.insert
                        model.content
                        (vertexAtCursor model.content.root model.cursor (CRDT.read model.content))
                        s
            in
            ( { model | content = c, cursor = model.cursor + 1 }, Just o )



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }


keyDecoder : Int -> JD.Decoder Msg
keyDecoder host =
    JD.field "key" JD.string
        |> JD.andThen (\s -> JD.succeed (KeyDown host s))



-- MODEL


type alias Host =
    { id : Int
    , content : CRDT.Model
    , cursor : Int
    , pending_operations : Dict Int (List CRDT.Operation)
    }


type alias Model =
    { host : Dict Int Host
    , next_host : Int
    , connexions : Set ( Int, Int )
    , global_operations : List CRDT.Operation
    , displayed_tree : Int
    , show_atoms : Bool
    }


addNewConnexions_rec : Int -> Set ( Int, Int ) -> Int -> Set ( Int, Int )
addNewConnexions_rec new_host_id acc current =
    -- Generate a set of all connexions between the new host and every old host
    if current >= new_host_id then
        acc

    else
        addNewConnexions_rec
            new_host_id
            (acc |> Set.insert ( current, new_host_id ) |> Set.insert ( new_host_id, current ))
            (current + 1)


addHost : Model -> Model
addHost model =
    let
        init_host =
            CRDT.init model.next_host

        host_content =
            List.foldr
                (\o m -> CRDT.apply m o)
                init_host
                model.global_operations

        new_host =
            { id = model.next_host
            , content = host_content
            , cursor = 0
            , pending_operations = Dict.empty
            }
    in
    { model
        | next_host = model.next_host + 1
        , host = Dict.insert model.next_host new_host model.host
        , connexions = addNewConnexions_rec new_host.id model.connexions 0
    }


init : Model
init =
    let
        init_host =
            CRDT.init 0

        ( host_content, operations ) =
            List.foldl
                (\c ( m, ops ) ->
                    let
                        ( new_m, o ) =
                            CRDT.insertAtEnd m c
                    in
                    ( new_m, o :: ops )
                )
                ( init_host, [] )
                (initial |> String.split "")

        new_host =
            { id = 0
            , content = host_content
            , cursor = 0
            , pending_operations = Dict.empty
            }
    in
    { host = Dict.insert 0 new_host Dict.empty
    , next_host = 1
    , connexions = Set.empty
    , global_operations = operations
    , displayed_tree = 0
    , show_atoms = True
    }


updateCursor : Model -> Int -> Int -> Model
updateCursor model host_id cursor =
    { model
        | host =
            Dict.update
                host_id
                (Maybe.map (\h -> { h | cursor = cursor }))
                model.host
    }


updateHost : Model -> Int -> (Host -> Host) -> Model
updateHost model host_id transform =
    { model
        | host =
            Dict.update
                host_id
                (Maybe.map transform)
                model.host
    }


allConnexionsFromHost_rec : Int -> Int -> Int -> List ( Int, Int )
allConnexionsFromHost_rec host_count host current =
    if host == current then
        allConnexionsFromHost_rec host_count host (current + 1)

    else if current < host_count then
        ( host, current ) :: allConnexionsFromHost_rec host_count host (current + 1)

    else
        []


allConnexionsFromHost : Int -> Int -> List ( Int, Int )
allConnexionsFromHost host_count host =
    allConnexionsFromHost_rec host_count host 0


propagateOperation : Int -> CRDT.Operation -> Set ( Int, Int ) -> Int -> Host -> Host
propagateOperation origin op connexions host_id host =
    if origin == host_id then
        host

    else if Set.member ( origin, host_id ) connexions then
        { host | content = CRDT.apply host.content op }

    else
        { host | pending_operations = Dict.update origin (\l -> Just (op :: Maybe.withDefault [] l)) host.pending_operations }


applyHostTransformation : Int -> Model -> ( Host, Maybe CRDT.Operation ) -> Model
applyHostTransformation origin model ( host, mop ) =
    case mop of
        Just op ->
            { model
                | host =
                    model.host
                        |> Dict.insert origin host
                        |> Dict.map (propagateOperation origin op model.connexions)
                , global_operations = op :: model.global_operations
            }

        Nothing ->
            { model | host = Dict.insert origin host model.host }


propagatePendingOperations : Int -> Host -> Host
propagatePendingOperations origin host =
    host
        |> .pending_operations
        |> Dict.get origin
        |> Maybe.withDefault []
        |> List.foldr (\op h -> { h | content = CRDT.apply h.content op }) host
        |> (\h -> { h | pending_operations = Dict.insert origin [] h.pending_operations })



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    case msg of
        KeyDown host s ->
            let
                _ =
                    Debug.log "Pressed : " s
            in
            model.host
                |> Dict.get host
                |> Maybe.map (keyToModelTransformation s)
                |> Maybe.map (applyHostTransformation host model)
                |> Maybe.withDefault model

        MoveCursor host c ->
            updateCursor model host c

        AddHost ->
            addHost model

        ToggleConnection c ->
            if Set.member c model.connexions then
                { model | connexions = Set.remove c model.connexions }

            else
                -- Propagate pending operations
                let
                    ( origin, target ) =
                        c

                    new_host =
                        Dict.get target model.host
                            |> Maybe.map (propagatePendingOperations origin)
                in
                case new_host of
                    Nothing ->
                        { model | connexions = Set.insert c model.connexions }

                    Just h ->
                        { model
                            | connexions = Set.insert c model.connexions
                            , host = Dict.insert target h model.host
                        }

        ShowTree i ->
            { model | displayed_tree = i }

        ToggleShowAtoms ->
            { model | show_atoms = not model.show_atoms }

        NOP ->
            model



-- TREE VIEW


drawAtom : CRDT.Vertex -> String
drawAtom v =
    case CRDT.vertexType v of
        CRDT.Data " " ->
            "<space>"

        CRDT.Data s ->
            s

        CRDT.Root ->
            "ROOT"

        CRDT.Delete ->
            "⇦"

        CRDT.Corrupted ->
            "ERROR"


drawId : CRDT.Vertex -> String
drawId ( ( h, t ), _, _ ) =
    "{h=" ++ String.fromInt h ++ ",t=" ++ String.fromInt t ++ "}"


drawNode show_atoms n =
    let
        content =
            if show_atoms then
                drawAtom

            else
                drawId
    in
    Svg.node "g"
        []
        [ Svg.rect [ width "50", height "20", fill "white", x "-25", y "-10" ] []
        , Svg.text_
            [ fill "black", y "5px", textAnchor "middle" ]
            [ Svg.text (content n) ]
        ]


drawLine : ( Float, Float ) -> Svg.Svg msg
drawLine ( targetX, targetY ) =
    Svg.line
        [ x1 "0", y1 "0", x2 (String.fromFloat targetX), y2 (String.fromFloat targetY), stroke "black" ]
        []


drawTree : Bool -> Host -> Html Msg
drawTree show_atoms model =
    TreeDiagram.Svg.draw TreeDiagram.defaultTreeLayout (drawNode show_atoms) drawLine (CRDT.toTree model.content)



-- STYLES


styleCursor : Bool -> List (Html.Attribute Msg)
styleCursor b =
    if b then
        [ HA.style "border-right" "1px solid black" ]

    else
        []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ HE.onClick AddHost ] [ text "Add host" ]
        , div [ HA.style "float" "left" ]
            (List.map
                (viewHost model.next_host model.connexions)
                (Dict.values model.host)
            )
        , div [ HA.style "float" "right" ]
            [ viewTree model ]
        ]


viewTree : Model -> Html Msg
viewTree model =
    div []
        [ viewTreeButtons model.next_host
        , viewActiveTree model
        ]


viewTreeButtons : Int -> Html Msg
viewTreeButtons host_count =
    div []
        [ button [ HE.onClick ToggleShowAtoms ] [ text "Toggle Atoms / IDs" ]
        , div []
            (List.map
                (\i -> button [ HE.onClick (ShowTree i) ] [ text ("Show tree of Host " ++ String.fromInt i) ])
                (List.range 0 (host_count - 1))
            )
        ]


viewActiveTree : Model -> Html Msg
viewActiveTree model =
    case Dict.get model.displayed_tree model.host of
        Nothing ->
            div [] []

        Just h ->
            div
                [ HA.style "max-height" "600px"
                , HA.style "overflow" "scroll"
                ]
                [ drawTree model.show_atoms h ]


viewHost : Int -> Set ( Int, Int ) -> Host -> Html Msg
viewHost host_count connexions host =
    div []
        [ h1 [] [ text ("Host " ++ String.fromInt host.id) ]
        , div
            [ HA.tabindex 1
            , HA.contenteditable False
            , HE.on "keydown" (keyDecoder host.id)
            , HA.style "padding" "10px"
            , HA.style "border" "1px solid black"
            , HA.style "width" "300px"
            ]
            (List.indexedMap
                (viewElement host.id host.cursor)
                (CRDT.read host.content)
            )
        , div [ HA.style "width" "600px" ]
            (List.map
                (\c -> viewConnexion c (Set.member c connexions))
                (allConnexionsFromHost host_count host.id)
            )
        ]


viewConnexion : ( Int, Int ) -> Bool -> Html Msg
viewConnexion ( s, e ) active =
    div
        [ HE.onClick (ToggleConnection ( s, e ))
        ]
        [ input
            [ HA.type_ "checkbox"
            , HA.checked active
            ]
            []
        , text
            (String.fromInt s ++ " → " ++ String.fromInt e)
        ]


viewAtom : CRDT.Vertex -> Html Msg
viewAtom t =
    case CRDT.vertexType t of
        CRDT.Data " " ->
            text "\u{00A0}"

        CRDT.Data s ->
            text s

        CRDT.Root ->
            text ""

        CRDT.Delete ->
            text ""

        CRDT.Corrupted ->
            text "ERROR"


viewElement : Int -> Int -> Int -> CRDT.Vertex -> Html Msg
viewElement host_id cursor index node =
    span
        ([ HA.contenteditable False
         , HE.onClick (MoveCursor host_id index)
         ]
            ++ styleCursor (cursor == index)
        )
        [ viewAtom node ]
