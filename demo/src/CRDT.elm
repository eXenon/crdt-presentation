{--
    Causal Graph CRDT implementation
--}


module CRDT exposing (..)

import Dict exposing (Dict)
import SortedTree
import TreeDiagram


type alias Id =
    ( Int, Int )


type alias Atom =
    ( Int, String )


type Operation
    = Insert Id Id Atom
    | Remove Id Id


type VertexType
    = Data String
    | Root
    | Delete
    | Corrupted


vertexType : Vertex -> VertexType
vertexType ( _, _, a ) =
    case a of
        ( 0, _ ) ->
            Root

        ( 1, s ) ->
            Data s

        ( 2, _ ) ->
            Delete

        ( _, _ ) ->
            -- Should never happen !
            Corrupted


type alias Vertex =
    ( Id, Id, Atom )


type alias Model =
    { state : SortedTree.T Vertex
    , next_id : Id
    , weave : List Vertex
    , root : Vertex
    , uninsertable_operations : List Operation
    , vertices : Dict Id Vertex
    }


increment : Id -> Id
increment ( h, t ) =
    ( h, t + 1 )


plaholder_id : Id
plaholder_id =
    ( 0, 0 )


plaholder_atom : Atom
plaholder_atom =
    ( 3, "" )


init : Int -> Model
init host =
    let
        root =
            ( ( 0, 0 ), ( 0, 0 ), ( 0, "" ) )

        gt =
            \( id1, _, _ ) ( id2, _, _ ) -> id1 > id2

        eq =
            \( id1, _, _ ) ( id2, _, _ ) -> id1 == id2
    in
    { state = SortedTree.make gt eq root
    , next_id = ( host, 1 )
    , weave = []
    , root = root
    , vertices = Dict.empty |> Dict.insert ( 0, 0 ) root
    , uninsertable_operations = []
    }


find : Model -> Id -> Maybe Vertex
find model id =
    Dict.get
        id
        model.vertices


successor_rec : List Vertex -> Vertex -> Vertex
successor_rec l n =
    case l of
        h1 :: h2 :: t ->
            if h1 == n then
                h2

            else
                successor_rec (h2 :: t) n

        _ ->
            n


successor : Model -> Vertex -> Vertex
successor model node =
    successor_rec model.weave node


predecessor : Model -> Vertex -> Vertex
predecessor model ( id, cause, _ ) =
    let
        cause_vertex =
            find model cause
                |> Maybe.withDefault model.root
    in
    case vertexType cause_vertex of
        Data _ ->
            cause_vertex

        Root ->
            cause_vertex

        Delete ->
            predecessor model cause_vertex

        Corrupted ->
            cause_vertex


insert : Model -> Vertex -> String -> ( Model, Operation )
insert model cause atom =
    let
        ( cause_id, _, _ ) =
            cause

        vertex =
            ( model.next_id, cause_id, ( 1, atom ) )

        state_ =
            SortedTree.insertBelow cause vertex model.state
    in
    ( { model
        | vertices = Dict.insert model.next_id vertex model.vertices
        , next_id = increment model.next_id
        , weave = build_weave state_
        , state = state_
      }
    , Insert model.next_id cause_id ( 1, atom )
    )


insertAtEnd : Model -> String -> ( Model, Operation )
insertAtEnd model atom =
    let
        cause =
            model.weave
                |> List.reverse
                |> List.head
                |> Maybe.withDefault model.root
    in
    insert model cause atom


remove : Model -> Vertex -> ( Model, Operation )
remove model cause =
    let
        ( cause_id, _, _ ) =
            cause

        vertex =
            ( model.next_id, cause_id, ( 2, "" ) )

        state_ =
            SortedTree.insertBelow cause vertex model.state
    in
    ( { model
        | vertices = Dict.insert model.next_id vertex model.vertices
        , next_id = increment model.next_id
        , weave = build_weave state_
        , state = state_
      }
    , Remove model.next_id cause_id
    )


retryOperations : Model -> Model
retryOperations model =
    let
        ops =
            model.uninsertable_operations
    in
    List.foldr
        (\o m -> apply m o)
        { model | uninsertable_operations = [] }
        ops


apply : Model -> Operation -> Model
apply model operation =
    case operation of
        Insert id cause atom ->
            if Dict.get cause model.vertices /= Nothing then
                let
                    vertex =
                        ( id, cause, atom )

                    cause_dummy =
                        -- Our comparison function only looks at the ID, so the rest doesn't matter
                        ( cause, plaholder_id, plaholder_atom )

                    state_ =
                        SortedTree.insertBelow cause_dummy vertex model.state
                in
                { model
                    | vertices = Dict.insert id vertex model.vertices
                    , weave = build_weave state_
                    , state = state_
                }
                    |> retryOperations

            else
                { model | uninsertable_operations = operation :: model.uninsertable_operations }

        Remove id cause ->
            if Dict.get cause model.vertices /= Nothing then
                let
                    vertex =
                        ( id, cause, ( 2, "" ) )

                    cause_dummy =
                        -- Our comparison function only looks at the ID, so the rest doesn't matter
                        ( cause, plaholder_id, plaholder_atom )

                    state_ =
                        SortedTree.insertBelow cause_dummy vertex model.state
                in
                { model
                    | vertices = Dict.insert id vertex model.vertices
                    , weave = build_weave state_
                    , state = state_
                }
                    |> retryOperations

            else
                { model | uninsertable_operations = operation :: model.uninsertable_operations }


build_weave_rec : List Vertex -> List Vertex -> List Vertex
build_weave_rec acc weave =
    case weave of
        v :: t ->
            case vertexType v of
                Root ->
                    build_weave_rec acc t

                Data s ->
                    build_weave_rec (v :: acc) t

                Delete ->
                    let
                        ( _, deleted_id, _ ) =
                            v
                    in
                    build_weave_rec (acc |> List.filter (\( i, _, _ ) -> deleted_id /= i)) t

                Corrupted ->
                    build_weave_rec acc t

        [] ->
            acc


build_weave : SortedTree.T Vertex -> List Vertex
build_weave tree =
    tree
        |> SortedTree.prefix
        |> build_weave_rec []
        |> List.reverse


read : Model -> List Vertex
read model =
    model.weave


toTree : Model -> TreeDiagram.Tree Vertex
toTree model =
    SortedTree.toTree model.state
