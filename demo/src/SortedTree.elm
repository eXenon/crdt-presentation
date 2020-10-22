module SortedTree exposing (T, insertBelow, make, prefix, toTree)

import TreeDiagram


type Tree a
    = Node a (List (Tree a))


type alias T a =
    { gt : a -> a -> Bool
    , eq : a -> a -> Bool
    , t : Tree a
    }


sort : (a -> a -> Bool) -> (a -> a -> Bool) -> (Tree a -> Tree a -> Order)
sort gt eq =
    \(Node a _) (Node b _) ->
        if eq a b then
            EQ

        else if gt a b then
            LT

        else
            GT


make : (a -> a -> Bool) -> (a -> a -> Bool) -> a -> T a
make gt eq n =
    { gt = gt, eq = eq, t = Node n [] }


insertIntoList : (a -> a -> Bool) -> (a -> a -> Bool) -> a -> List (Tree a) -> List (Tree a)
insertIntoList gt eq node tree_list =
    let
        new_tree =
            Node node []
    in
    new_tree
        :: tree_list
        |> List.sortWith (sort gt eq)


insertBelow_rec : (a -> a -> Bool) -> (a -> a -> Bool) -> a -> a -> Tree a -> Tree a
insertBelow_rec gt eq parent node (Node n children) =
    if eq n parent then
        Node n (insertIntoList gt eq node children)

    else
        Node n (List.map (insertBelow_rec gt eq parent node) children)


insertBelow : a -> a -> T a -> T a
insertBelow parent node tree =
    { tree | t = insertBelow_rec tree.gt tree.eq parent node tree.t }


prefix_req : Tree a -> List a
prefix_req t =
    case t of
        Node d [] ->
            [ d ]

        Node d children ->
            d :: List.concat (List.map prefix_req children)


prefix : T a -> List a
prefix tree =
    prefix_req tree.t


toTree_rec : Tree a -> TreeDiagram.Tree a
toTree_rec (Node n children) =
    TreeDiagram.node n (List.map toTree_rec children)


toTree : T a -> TreeDiagram.Tree a
toTree tree =
    toTree_rec tree.t
