module Tree exposing
    ( Tree(..), Forest, singleton
    , isEmpty, item, children, descendants
    , insert
    , map, map2, filter, filterMap, sort, sortBy, sortWith, andMap, flatten, andThen
    , forestMap, forestMap2
    )

{-| This module implements Rose Tree data structure.

> In computing, a multi-way tree or rose tree is a tree data structure
> with a variable and unbounded number of branches per node.


# Types & Constructor

@docs Tree, Forest, singleton


# Query

@docs isEmpty, item, children, descendants


# Modify

@docs insert


# Transforms

@docs map, map2, filter, filterMap, sort, sortBy, sortWith, andMap, flatten, andThen


# Forest

@docs forestMap, forestMap2

-}


{-| -}
type Tree a
    = Tree a (Forest a)


{-| -}
type alias Forest a =
    List (Tree a)



-- Tree


{-| Puts value in minimal `Tree` context

    singleton "foo"
        |> item
    --> "foo"

-}
singleton : a -> Tree a
singleton a =
    Tree a []


{-| Check if `Tree` doesn't have any child.

    singleton "foo"
        |> isEmpty
    --> True

    singleton "foo"
        |> insert (singleton "bar")
        |> isEmpty
    --> False

-}
isEmpty : Tree a -> Bool
isEmpty =
    List.isEmpty << children


{-| Obtain item from `Tree`.

    singleton "foo"
        |> item
        |> "foo"

-}
item : Tree a -> a
item (Tree i _) =
    i


{-| Obtain children items of `Tree`.

    singleton "foo"
        |> insert (singleton "bar")
        |> insert (singleton "baz")
        |> children
    --> [ "bar", "baz" ]

-}
children : Tree a -> List a
children =
    List.map item << descendants


{-| Obtain descendants as `Forest` from the `Tree`.

    singleton "foo"
        |> insert (singleton "bar")
        |> insert (singleton "baz")
        |> descendants
        |> List.map item
    --> [ "bar", "baz" ]

    singleton "foo"
        |> insert (singleton "bar" |> insert (singleton "baz"))
        |> descendants
        |> List.map (children)
    --> [ [ "baz" ] ]

-}
descendants : Tree a -> Forest a
descendants (Tree _ d) =
    d


{-| Map function over `Tree`.

    singleton 1
        |> map ((+) 1)
        |> item
    --> 2

    singleton 1
        |> insert (singleton 2)
        |> insert (singleton 3)
        |> map ((*) 2)
        |> children
    --> [ 4, 6 ]

-}
map : (a -> b) -> Tree a -> Tree b
map predicate (Tree a forest) =
    Tree (predicate a) <| forestMap predicate forest


{-| Map function over two `Tree`s

    map2 (+) (singleton 1) (singleton 5)
        |> item
    --> 6

    import Lazy.LList as LL

    Tree 1 (List.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> map2 (+) (Tree 5 <| List.fromList [ singleton 6, singleton 7 ])
        |> children
    --> [ 8, 10 ]

-}
map2 : (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2 predicate (Tree a1 f1) (Tree a2 f2) =
    Tree (predicate a1 a2) <| forestMap2 predicate f1 f2


{-| Filter `Tree` children by given function.

This function goes from children of root downwards.
This means that nodes that doesn't satisfy predicate
are excluded and filter is never performed over their children
even if on those it might pass.

    Tree 1 [ singleton 2, singleton 3, singleton 4 ]
        |> filter ((>) 4)
        |> children
    --> [ 2, 3 ]

    Tree 1 [ insert (singleton 5) <| singleton 2, insert (singleton 6) <| singleton 3, singleton 4 ]
        |> filter ((<) 2)
        |> descendants
        |> List.map children
    --> [ [ 6 ], [] ]

-}
filter : (a -> Bool) -> Tree a -> Tree a
filter predicate (Tree i c) =
    Tree i <| List.filterMap (filter_ predicate) c


filter_ : (a -> Bool) -> Tree a -> Maybe (Tree a)
filter_ predicate (Tree i c) =
    if predicate i then
        Just <| Tree i <| List.filterMap (filter_ predicate) c

    else
        Nothing


{-| FilterMap on `Tree`. Works similarly to `List.filterMap` with same properties as [filter](#filter).
In case of `filterMap` even root node has to satisfy predicate otherwise
`Nothing` is returned.

    Tree 1 [ singleton 2, singleton 3, singleton 4 ]
        |> filterMap (\a -> if a < 4 then Just (a * 2) else Nothing)
        |> Maybe.map children
    --> Just [ 4, 6 ]

    Tree 1 [ singleton 2, singleton 3, singleton 4 ]
        |> filterMap (\a -> if a > 2 then Just (a * 2) else Nothing)
        |> Maybe.map children
    --> Nothing

-}
filterMap : (a -> Maybe b) -> Tree a -> Maybe (Tree b)
filterMap predicate (Tree item_ c) =
    predicate item_
        |> Maybe.map (\i -> Tree i <| List.filterMap (filterMap predicate) c)


{-| Sort `tree`.

    singleton 10
        |> insert (singleton 5)
        |> insert (singleton 2)
        |> sort
        |> children
    --> [ 2, 5 ]

it applies all levels:

    import Lazy.LList as LL

    singleton 10
        |> insert (Tree 20 <| (List.reverse << List.map singleton << List.range 1) 5)
        |> sort
        |> descendants
        |> List.map children
    --> [ [ 1, 2, 3, 4, 5 ] ]

-}
sort : Tree comparable -> Tree comparable
sort (Tree a f) =
    Tree a <| List.map sort <| List.sortBy item f


{-| Sort `Tree` by a function.

    singleton { val = 10 }
       |> insert (singleton { val = 7 })
       |> insert (singleton { val = 3 })
       |> sortBy .val
       |> children
    --> [ { val = 3 }, { val = 7 } ]

it applies to all levels:

    singleton { a = 10 }
        |> insert (Tree { a = 20 } <| (List.reverse << List.map (\v -> singleton { a = v }) << List.range 1) 3)
        |> sortBy .a
        |> descendants
        |> List.map children
    --> [ [ { a = 1 }, { a = 2 }, { a = 3 } ] ]

-}
sortBy : (a -> comparable) -> Tree a -> Tree a
sortBy predicate (Tree a f) =
    Tree a <|
        List.map (sortBy predicate) <|
            List.sortBy (predicate << item) f


{-| Sort `Tree` using custom Ordering function

    flippedComparison : comparable -> comparable -> Order
    flippedComparison a b =
        case Basics.compare a b of
            LT -> GT
            EQ -> EQ
            GT -> LT

    singleton 10
        |> insert (singleton 2)
        |> insert (singleton 5)
        |> sortWith flippedComparison
        |> children
    --> [ 5, 2 ]

-}
sortWith : (a -> a -> Order) -> Tree a -> Tree a
sortWith predicate (Tree i f) =
    Tree i <|
        List.map (sortWith predicate) <|
            List.sortWith (\a b -> predicate (item a) (item b)) f


{-| Chain map operations.

    import Lazy.LList as LL

    Tree Tuple.pair [ singleton Tuple.pair, singleton Tuple.pair, singleton Tuple.pair ]
        |> andMap (Tree 1 <| List.fromList [ singleton 2, singleton 3, singleton 4 ])
        |> andMap (Tree 5 <| List.fromList [ singleton 6, singleton 7 ])
        |> children
    --> [ (2, 6), (3, 7) ]

-}
andMap : Tree a -> Tree (a -> b) -> Tree b
andMap =
    map2 (|>)


{-| Flatten `Tree` of Trees.

    singleton (singleton 1)
        |> flatten
        |> item
    --> 1

    Tree (Tree "foo" [ singleton "bar"]) [ singleton <| singleton "baz" ]
        |> flatten
        |> children
    --> [ "bar", "baz" ]

-}
flatten : Tree (Tree a) -> Tree a
flatten (Tree (Tree i c) f) =
    Tree i <| List.append c <| List.map flatten f


{-| Map given function onto a `Tree` and flatten the result.

    singleton "foo"
        |> insert (singleton "bar")
        |> insert (singleton "baz")
        |> andThen (\a -> Tree a [ singleton <| a ++ " fighter" ])
        |> children
    --> [ "foo fighter", "bar", "baz" ]

-}
andThen : (a -> Tree b) -> Tree a -> Tree b
andThen fc =
    flatten << map fc


{-| Insert one `Tree` as children another.

    singleton 1
        |> insert (singleton 2)
        |> insert (singleton 3)
        |> children
    --> [ 2, 3 ]

    singleton 1
        |> insert (singleton 2)
        |> item
    --> 1

-}
insert : Tree a -> Tree a -> Tree a
insert t (Tree i c) =
    Tree i <| List.append c [ t ]



-- Forest


{-| Map function over `Forest`.

    import Lazy.LList as LL

    [ singleton 1, singleton 2, singleton 3 ]
        |> forestMap ((+) 1)
        |> List.map item
    --> [ 2, 3, 4 ]

-}
forestMap : (a -> b) -> Forest a -> Forest b
forestMap predicate =
    List.map (map predicate)


{-| Map function over two `Forest`s.

    [ singleton 1, singleton 2, singleton 3 ]
        |> forestMap2 (+) [ singleton 1, singleton 2]
        |> List.map item
        |> List.toList
    --> [ 2, 4 ]

-}
forestMap2 : (a -> b -> c) -> Forest a -> Forest b -> Forest c
forestMap2 predicate =
    List.map2 (map2 predicate)
