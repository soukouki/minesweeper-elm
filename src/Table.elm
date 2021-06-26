module Table exposing (..)

type alias Table a = List (List a)

type alias TablePos = (Int, Int)

indexedMap : (TablePos -> a -> b) -> (Table a) -> Table b
indexedMap proc table =
  List.indexedMap (\y line ->
    List.indexedMap (\x item ->
      proc (x, y) item) 
      line) 
    table

getSurroundItem : Table a -> TablePos -> List (Maybe a)
getSurroundItem table (x, y) =
  let
    it = (\(ix, iy) -> at table (x+ix, y+iy))
  in
    [ it (-1, -1), it(0, -1), it(1, -1)
    , it (-1, 0),             it(1, 0)
    , it (-1, 1),  it(0, 1),  it(1, 1)
    ]

indexedFoldl : (a -> b -> TablePos -> b) -> b -> Table a -> b
indexedFoldl proc default table =
  .res
  <| List.foldl
    (\line { res, y } -> 
      { res = .resi
          <| List.foldl
              (\item { resi, x } ->
                { resi = proc item resi (x, y)
                , x = x+1}
              ) { resi = res, x=0 } line
      , y = y+1
      }
    ) { res=default, y=0 }
  <| table

at : Table a -> TablePos -> Maybe a
at table pos =
  indexedFoldl (\item res ipos ->
    if pos==ipos then
      Just item
    else
      res) Nothing table

all : (a -> Bool) -> Table a -> Bool
all proc table =
  indexedFoldl (\item res pos -> res && proc item) True table