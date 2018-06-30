project :: [Field] -> Table -> Table
project columns table@(header:_)
    = transpose (map projectC (map fromJust fIndexes))
    where projectC indexes = transpose table !! indexes
          cFilter = filter (`elem` header) columns
          fIndexes = map gElem cFilter
          gElem c = elemIndex c header
