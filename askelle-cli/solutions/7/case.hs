select column value table@(header:rows)
    = let columnNumber = elemIndex column header
    in case columnNumber of
      Nothing -> table
      Just found -> header : filter (\f -> f !! found == value) rows