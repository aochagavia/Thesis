select column value table@(header:rows)
    = maybe table
            (\n -> header : filter (\row -> (row !! n) == value) rows)
            (elemIndex column header)
