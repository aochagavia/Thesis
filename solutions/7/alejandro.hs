select column value table@(header:rows)
    = maybe table
            (\n -> header : filter ((== value) . (!! n)) rows)
            (elemIndex column header)
