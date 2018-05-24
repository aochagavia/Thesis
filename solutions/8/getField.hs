project :: [Field] -> Table -> Table
project colNames table@(header:_) =
        map (\row -> map (getField row) (mapMaybe getColIndex colNames)) table
    where
        getColIndex colName = elemIndex colName header
        getField row index = row !! index
