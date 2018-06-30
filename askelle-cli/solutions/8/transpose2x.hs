project colNames table@(header:_)
    = transpose $ map getCol $ mapMaybe getColIndex colNames
      where
        getColIndex colName = elemIndex colName header
        getCol index = transpose table !! index
