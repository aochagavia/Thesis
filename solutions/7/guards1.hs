select :: Field -> Field -> Table -> Table
select column value table@(header:rows)
    | i == (-1) = table
    | otherwise = header : filter cond rows
        where i = fromMaybe (-1) (elemIndex column header)
              cond r = r !! i == value
