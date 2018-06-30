select :: Field -> Field -> Table -> Table
select column value table@(header:rows)
            =  header : filter predicate rows
    where   predicate xs = maybe True (f xs) (elemIndex column header)
            f :: Row -> Int -> Bool
            f xs h = (xs !! h) == value
