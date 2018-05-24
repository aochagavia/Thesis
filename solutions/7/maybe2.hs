select :: Field -> Field -> Table -> Table
select column value table@(header:rows)
    =  header : maybe rows function mbvalue
    where
    mbvalue = elemIndex column header
    function index = filter predicate rows
        where
        predicate row = (row !! index) == value
