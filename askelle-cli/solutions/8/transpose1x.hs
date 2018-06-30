project :: [Field] -> Table -> Table
project columns table@(header:_)
    = transpose (map getRow colIndices)
    where colIndices = mapMaybe getColIndex columns
          getColIndex colName = elemIndex colName header
          getRow colIndex = map (!! colIndex) table

-- For each projected column:
--   For each row:
--     Get the field corresponding to the column
--
-- Result: a list of columns
-- Tranpose necessary at the end