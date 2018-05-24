project :: [Field] -> Table -> Table
project columns table@(header:_)
    = transpose (mapMaybe getColumn columns)
    where getColumn name = find (\x -> name == x!!0) (transpose table)

-- Transpose table, so we get a list of columns
-- Get all columns that begin with the given name
-- Transpose back
