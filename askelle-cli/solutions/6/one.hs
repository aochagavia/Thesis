printTable table@(header:rows)
    = line : headerLine : line : otherlines ++ line : [] where
    line = printLine width
    width = columnWidths table
    headerLine = printRow (zip width (map (map (toUpper)) header))
    otherlines = map (printRow . zip width) rows
