printTable :: Table -> [String]
printTable table@(header:rows)
    = hLine : pHeader : [hLine] ++ pRows ++ [hLine]
    where widths = columnWidths table
          hLine = printLine widths
          pHeader = printRow (zip widths header)
          pRows = map (printRow . zip widths) rows
