printTable :: Table  -> [String]
printTable table@(header:rows)
    = [printLine cmwth] ++ [map toUpper (f header)] ++ [printLine cmwth] ++ map f rows ++ [printLine cmwth]
        where cmwth = columnWidths table
              f = printRow . (\x -> zip cmwth x)
