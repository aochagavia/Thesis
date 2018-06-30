select column value table@(header:rows) | not present = table
                                        | otherwise = header: filter ((==value) . (!!position)) rows
    where       elemindex   = elemIndex column header
                present     = isJust elemindex
                position    = fromJust elemindex
