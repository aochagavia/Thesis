project columns table@(header:_)
    = transpose(map takeRow(mapMaybe checkExist columns))
      where
        checkExist column = elemIndex column header
        takeRow num = transpose table!!num
