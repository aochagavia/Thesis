Before:

57
8
5
3
3
2
-> 78

After:

71
6
5
3
2
-> 87

Wrong: 9
Good: 102

# The exercise

Write a function `project :: [String] -> Table -> Table` that projects several columns from a table.

Note: If a given column is not present in the original table it should be omitted from the resulting table

# Issues

Singleton groups (27):
* 4132998: for each column name, retrieve its projected table. Concat afterwards and transpose back.
* 5987857: the code to retrieve the columns returns a `[]` if the column has not been projected. However, the `[]`s are filtered away by `transpose`

Unnamed group (3), wontfix: reimplement `mapMaybe` (first `filter . flip elem`, then `map $ fromJust . elemIndex`)
* 6014542
* 5911486
* 5721156

Wontfix:
* 3921387: uses `isNothing` and `fromMaybe`, adds a useless base case in a helper function
* 4020332: uses `isJust` and `fromJust`, maps everything `toLower`
* 4257383: useless base cases everywhere
* 5495792: useless base case in helper function
* 5553865: uses `fromMaybe` as `isJust`, then as `fromJust`
* 5715938: uses `isJust` and `fromMaybe`
* 5943272: filters `isJust`, maps `fromJust`
* 5956935: filters `isJust`, maps `fromJust`
* 5989973: uses `toUpper` when comparing strings to ignore casing
* 6303013: filters `isJust`, maps `fromJust`
* 6310621: uses `fromMaybe` as `isJust`, then as `fromJust`

Wrong:
* + 5542731: crashes if no column names are given
* + 5546354: will project any column that contains the term, not only in the header!
* + 5551528: returns [[]] if no column names are given
* + 5707617: will project any column that contains the term, not only in the header!
* + 5747929: returns the original table if the projection is empty
* + 5750032: returns the original table if the projection is empty
* + 5905818: crashes if no column names are given
* + 5936608: returns [[]] if no column names are given
* + 5941016: returns a transposed table
* + 5978831: returns the original table if the projection is empty
* + 6303919: crashes when the projection is empty

# Fixed

Unnamed group (3): too much logic inside the function passed to `mapMaybe`. It could be extracted to a `map` after `mapMaybe`
* 6318908
* 5938600
* 4132955

* 5630177: uses `mapMaybe` instead of `map`
* 6323499: uses `foldr` as `id`
* 5723493: uses `table !! 0` instead of `header`

# Added

* Base case removal
* Apply const
* Rewrite `mapMaybe (\f -> Just e)` as `map (\f -> e)`
* Rewrite `foldr f d []` as `d`
* Rewrite `!! 0` as `head`
* Rewrite `mapMaybe (\x -> fmap f e) xs` as `map f $ mapMaybe (\x -> e) xs`

# Comments

Lots of students using variants of `isJust` and `fromJust`. Some of them even writing their own versions, like `fromMaybe (-1) x < 0` to use in the condition of an if and in the *then* branch.

Lots of students trying to be overly clever by making assumptions about the input, while nothing is said in the specification: crashes on the empty projection and returning the original table. Only two of the wrong solutions are true errors, the rest is just a consequence of not following the spec.

Again, the problem of underspecification. What do we consider correct? Right now, one of the model solutions is not semantically equivalent to the others (returns [[]] if no column names are given).
