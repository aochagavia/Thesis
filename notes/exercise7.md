Before:

19
7
6
6
4
4
4
=> 50, 7 solutions (49%)

49 singletons!

After:
34
19
8
7
6
=> 74, 5 solutions (73%)

33 singletons!

Correct: 102

# The exercise

Write a function `select :: String -> String -> Table -> Table` that, given a column name and a field value, selects only those rows from the table that have the given field value in the given column.

Note: we use `elemIndex` to retrieve the index of the column in question. If the column name does not match any existing column, the table is returned unchanged.

# Model solutions

Maybe1: uses `maybe` at the top (returns the original table if `Nothing`)
Maybe2: uses `maybe` at the row level (returns the original rows if `Nothing`)
Maybe3: uses `maybe` at the filtering level (returns `True` if `Nothing`)
Case: uses case at the top level instead of maybe
Guards1: uses `fromMaybe (-1)` and checks in a guard whether the thing is `-1`. To me it seems like using `isJust` and `fromJust`.
Guards2: same as Guards1, but using `isJust` and `fromJust`

# Issues

Singletons: first get the indices of the rows that match, then get the rows using indexing (they don't unify due to different approaches)
* 4228316
* 5995442

Unnamed group (2) - wontfix: equivalent to Guards1, but reimplements `fromMaybe` as `maybe (-1) fromIntegral (elemIndex column header)`
* 6290655
* 6290639
Unnamed group (2) - wontfix: uses `elem column header` in combination with `fromJust (elemIndex column header)`
* 6014542
* 4132998

Wontfix:
* 4020332: reimplements `fromJust`, uses `toLower` somewhere
* 5533988: uses a magic number to indicate that the column was not found
* 5551528: uses `negate 1` instead of `-1`, adds a base case for when table = [], uses `< 0` when checking the result of `fromMaybe`
* 5630177: reimplements `fromJust`, uses it in the filtering condition
* 5632102: adds base case for [], uses `elem`, uses `fromMaybe` as a poor man's `fromJust`
* 5715938: uses `fromMaybe 0` as a poor man's `fromJust`
* 5730015: reimplements filter using guards instead of pattern matching and with a base case of [x]
* 5744652: transforms each non-matching row to the empty string and filters them out afterwards
* 5747929: reimplements filter with a base case of [x]
* 5847001: similar to `maybe1`, but filters the whole table with a special case in the condition to account for the header (instead of just prepending the header before the filter)
* 5936608: reimplements `fromMaybe 99999` and uses it as a poor man's `fromJust`
* 5941016: reimplements `fromMaybe (-5)` and uses it as a poor man's `fromJust`
* 5954878: similar to `guards1`, but uses `> -1` instead of `/= -1`
* 5963761: uses `+0` instead of `id`, reimplements `fromJust` with infinite recursion in the case of `Nothing` (using `maybe`)
* 5982065: reimplements elemIndex defaulting to 0 if the element is not present in the list
* 5989973: uses `find (equalsIgnoreCase colName)` to retrieve the name of the column and then `elemIndex` to retrieve its index
* 6229077: adds base case for [], uses `fromMaybe (-1)` but checks for `>= 0` and `< 0` in the guards (instead of `== -1`)
* 6303919: adds a guard checking for an impossible condition
* 6310621: reimplements `fromMaybe (-1)`, checks in guards for `== -1` and `>= 0`
* 6323367: but adds a base case for [], almost similar to `guards2` but uses `header : if ...` instead of having the if at the top level

Wrong:
* + 3921387: duplicates the header (also reimplements `isNothing` as `maybe True (const False)`, uses `maybe 0 id` as a poor man's `fromJust`)
* + 4157893: Checks if the value is present in the entire row, not only in the given column
* + 5495792: if the column does not exist, it will filter the rows against the first column
* + 5550904: filters the whole table instead of only the rows. Therefore, if the searched string is equal to the column name, the header would be displayed twice.
* + 5750865: adds base case for [], if the select results in an empty table, returns the original one (this deviates from the spec)
* + 5858348: adds base case for [], filters the whole table instead of only the rows. Therefore, if the searched string is equal to the column name, the header would be displayed twice.
* + 5878683: similar to `maybe1`, but filters the whole table and will keep rows with a value equal to the column name
* + 5926874: duplicates the header
* + 5936608: duplicates the header
* + 5978831: filters the whole table instead of only the rows. Therefore, if the searched string is equal to the column name, the header would be displayed twice.
* + 6303013: if the select results in an empty table, returns the original one (this deviates from the spec)
* + 6303919: duplicates the header

Wrong?

# Fixed

Unnamed group (2): equivalent to Guards2, but adds a base case for when table = []
* 6323375
* 5871476
Unnamed group (2): equivalent to Maybe1, but adds a base case for when table = []
* 6323472
* 5850983
Unnamed group (3): equivalent to Guards2, but uses `isNothing` instead of `isJust`
* 5995256
* 5893348
* 5721156
Unnamed group (4): equivalent to Maybe1, but arguments to `(==)` are swapped
* 6329616
* 5750032
* 5542731
* 4255143
Unnamed group (3): equivalent to Guards2, but arguments to `(==)` are swapped
* 6315100
* 6059929
* 4292839
Singleton groups:
* 4019768: reimplements `head` as `map head . transpose`
* 4257383: reimplements `fromMaybe (-1)` as `maybe (-1) id`, reimplements filter recursively
* 5500206: reimplements `fromJust`
* 5546354: reimplements `isNothing` as `== Nothing`, reimplements `filter cond` as `concatMap (\x -> if cond x then [x] else [])`
* 5732409: similar to `maybe3`, but reimplements the `maybe` function
* 5905818: reimplements `isNothing x` as `maybeToList x == []`, `isJust x` as `maybeToList x /= []` and `fromJust x` as `maybeToList x !! 0`. Nested if statements (`if x then if x then ...`)
* 5943272: reimplements `isJust` as `=/ Nothing`
* 5988179: reimplements `fromJust`

# Added

* Use `sort` on the parameters of commutative functions
* Rewrite `isNothing` as `not . isJust`
* Rewrite `if cond then True else False` as `cond`
* Rewrite `map head . transpose` as `head`
* Rewrite `concatMap (\x -> if cond x then [x] else [])` as `filter cond`
* Rewrite `Nothing == x` as `isNothing x` and `Nothing /= x` as `isJust x`
* Rewrite `notElem` as `not . elem`
* Rewrite `maybeToList x == []` as `isNothing x`
* Rewrite `maybeToList x /= []` as `isJust x`
* Rewrite `maybeToList x !! 0` as `fromJust x`
* Simplify tuple case when a variable is being rebound in all alts
* Activate base case removal
* Detect and abstract reimplementations of `filter`
* Apply `fromJust` (we benefit from pattern lifting, but there is no clear effect)
* Apply `maybe` (a whole solution becomes unnecessary)
* Abstract `fromMaybe`
* Apply `flip` by switching the order of the lambda parameters
* Remove last alternative of a case statement that leads to undefined (fixes 5988179)
* Remove nested if statements that check the same condition (fixes 5905818)

Would require analysis to properly remove maybe-related functions:
* 5630177: `maybe 0 id columnIndex` ==> `fromJust columnIndex`
* 5715938: `fromMaybe e x` ==> `fromJust x`
* 5941016:
* And many more
