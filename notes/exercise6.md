Before:

42
11
7
4
4
4
-> 72 (70%)

Note: Only 101 correct submissions

After:
69
18
-> 87 (85%)

# The exercise

line
header (printRow and uppercase everything)
line
body (printRow for each row)
line

# Unfixable

Singleton groups:
* 5553865: extra code to handle the case of only a header and no rows
* 5640059: reimplements `(++) (map (\x -> f x))` as `foldr (\x -> (:) (f x))` or something like that
* 5707617: instead of using `printLine`, reimplements it with the name `horizontalBorder`, that takes a `Table` as argument (instead of `[Int]`)
* 5850983: uses indexing
* 6059929: reimplements zip (buggy, but correct if you assume that the lists are of equal length, which holds in this case), reimplements map with 2 args, but the empty case adds an element
* 6229077: adds useless base cases for `[x]` and `[x, [[]] ]`
* 6290655: reimplements map with guards instead of matching and `[x]` as the base case
* 6310621: maps after zipping, but before printing the row

Same problem, but still singleton groups:
* 5747929: reimplements map, but the empty case adds an element
* 5878713: reimplements map, but the empty case adds an element. First `printRow`, then `map toUpper`.
* 5982065: reimplements map, but the empty case adds an element. First `map (map toUpper)`, then `printRow`.

Same problem, but still singleton groups:
* 5750865: maps the whole table instead of only the rows
* 5974402: maps the whole table instead of only the rows
* 6323375: maps the whole table instead of only the rows

Wrong, no `toUpper`
* + 5963761 (there is a `headerUp` function, but it is not called!)
* + 5931126
* + 6014542

Wrong:
* + 3921387: calls `columnWidth` instead of `columnWidths`. It unifies once you change that.
* + 4240537: his `printRow` function does not have starting and ending `|`, so he adds them here
* + 4302907: uses `concatMap` on `printRow`, which joins all rows in a single string instead of returning them as separate strings
* + 5495792: returns a singleton list containing all lines intercalated by `\n`
* + 5907985: calls `getColumnWidths` instead of `columnWidths`. It unifies once you change that.
* + 5941016: prints a transposed table
* + 6323367: the header and its surrounding lines are all in the same string, separated by newlines (they should be separate strings, as we are returning a `[String]`)

# Model solutions

* One: first `toUpper`, then `printRow`
* Two: first `printRow`, then `toUpper`

# Added

Activated base case removal

* `map f [x]` ==> `[f x]`
* `[a , .. , z]` ==> `a : .. : [z]`
* Dead `as` removal (works, but didn't get us more coverage)
* Improved map heuristic, now supporting reimplementations with multiple args. See 4257383, 5905818 for an example
* Basic recognition of reimplementations of `zip`
* Improvements to tuple lifting, to replace ad-hoc rules for `fst` and `snd`
* Arbitrary pattern lifting (as long as it is matched in a let), to replace ad-hoc rules for `head` and `tail`
* Apply `fst`, `snd`, `head` and `tail`. Instead of having ad-hoc lifting rules for them, let the pattern match lifting stuff do the job.
