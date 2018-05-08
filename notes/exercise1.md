# Before

Three big groups:

* `parseTable = map words`
* `parseTable xs = map words xs`
* `parseTable xs = [words x | x <- xs]`

Matched to big groups: 94
Others: 16
* 6290639: uses error
* 6290655: uses error
* 6318908: uses where
* 5897548: uses where
* 5933439: superfluous base case (map)
* 5723493: superfluous base case (map)
* 5553865: superfluous base case (map)
* 4020332: superfluous base case (list comprehension)
* 5845866: contains dead code
* 5911486: contains dead code
* 5982065: reimplements map (horribly)
* 5972868: reimplements map (horribly)
* 5938600: reimplements map
* 5546354: reimplements map
* 5984394: reimplements map (horribly, using head and tail instead of pattern matching)
* 5893348: kind of reimplementation of map (returns [[]] in the empty case)

Semantically different:
* 5931126: reimplements words using a function from an external package

# After

Two big groups:

* `parseTable = map words`
* `parseTable xs = [words x | x <- xs]`

Matched to big groups: 109
Others: 2
* 5893348: kind of reimplementation of map (returns [[]] in the empty case)

Wrong:
* + 5931126: reimplements words using a function from an external package
