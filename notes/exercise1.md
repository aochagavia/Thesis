# Cleanup

Cleanup steps:
* Remove all branches that use `error` (i.e. when an empty list is passed)

# Match against model solutions

Amount of model solutions: 3

Run the from GHCi

```
checkAndReport "../fp-practicals-ask-elle/solutions/1" "../fp-practicals-ask-elle/2017-assignment1-lists/exercise1"
```

Matched: 98
Rejected: 13
* 6318908: uses where
* 5984394: monstruosity
* 5982065: reimplements map (horribly)
* 5972868: reimplements map (horribly)
* 5938600: reimplements map
* 5933439: superfluous base case
* 5931126: reimplements words
* 5897548: uses where
* 5893348: (buggy) reimplementation of map
* 5723493: superfluous base case
* 5553865: superfluous base case
* 5546354: reimplements map
* 4020332: superfluous base case

# Normalizing

Normalizing all submissions gives a set of 13 elements. This means that adding 10 solutions would give us full coverage.

# Feasible improvements

The improvements below should help us recognize some extra submissions
* Better where inlining
* Detect reimplementations of map
* Detect superfluous base cases
