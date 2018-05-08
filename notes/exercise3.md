# Before

Baseline: 4 groups, 56 / 108 (51.9%)
Before: 3 groups, 80 / 108 (74.1%)
After: 3 groups, 88 / 108 (81.5%)

# After

It is clear that people didn't understand what a precondition is... Lots of students check for it instead of assuming it correct! The specification doesn't mandate the semantics of the function when `n < length s`, so people come up with their own ideas (and mess up our normalization)

## Fixable

Probably too ad-hoc:
* 5887585: `elem False . map isDigit` instead of `not (all isDigit)`
* 5987857: reimplements `and f` as `foldl (\a x -> f x && a) True`

## Unfixable

Unnamed group (4): explicit checking for the case `x == length y`
* 6040063
* 6014542
* 5995256
* 5747929

Unnamed group (2): explicit checking `(n > length s)`
* 5640059
* 5632102

Unnamed group (2): explicit checking `(>=) (length x2) x1`
* 6318908
* 6303919

Explicit checking and variants thereof (singleton groups):
* 4292839: defensive programming, uses `replicate (max 0 (d - length s)) ' '` instead of assuming that `d >= length s`
* 5546354: overly complicated checking for `n > length c`, uses `and . map isDigit` instead of `all isDigit`
* 5847001: adds an additional guard checking whether `length s == n`
* 5850983: adds an additional guard checking whether `length s < n`
* 5954878: adds a guard checking whether `length s == n`. Reimplements `all` recursively
* 6229077: adds guard checking whether `n > length x`
* 6329616: adds guard checking whether `n > length s`

* 4020332: explicit checking for `n < length s`, impossible case
* 5978610: adds guard checking whether `length s > x`, impossible case
* 6310621: adds guard checking whether `length s > n`, impossible case

Incorrect:
* + 5553865: only checks whether the first char in the string is a digit (`isDigit (head a)`), instead of the whole string
* + 5721156: adds a base case matching on the empty list
* + 5723493: adds two base cases, one for 0 and one for the empty list
* + 5926874: capitalizes the string when the length of the string is equal to the width of the column (maybe a hack to capitalize headers?). Also other issues: guard checking whether `length s == n`. Uses `and . map isDigit` instead of `all isDigit`.
* + 5941016: adds a base case matching on the number 0
* + 5943272: prepends a `|` to all strings, probably as a hack to make Exercise 4 work
* + 5963761: adds guards checking whether `n > length s`, `n < length s` or otherwise
* + 6290639: adds a base case with an error for the empty string

## Improvements in normalization / style

Boolean-specific rewrite rules: `e == False` ==> `not e` etc

Rewrite `if not e then e1 else e2` to `if e then e2 else e1`

Two guards: `e` and `not (e)`, rewrite the second as `otherwise`

## The problem with explicit checking

We could (somehow) replace all occurrences of `length s > n` by `False`, because of the precondition. Sounds doable, but I have no clue regarding how...

We cannot do much when we find a check of the kind `length s < n`, because it is could also be the case that `length s == n`. If we could somehow merge two branches (`length s < n` and `length s == n` into `length s <= n`), then we could replace that by `True`. But this seems too ambitious.

Sometimes the only additional check is `length s == n`. Usually, the semantics of the program don't change if we remove this branch. But how to automatically prove it? Too ambitious, again?

## Conclusion

Working with preconditions confuses students. Besides, it means that a part of the program is unspecified. When a part of the program is unspecified, the students are allowed to write whatever they want in that part of the program, as long as it does not influence the behavior of the program when the precondition does hold. This means that program unification becomes impossible, unless you are somehow able to slice out the unspecified part (which brings its own problems).

Therefore, a conclusion here is to formulate exercises that don't assume preconditions and are fully specified for the domain of the function as accepted by the type system.
