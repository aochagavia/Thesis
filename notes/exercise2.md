# Before

5 model solutions, 40 submissions covered, 36.04%

Big groups (>= 4 elements):

```
-- 20
printLine [] = "+"
printLine (x:xs) = "+" ++ replicate x '-' ++ printLine xs
```

```
-- 7
printLine x = "+" ++ intercalate "+" (map (flip replicate '-' ) x) ++ "+"
```

```
-- 5
printLine [] = "+"
printLine (x:xs) = "+" ++ concat (replicate x "-") ++ printLine xs
```

```
-- 4
printLine []     = ['+']
printLine (x:xs) = ['+'] ++ replicate x '-' ++ printLine xs
```

```
-- 4
printLine = foldr addField "+"
    where
        addField x str = "+" ++ replicate x '-' ++ str
```

There are 44 submissions that use `foldr`... From the set of normalizations, the most frequent one containing foldr appears only 3 times!

# After

Recursive: 39
Foldr: 22
Intercalate: 18
concatMap: 6

4 model solutions, 85 submissions covered, 76.58%

## Lambda unification issue

There are 6 `foldr` answers that don't unify with the big group:
* `(++) . (\ x -> "+" ++ replicate x '-')` vs `(\x xs -> '+' : replicate x '-' ++ xs)`

foldr  - `(\ x1 -> (\ x2 -> (:) '+' ((++) (replicate x1 '-') x2)))`
foldr2 - `(\ x1 -> (++) ((:) '+' (replicate x1 '-')))`

We have already added an eta reduction rule that applies flip. Adding another one for this case seems too ad-hoc:
* Current rule: `\x -> replicate x "+"` ==> `flip replicate "+"`
* Additional rule: `\xs -> (:) '+' ((++) ys xs)` ==> `(++) ((:) '+' ys)`

* 6323499
* 6014542
* 5750032
* 5723493
* 5500206
* 4157893

## Other issues

Recursion with code repetition (2 cases):
* 5640059: Uses `[x]` as base case instead of `[]`.
* 6290655: Uses `[x]` as base case instead of `[]`. Also, uses guards instead of pattern matching.

Recursion with 3 cases (matches on `[]`, `[xs]` and `x:xs`):
* 5747929
* 5893348
* Note: the case for `[]` is actually unreachable, but our heuristic doesn't see that because it doesn't handle recursive functions.

Logic split into `foldr` and `map`:
* 5995442
* 6323472: Also, fold lambda uses `concat ['+', xs, ys]` instead of `'+' : xs ++ ys`

Uses `'+' : concatMap ...` instead of `concatMap ...`:
* 6290639
* 5744652
* 5715938

Other:
* 4240537: `foldr f "" l ++ "+"` instead of `foldr f "+" l`
* 4020332, 6303919: `'+' : foldr ...` instead of `foldr ...`
* 5871476, 5630177: foldl
* 5850983: list comprehension with list indexing
* 6059929: irrelevant base case in foldr function
* 5910137: reimplements replicate from scratch
* 5750865: reimplements `concat . (++ "+")` as `filter (/= " ") . (\xs -> unwords xs ++ "+")`

Wrong when input is `[1]`. Could be a bug...
* 5936608, 5978831: guards instead of pattern matching on the empty list.

Here is a case where pattern lifting breaks a program!!!

## Better normalization

Rewrite string literals as list literals (up to length 1)

Split tuples in lets

    let (x, ..., y) = (a, ..., b) in e
    -- becomes
    let x = a
        ... = ...
        y = b
    in e

Enforce an order whenever `(:)` is used together with `(++)`: `(x : xs) ++ ys` becomes `x : (xs ++ ys)`

When eta-reducing, check whether flipping the function would allow reduction: `(\x -> f x e)` becomes `flip f e`.

removeErrorAlts replaced by removeIrrelevantCase. Instead of removing cases when `error` is used, we move `[]` cases of non-recursive functions.

## Style-related

Reimplementations of functions:
* `take x . cycle [e]` becomes `replicate x e`
* `intercalate []` becomes `concat`
* `foldr (++) ys` becomes `\xs -> concat xs ++ ys` (too ad-hoc?) YES
* `foldr (:)` becomes `flip (++)`

Other style improvements:
* `concatMap (\x -> [e])` becomes `map (\x -> e)`
* `concatMap f . map g` becomes `concatMap (f . g)`
* `concat (replicate x [e])` becomes `replicate x e`
* `concat (x : xs)` becomes `x ++ concat xs`
* `concat . map` becomes `concatMap`

## Comments

List comprehensions are normalized away, since they use `concatMap` under the hood. Exercise 1 now has one huge group instead of two.

Still using conversion from `case` to `let`. Necessary for tuple splitting
