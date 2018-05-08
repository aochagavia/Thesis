Before

71
9
7
=> 78% with 3 model solutions

After

72
14
8
=> 85% with 3 model solutions

# Not fixed

Unfixable singleton groups:
* 5542731: uses `foldr (++) "|"` as a complicated way to concat
* 5640059: recursive with `[x]` as base case
* 5723493: Doesn't unify with the `foldr` solution because it uses point free style in the lambda
* 5744652: too creative... Uses `concatMap`, mixing the functionality of `map` and `intercalate` in one single lambda.
* 5871476: foldl. Looks good.
* 5878713: goes too far with point-free style. Maybe could be unified if we rewrote `(>>>)` as `flip (.)`. Problem: this transformation is only valid if we know that `>>>` is being applied to functions. Requires type information. Don't have that in the Ask-Elle representation.
* 5995442: implements a function like `intercalate`, but which adds the element at the front and at the back.
* 6290655: mixes guards and pattern matching. Base case is `[x]`

Fixable but ad-hoc?
* 6290639: similar to `map1`, but without `uncurry` (uses `let` to destructure the tuple)

Don't match the specification:
* + 4240537: missing a pipe at the beginning and end of the string
* + 4302907: adds a "\n" at the end of the string
* + 5943272: missing a pipe at the beginning

# After

Done:
* `concat [a, b, c]` becomes `a ++ b ++ c`
* Detect usage of `fst` and `snd`, introduce pattern matching instead
* Remove pointless deconstruction and reconstruction of tuples (only when it doesn't change the semantics of the matching, that is, for tuples that only bind variables)
* Push irrefutable tuple destructuring upwards when possible

Interesting, fixed:
* 5659507, 5897548: superfluous tuple destructuring and reconstruction
* 5982065, 6059929: nested pattern matching

Note: activated base case removal for `[]`

Note: switching from pattern matching to a PVar increases lazyness. Switching back removes lazyness, which can change the semantics of the program.

```
case (undefined, 42) of
    (xs, _) -> let (a, b) = xs
               in f a b

case (undefined, 42) of
    ((a, b), _) -> f a b
```

We could add a global remark to the thesis, saying that the lazyness changes depending on the transformations. Give some examples, explain why this is not a problem (our target audience doesn't care)

Consider adding exercise-specific stuff. (Look at the two ad-hoc transformations in exercise 3)
