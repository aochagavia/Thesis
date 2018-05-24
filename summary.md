# Statistics

Exercise 1 (110 valid):

* Before: 85.5% (94) with 3 model solutions
* After: 99.1% (109) with 1 model solution

Exercise 2 (111 valid):

* Before: 36% (40) with 5 model solutions
* After: 80.2% (89) with 5 model solutions

Exercise 3 (103 valid):

* Before: 54.4% (56) with 4 model solutions
* After: 85.4% (88) with 3 model solutions

Exercise 4 (108 valid):

* Before: 61.1% (66) with 5 model solutions
* After: 90.7% (98) with 3 model solutions

Exercise 5 (111 valid):

* Before: 45% (50) with 4 model solutions
* After: 96.4% (107) with 2 model solutions

Exercise 6 (101 valid):

* Before: 7.9% (8) with 4 model solutions
* After: 86.1% (87) with 2 model solutions

Exercise 7 (99 valid):

* Before: 10.1% (10) with 3 model solutions
* After: 74.7% (74) with 5 model solutions

Exercise 8 (99 valid):

* Before: 23.2% (23) with 5 model solutions
* After: 87.9% (87) with 5 model solutions

# Common student problems

Confusion around preconditions (esp. exercise 3):

* Many students do not understand that it is unnecessary to check whether a precondition holds. Instead, they explicitly handle the case in which the precondition does not hold.
* Since the exercise specifications only cover the case in which the precondition holds, everyone does whatever seems reasonable in the error handling code. This results in programs doing different things, which means they cannot be unified.
* Even when students handle the error case in the same way, explicitly checking for the precondition is done in lots of different ways too, which would require using an advanced solver to unify robustly.

Reimplementing higher order functions:

* Many students solve the problems recursively instead of using higher-order functions
* Some of them do it wrong, so our heuristic cannot abstract the function

Confusion around `Maybe`:

* Lots of students using a combination of `isJust`, `fromJust` and `fromMaybe`
* Some of them use `fromMaybe` with a magic number for the default case, which they check afterwards in an if statement. In practice, this amounts to using `isJust` and `fromJust`.

# Transformations

## General

Bindings:

* Inlining
* Dead code removal

Fully apply functions:

* fst
* snd
* const
* id

Abstract functions:

* `\x -> x` ==> `id`

Apply laws to enforce an order:

* Sort arguments of commutative functions

## Pattern matching

Let simplification:

* Expand tuple decls: `(a, b) = (c, d)` ==> `a = c; b = d`

Pattern simplification:

* Remove nested as patterns (and rename invalidated bindings)
* Remove wildcard patterns bound to a name

Case simplification:

* Remove last alternative if it leads to undefined
* Remove unreachable alternatives (heuristic: all alts after a PVar)
* Remove unnecessary tuple matching: `case (a, b) of (x, _) -> e` ==> `case a of x -> e`
* Remove unnecessary tuple destructuring and reconstruction: `case e of (a, b) -> f (a, b)` ==> `case e of x -> f x`
* Rewrite case as let if there is only one alternative

Pattern lifting:

* Applied to irrefutable decls: `let (a, b) = e in ...`
* Applied to decls that crash if the match is invalid: `let [x] = e in ...`
* Applied only when the rhs of the decl is a variable
* Procedure: if the variable in the rhs comes from a pattern match, remove this decl and add the lhs to the original pattern
* Example: `case e of (x, y) -> let 42 = x in ...` ==> `case e of (x@42, y) -> ...`
* Note: causes problems in exercise 2 because of laziness (see submissions 5936608 and 5978831): head is used inside a `map`, but is lifted outside of it

## Flip

* Insert `flip` if it allows to eta reduce the expression
* Apply flip if all arguments are provided (3)
* Remove flip when it is applied to a commutative function
* Reorder the parameters of a lambda when flip is applied to it

## Lists

Empty list base case removal:

* In top-level functions, if specified by the exercise
* When map-like functions are used: `f [] = []; f xs = map g xs` ==> `f xs = map g xs` (empty_base_case, no auto)

List and string literal/pattern normalization:

* `[a , .. , z]` ==> `a : .. : z : []`
* `"a .. z"` ==> `'a' : .. : 'z' : []`

Rewrite list comprehensions in terms of higher-order functions:

* Algorithm: https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-420003.11

Fully apply functions:

* head
* tail
* `[x] ++ xs` ==> `x : xs` (singleton_concat)
* `[]  ++ xs` ==> `xs` (empty_append_1)
* `xs  ++ []` ==> `xs` (empty_append_2, no auto)

Partially evaluate functions:

* `map f (x : xs)` ==> `f x : map f xs` (partial_map)
* `concat (x : xs)` ==> `x ++ concat xs` (partial_concat)
* `concat []` ==> `[]` (empty_concat)

Abstract functions:

* Recursive case expression ==> `map`
* Recursive case expression ==> `zip`
* Recursive case expression ==> `filter`
* `take n (cycle [e])` ==> `replicate n e` (SKIP: too difficult to let Coq reason about infinite lists)
* `intercalate []` ==> `concat` (intercalate_concat, no auto)
* `foldr (++) []` ==> `concat` (foldr_append)
* `foldr (:)` ==> `flip (++)`(foldr_cons, no auto)
* `concatMap (\x -> if cond x then [x] else [])` ==> `filter cond` (concat_map_filter, no auto)
* `!! 0` ==> `head` (zero_index)
* `concat . map` ==> `concatMap` (concat_map)
* `map head . transpose` ==> `head` (SKIP: too difficult to implement transpose)
* `map id` ==> `id` (map_id, no auto)
* `foldr f d []` ==> `d` (foldr_empty)

Apply laws:

* `concatMap (\x -> [e])` ==> `map (\x -> e)` (e could be any expression, proved for [x] in concat_map_map, no auto)
* `concatMap f . map g` ==> `concatMap (f . g)` (concat_map_comp, no auto)
* `concatMap (flip (:) [])` ==> `id` (concat_map_flip, no auto)
* `concat (replicate x [e])` ==> `replicate x e` (similar situation as in concat_map_map, proved for [42] in concat_replicate, no auto)
* `map f (map g xs)` to `map (f . g) xs` (map_map_comp, no auto)
* `mapMaybe (\x -> fmap f e) xs` ==> `map f $ mapMaybe (\x -> e) xs` (similar situation as in concat_map_map, proved for `Some 42` in map_maybe_fmap, no auto)
* `mapMaybe (\x -> Just e)` ==> `map (\x -> e)` (similar situation as in concat_map_map, proved for 42 in map_maybe_just, no auto)
* `>>>` ==> `flip (.)` (SKIP: Coq doesn't have >>>, the implementation would be equal to the proof)

Apply laws to enforce an order:

* `(x : xs) ++ ys` ==> `x : (xs ++ ys)` (cons_append)
* `(xs ++ ys) ++ zs` ==> `xs ++ (ys ++ zs)` (append_append)
* `transpose . map (map f)` ==> `map (map f) . transpose` (SKIP: too difficult to implement transpose)

## Maybe

Fully apply functions:

* fromJust
* maybe

Abstract functions:

* Case expression ==> `fromMaybe`
* Case expression ==> `fmap`
* `Nothing == x` ==> `isNothing x` (is_nothing, no auto)
* `Nothing /= x` ==> `isJust x` (is_just, no auto)
* `maybeToList x == []` ==> `isNothing x` (maybe_to_list_1, no auto)
* `maybeToList x /= []` ==> `isJust x` (maybe_to_list_2, no auto)
* `maybeToList x !! 0` ==> `fromJust x` (maybe_to_list_3, no auto)

Apply laws:

* `isNothing` ==> `not . isJust` (not_is_just, no auto)

## Bool

Abstract functions:

* `(==) True` ==> `id` (true_id, destr_bool)
* `(==) False` ==> `not` (false_neg, destr_bool)
* `(&&) True` ==> `id` (and_id, destr_bool)
* `foldr (&&) True` ==> `and` (foldr_and)
* `and . map` ==> `all` (and_map, no auto)
* `all id` ==> `and` (all_id, no auto)

Simplify:

* `if cond then True else False` ==> `cond` (simplify_id, destr_bool)
* `if (not cond) then x else y` ==> `if cond then y else x` (switch_if_branches, destr_bool)
* Remove nested if statements that check the same condition
* Rewrite two guards as an if statement if they are in the form `e` and `not e`

Apply laws:

* `notElem` ==> `not . elem` (SKIP: Coq doesn't have elem or notElem, the implementation would be equal to the proof)
