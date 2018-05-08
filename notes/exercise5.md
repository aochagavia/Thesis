Before:

58
24
10
4
-> 4 model solutions, 96 recognized (87%)

After: 1 model solution, 103 recognized (93%). One more model solutions, 107 (96%)

# Not fixed

Specialized reimplementation of map, with `[x]` as base case instead of `[]`:
* 5981778
* 5964962
* 5911486
* 5721156
Singleton groups (4):
* 5553865: monstruosity I don't understand
* 5850983: uses indexing, really horrible
* 5982065: guards instead of pattern matching, uses `zip` for some reason
* 6054846: uses `foldr` to get the maximum length of each column. Neat!

# Added

* `map f (map g xs)` to `map (f . g) xs`
* `transpose . map (map f)` to `map (map f) . transpose` (just to enforce an order)
* `\x -> x` to `id`
* `map id` to `id`
* `id x` to `x`
* `>>>` to `flip (.)`: unsound if `>>>` used on something else than functions, but Helium only accepts `>>>` in the context of functions anyway!
* Improved map heuristic to introduce lambda abstraction
* Activated empty case removal (for 5936608)

# Students using `>>>`

Using `>>>` and `map f (map g xs)` instead of `map (f . g) xs`:
* 6059929
* 5963761
* 5878713
* 5671833

* 6315100: uses `>>>`
