# Preliminary results {#results}

Note: use a bubble chart!

Other ideas: pie chart with ranges (singleton, 2, < 4, < 8, < 16, < 32, < 64)

I like the pie chart with ranges!

8 pie charts in one page vs 4 in two pages

Need to figure out how to generate them from r

## Modifications to Ask-Elle

Imports

List comprehensions

## Establishing a baseline

Exercise number  Groups Big groups Small groups Singleton groups
---------------  ------ ---------- ------------ ----------------
1                12     1          1            1
2                123    2          1            1
3                1000   3          1            1
----------------------------------------------------------------

## Some interesting transformations to look at

By looking at the first exercise...

Superfluous base cases (matching on empty list when using map)

Sort argument when applying commutative functions (i.e. `(*) 3 2` always becomes `(*) 2 3`):
* Define and enforce an ordering on expressions
* Question. Is this really necessary for our exercises? It seems easy to implement... Maybe still interesting to begin with

Identify reimplementations of common functions:

* map
* foldr

Remove branches containing calls to `error`

We expect to discover additional transformations during our research, as we will have time to analyze the rest of the exercises.

## Normalization bugs

Any bugs there? Probably...
