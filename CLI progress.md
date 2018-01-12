# Current state of affairs

Exercises covered:
* 1
* 2 (in progress):
    * Added `intercalate` to the environment (see `Language.Compiler.Helium.ImportEnvs`) and to `preludeFeedback` as we did with `words`
    * First attempt results in 23 matches, 87 unrecognized and 1 compiler error

You can generate a report for each exercise with the following function call:

```haskell
checkAndReport "../fp-practicals-ask-elle/solutions/1" "../fp-practicals-ask-elle/2017-assignment1-lists/exercise1"
```

Another useful function:

```
printExerciseSteps "mySum = foldr (+) 0"
```

# Problems

`NoType` error. Triggered when the name of the model function doesn't match the name of the exercise (e.g. if the exercise is `haskell.list.myreverse`, then the function must be `myreverse`).

Fixed bugs:
* When loading a piece of code that includes a type signature. Resolved by filtering out type signatures in `View.hs`.

# Observations

* The `lib/Prelude.hs` file seems to be unused. I removed it and nothing stopped working (I restored it later, since I don't want to change things unless they truly get in my way). On the other hand, it seems like `ImportEnvs.hs` has been generated from `Prelude.hs`... But I cannot find the generating code anywhere.
* In order for a function to be available to Helium, it needs to be specified in `Language.Compiler.Helium.ImportEnvs`, by providing the name and the type signature. Otherwise you get a compile error.
* Ask-Elle only accepts functions that are listed in `preludeFeedback` (in `PreludeS.hs`). Otherwise, any submission using that function will not be matched.

Stuff unsupported by Ask-Elle:
* List comprehensions
* Do notation

The answer functions must have the same name as the model function.

Diagnose results interesting for us:
* Equivalent: match
* Correct: no match + passes all tests
* NotEquivalent: no match + counterexample
