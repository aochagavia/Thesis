# Current state of affairs

All exercises covered.

# Approaches to checking

Currently, there are two functions to check an exercise: `ideasCheckAndReport` and `normCheckAndReport`. Both take the same arguments and produce similar output. The main diference is that `checkAndReport` goes through the IDEAS framework, while `normalizeAndReport` does not. This results in the second being *much* (orders of magnitude) faster than the first. We *assume* that both produce the same results for all exercises, but we have been unable to test it because IDEAS runs into an infinite loop in exercise 3.

Additionally, there is a function similar to `normCheckAndReport`, called `normalizeAndGroup`, which identifies groups of submissions that normalize to the same code.

You can use the functions mentioned above as shown below:

```haskell
ideasCheckAndReport "../solutions/1" "../fp-practicals-ask-elle/2017-assignment1-lists/exercise1" imports1
normCheckAndReport "../solutions/1" "../fp-practicals-ask-elle/2017-assignment1-lists/exercise1" imports1
normalizeAndGroup "../solutions/1" "../fp-practicals-ask-elle/2017-assignment1-lists/exercise1" imports1
```

# Solved problems

We filter out type signatures after parsing Helium programs. This helps remove errors caused by users referring to unknown types (e.g. `Table`, `Row`, etc).

Some functions, such as `intercalate`, cause compile errors in Helium. This can be solved by adding them to the environment (see `Language.Compiler.Helium.ImportEnvs`). Of course, you also need to add them to `preludeFeedback` afterwards.

`NoType` error. Triggered when the name of the model function doesn't match the name of the exercise (e.g. if the exercise is `haskell.list.myreverse`, then the function must be `myreverse`).

# Observations

* In order for a function to be available to *Helium*, it needs to be specified in `Language.Compiler.Helium.ImportEnvs`, by providing the name and the type signature. Otherwise you get a compile error.
* In order for a function to be available to *Ask-Elle*, it needs to be listed in `preludeFeedback` (in `PreludeS.hs`). Otherwise, any submission using that function will not be matched.
* Two programs are considered equivalent if their normalised form is structurally equivalent

Stuff unsupported by Ask-Elle:
* List comprehensions
* Do notation

The answer functions must have the same name as the model function.

# Questions

## Wat doet de `ns` parameter in de `normalise` functie (Language.Haskell.Equality)?

> Dat is een lijstje met namen die niet hernoemd worden tijdens het alpha renamen. Dit zijn de namen van de functies die je van de student verwacht, bv. myreverse.

## Bij het checken of twee stukken code (zonder holes) equivalent zijn, gebruik ik normalise op beide en daarna vergelijk ik het resultaat. Mag dit eigenlijk?

Zeker, dat is wat de functie eqModule in de Equality module doet. We gebruiken het ideas raamwerk voor de strategieën die ons tussenliggende uitwerkingen geven. Kijk je enkel naar volledig uitgewerkte opgaven, dan heb je ideas idd niet nodig.
