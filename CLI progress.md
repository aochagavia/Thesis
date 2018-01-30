# Current state of affairs

Our `CLI.hs` code is able to analyze all student submissions. The reports show that there is probably something wrong with the current normalization code.

# Approaches to checking

Currently, there are two functions to check an exercise `normCheckAndReport` and `normalizeAndGroup`. Both take the same arguments, but produce different output. The main diference is that `normalizeAndGroup` also reports the existing groups of submissions that normalize to the same code.

You can use the functions mentioned above as shown below:

```haskell
normCheckAndReport "../solutions/1" "../fp-practicals-ask-elle/2017-assignment1-lists/exercise1" imports1
normalizeAndGroup "../solutions/1" "../fp-practicals-ask-elle/2017-assignment1-lists/exercise1" imports1
```

# IDEAS

Originally, we used the `IDEAS` framework to check an exercise. However, since we are only checking whether finished submissions correspond to the model solutions, it is faster and more reliable to compare them directly, without going through the ideas framework. In other words, using IDEAS is considerably slower and the integration with Ask-Elle is buggy. Therefore, we are no longer using it.

Some bugs:

* Checking submissions that contain type signatures leads to infinite loops
* Some files lead to infinite bugs

Other issues:

* Program matching fails whenever a function is used that is not registered in the `preludeFeedback` function (see `PreludeS.hs`). No error message is displayed, so you need to make sure all functions you need are listed
* The code to initialize an IDEAS exercise is quite involved

# Solved questions

## Wat doet de `ns` parameter in de `normalise` functie (Language.Haskell.Equality)?

> Dat is een lijstje met namen die niet hernoemd worden tijdens het alpha renamen. Dit zijn de namen van de functies die je van de student verwacht, bv. myreverse.

## Bij het checken of twee stukken code (zonder holes) equivalent zijn, gebruik ik normalise op beide en daarna vergelijk ik het resultaat. Mag dit eigenlijk?

Zeker, dat is wat de functie eqModule in de Equality module doet. We gebruiken het ideas raamwerk voor de strategieën die ons tussenliggende uitwerkingen geven. Kijk je enkel naar volledig uitgewerkte opgaven, dan heb je ideas idd niet nodig.
