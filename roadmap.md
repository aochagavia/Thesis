Progress
========

# Now:
* Create a function that normalizes all files in a directory (look at `isPredecesor` in `DeepDiagnose.hs`)
* Create a set with the resulting normalized programs, paired to the file names
* This gives us a minimal set of model solutions
* How big is the set? Do they look like each other?
* Which refactorings could we use to reduce the size of the set?
* Maybe we should clean up a bit (remove the code that uses "error" and such)

# Next:
* Proper support for list comprehensions (add them to the AST). This way we get to know the guts of Ask-Elle
* Get the other exercises working: against a model solution, generating a model solution set
* Support do notation? Should be quite similar to list comprehensions

# Proposal (old):
* Come up with model solutions and generally accepted refactorings beforehand
* Measure the difference between the amount of recognized programs, before and after the refactorings
* If time allows, produce hints that can be reported to users

# Interesting ideas for some day
* Check out the suggested refactorings from HLint

# Done
* Debugging function to show the refinement steps from `?` to the end goal (greedily takes the first step every time)
* Ask-Elle able to run on exercises: 1...2
* Pipeline to check all submissions against the model solutions and report the results
* Hacky support for list comprehensions (works for checking, not for hinting)
