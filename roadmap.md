Progress
========

# Cleanup data:

* What to do about non-compiling programs? Right now we are ignoring them. Could we delete them?
* Cleanup exercise 1 and others, if necessary (e.g. remove uses of `error`)

# Identify possible refactorings to bring down the size of the sets

* How to proceed? There are lots of files and lots of normalized groups

# Proposal (old):
* Is it realistic to aim for recognition + feedback? Askelle seems a bit immature. Maybe we need to aim less high?
* Idea: automatic grading FP assignments, including style feedback? If that succeeds, we can go further. On the other hand... Why not just use HLint instead of making things so complicated?
* Come up with model solutions and generally accepted refactorings beforehand
* Measure the difference between the amount of recognized programs, before and after the refactorings
* If time allows, produce hints that can be reported to users

# Interesting ideas for some day
* Try to make my fork compatible with the master branch
* Get IDEAS working and ensure that the output is the same as the output of norm
* Proper support for list comprehensions (add them to the AST). This way we get to know the guts of Ask-Elle
* Support do notation? Should be quite similar to list comprehensions
* Check out the suggested refactorings from HLint

# Done
* Debugging function to show the refinement steps from `?` to the end goal (greedily takes the first step every time)
* Debugging function to take the set of all normalized files in a directory
* Ask-Elle able to run on exercises: 1...2
* Pipeline to check all submissions against the model solutions and report the results
* Hacky support for list comprehensions (works for checking, not for hinting)
