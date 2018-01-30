Progress
========

# Write proposal

Aim of our research: improve the ability of Ask-Elle to recognize user submissions

Aim (technical):
* The motor behind the ability of Ask-Elle to recognize submissions is through program transformations
* Model solutions and user submissions are transformed into some kind of normal form, which is then compared
* A submission is considered to match the solution whenever its normal form is syntactically equivalent to that of the model solution
* By improving the normalization process we expect to increase recognition rate

How?
* Review existing transformations
* Add style-based transformations
* Add slicing

Measuring:
* We measure the reduction in the amount of normalized groups per exercise

Interesting use cases:
* Style-related feedback, so students get hints related to style
* Automatic grading (or computer-assisted grading)

Current situation:
* Buggy normalization
* Even when fixed, it is quite basic

# Interesting refactorings

Identify reimplementations of common functions:

* map
* foldr

Remove branches containing calls to `error`

Take HLint output and apply it automatically?

# Strategy

Some exercises have tons of normalized groups. It makes sense to start working on the simple ones, in the hopes that that will result in transformations that will have an effect on the more complex ones. That way, when we are finished with the simple functions, we can go to the complex ones and have to deal with less normalized groups.

# Additional features

Keeping a trace of the transformations that have been applied

# Interesting ideas for some day
* Ensure that everything integrates well with IDEAS
* Proper support for list comprehensions (add them to the AST)

# Done

Data cleanup:
* Make broken programs compile again
* Work together with Alejandro to restore programs when information is missing

Ask-Elle (PR):
* Hacky support for list comprehensions (works for checking, not for hinting)
* Support for specifying additional imports for each exercise

CLI:
* Able to check all exercises
* Debugging function to take the set of all normalized files in a directory
* Debugging function to check all submissions against the model solutions and report the results

# Done, but no longer in use

Data cleanup:
* Remove all branches using `error`

CLI:
* Debugging function to show the refinement steps from `?` to the end goal (greedily takes the first step every time)
* Exercise checking by using IDEAS
