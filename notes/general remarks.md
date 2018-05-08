# Preconditions

It is not practical to design exercises involving preconditions (e.g. argument `xs` is never the empty list), as the students tend to ignore this and add code to handle the case in which the precondition does not hold.

This causes lots of problems, because there is no specification for the case in which the precondition does not hold. This means that any code is valid. Therefore, the only way to unify such programs is to slice out the part of the code that is ruled out by the specification.

We are doing this in the simple case of the empty list, but for complex cases there is no easy way to tackle the problem.

In the future, programming assignments should be fully specified instead of relying on preconditions holding.

Exercise 3 is particularly problematic in this regard, because the precondition is more complex than "`xs` is not empty".

# Pattern match lifting

Note that this has consequences regarding lazyness!

```
-- This won't crash
case e of
  xs -> map (* (head xs)) []
```

```
-- This will crash
case e of
  xs@(headVar, _) -> map (* headVar) []

```

This refactoring is right now the most interesting one we have found

# Candidates for exercise specific refactorings

Ex3
Ex8?

# Transformations

Rewrite

Apply

Abstract

# Apply vs abstract

When to use which?

# Specification leaves space for too much creativity

What should happen if the input parameter is the empty list?

Students follow their own judgement and come up with wildly different solutions

# Testing

First:
* Can we compile all programs after pretty printing them?

We could specify QuickCheck properties for all exercises, and test the normalized versions to verify that the transformations preserve the semantics.

Problem: not all answers satisfy the specification, therefore we need to filter out some of them. That is, skip all programs where the original doesn't pass the tests.

Good to validate!
* Found new bugs that had been hidden
* Found out that one of the model solutions in Ex8 actually has different semantics!
* Pattern lifting breaks programs! See Ex2, 5936608, where `head` is used after matching in an if statement, but our lifting algorithm extracts it. Idea: modify the algorithm to treat if statements as boundaries?
