# Some background

Existing technology. Tell more about Ask-Elle and related things? [@2017askelle]

## Program matching

One of the core components of Ask-Elle is a solver [FIXME: is this the right term?] that compares functions in order to determine whether they are equivalent. This is tricky business, as automatically comparing programs in general amounts to solving the halting problem.

Not only must the solver be able to say whether two programs are equivalent. It also must be able to say how far the user is from the solution, so the appropriate hints can be given.

Ask-Elle tackles the problem by simplifying the ASTs through semantics-preserving transformations. Afterwards, the resulting ASTs are compared. Programs are considered equivalent only when their simplified AST is structurally equal.

### When program equivalence breaks

Because of the fundamental limitations mentioned above, the solver is not always able to establish the equivalence between functions. This can happen when the user has provided an incomplete implementation of the function, but also when a complete implementation is provided.

If the user is still not finished writing the function and the solver fails to identify it as a partial solution, then Ask-Elle does not know how to check it or provide hints. In such cases, the following message is shown:

```
You have drifted from the strategy in such a way that we can not help you any more.
```

If the function is finished and the solver fails to identify it as a full solution, then Ask-Elle will resort to property-based testing. While this is not ideal, it gives a high degree of confidence in the correctness of the solution.

### How it works in practice

We have seen the limitations of the solver, but how does it work in practice? [FIXME: statistics from the FP course].

How many programs are found to be equivalent to the solution?

How many programs are not recognized as equivalent, but pass the tests?

It is probably difficult to ask previous questions for incomplete programs, as they cannot be tested (and if they can, it is not very useful, right?)...

Say something about it.
