# Introduction

## Ask-Elle

Ask-Elle [@2017askelle] is a programming tutor designed to help students learn the Haskell programming language. With Ask-Elle, teachers can specify exercises and students can solve them interactively. During the process, the system is able to check whether the students are on the right track and, in case they get stuck, it can provide relevant hints. Afterwards, it can check whether the provided solution is correct.

The exercises supported by Ask-Elle consist of implementing functions. For instance, an exercise to teach basic concepts around lists and recursion could be to implement the `length` function.

One of the strengths of Ask-Elle is its ability to provide feedback and hints based only on a model solution. The teacher writes a solution for the exercise and Ask-Elle does the rest. This is very convenient, because it minimizes the work to set up the exercises.

Besides helping *students* learn Haskell, Ask-Elle also helps *teachers* grade assignments. Using the same model solution, submitted exercises can be automatically checked for correctness. As courses get bigger, this is an advantage that we cannot underestimate.

## Limitations

A typical use case for Ask-Elle is to aid teaching in an introductory course on functional programming. On this regard, there are two limitations that stand out. We will discuss them below.

### No feedback regarding style

Right now, the only criterion followed by Ask-Elle when providing hints is whether the next step brings the program closer to one of the model solutions. On a fundamental level, Ask-Elle does not care about the beauty of the code.

In this light, the current approach to encourage a good programming style consists of enforcing it through model solutions. If the style of the solutions is good, the hints will lead to a program with good style.

The aforementioned approach is limited, because it only provides hints when the student is already on the right track. Only when an incomplete program adheres to the style of the model solution, Ask-Elle can suggest which code should be written next.

However, when the incomplete program does not follow the style of the model solution, Ask-Elle is unable to come up with suggestions or hints. This happens because Ask-Elle can only reason about what needs to be *added* to a program, while style improvements often require *refactoring* a program.

This is an undesirable limitation, considering that teaching good style is an integral part of programming courses and that style is often an important factor in the grading. Lack of knowledge may lead beginners to write complex code, while simpler solutions exist. Style-oriented hints seem particularly useful for them, as they would help remove unnecessary complexity.

### Style-dependent program matching

The core of Ask-Elle's ability to provide hints and check the correctness of student answers is program matching. That is, a mechanism to determine that two programs are equal, or that one is an incomplete version of the other.

Program matching is a complex process that we will not describe here. For our purposes, the relevant part is that the current algorithm is not always able to determine equivalence between two functions or expressions. For instance, it is currently unable to know that `if x == True then y else z` is equivalent to `if x then y else z`.

Whenever program matching fails, Ask-Elle is unable to provide hints and guide the user towards the solution. In such cases, an error message is shown instead: `You have drifted from the strategy in such a way that we can not help you any more`.

It turns out that failure in program matching is often triggered by bad style, a common problem among beginners (see Chapter [3](#results) for details). While the problem is somewhat alleviated by performing property-based testing on the functions, it is unfortunate that the user cannot get any hints because of style issues.

## The solution

We propose extending Ask-Elle with a mechanism to reason about style. This should allow Ask-Elle to produce style-aware hints and to increase the success rate of program matching.

TODO: how are we actually going to do this?

As far as I know, limitation 1 is kind of binary. If your style is decent, you get feedback. If your style is bad, you don't. There is no gradual quality decrease. In the first case, making Ask-Elle style-aware has no benefit. In the second, it allows showing feedback in situations where there used to be none. Therefore we cannot measure the improvement in feedback quality. However, we can measure the improvement in the amount of cases when the student can get feedback. That is, the difference in the amount of failed program matching before and after introducing this feature.

Regarding limitation 2, we can measure the improvement in program matching by testing against the set of submissions from assignment one.
