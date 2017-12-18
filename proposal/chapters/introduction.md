# Introduction

## Ask-Elle

Ask-Elle [@2017askelle] is a programming tutor designed to help students learn the Haskell programming language. With Ask-Elle, teachers can specify exercises and ask students to solve them. During the process, the system is able to check whether the students are on the right track and, in case they get stuck, it can provide relevant hints. Afterwards, it can check whether the provided solution is correct.

The exercises supported by Ask-Elle consist of implementing functions. For instance, an exercise to teach basic concepts around lists and recursion could be to implement the `length` function.

One of the strengths of Ask-Elle is its ability to provide feedback and hints based only on a model solution. The teacher writes a solution for the exercise and Ask-Elle does the rest. This is very convenient, because it minimizes the work to set up the exercises.

Besides helping *students* learn Haskell, Ask-Elle also helps *teachers* grade assignments. Using the same model solution, submitted exercises can be automatically checked for correctness. As courses get bigger, this is an advantage that we cannot underestimate.

## Limitations

In the context of a course on functional programming for absolute beginners, Ask-Elle has two limitations that stand out. We will discuss them below.

### No consideration for style

Right now, the only criterion followed by Ask-Elle when providing hints is whether the next step brings the program closer to the model solution. This means that there is no feedback regarding style. Ask-Elle does not care about the beauty of the code, as long as the resulting function fulfills the specification.

This is a major limitation, considering that teaching good style is an integral part of programming courses and that style is often an important factor in the grading.

Furthermore, lack of knowledge may lead beginners to write complex code, while simpler solutions exist. Proper style feedback seems particularly useful for them, as it would detect unnecessary complexity and point them in the right direction.

### Style-dependent program matching

The core of Ask-Elle's ability to provide hints and check the correctness of student answers is program matching. That is, a mechanism to determine that two programs are equal, or that one is an incomplete version of the other.

Program matching is a complex process that we will not describe here. For our purposes, the important part is that the current algorithm is not always able to determine equivalence between two functions. For instance, it is currently unable to know that `if x == True then y else z` is equivalent to `if x then y else z`.

Whenever program matching fails, Ask-Elle is unable to provide hints and guide the user towards the solution. In such cases, an error message is shown instead: `You have drifted from the strategy in such a way that we can not help you any more`.

It turns out that failure in program matching is often triggered by bad style [FIXME: citation needed], a common problem among beginners. Fortunately, the problem is alleviated by performing property-based testing on the functions. Still, it is unfortunate that the user cannot get any hints because of their bad style.

## The solution

We propose extending Ask-Elle with a mechanism to reason about style. This should tackle the limitations described above. Applying style-related knowledge means that the matching algorithm can work in more cases and that style feedback can be shown to the user.

Not only should this result in better feedback whenever style issues arise. It should also result in a higher amount of programs that can be automatically checked by Ask-Elle.
