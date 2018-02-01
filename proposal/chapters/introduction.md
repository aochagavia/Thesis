# Introduction

## Ask-Elle

Ask-Elle [@2017askelle] is a programming tutor designed to help students learn the Haskell programming language. With Ask-Elle, teachers can specify exercises and students can solve them interactively. During the process, the system is able to check whether the students are on the right track and, in case they get stuck, it can provide relevant hints. Afterwards, it can check whether the provided solution is correct.

The exercises supported by Ask-Elle consist of implementing functions. For instance, an exercise to teach basic concepts around lists and recursion could be to implement the `length` function.

One of the strengths of Ask-Elle is its ability to provide feedback and hints based only on a model solution. The teacher writes a solution for the exercise and Ask-Elle does the rest. This is very convenient, because it minimizes the work to set up the exercises.

## Program matching

In order to provide hints and check the correctness of student answers, Ask-Elle relies on program matching. That is, a mechanism to compare two programs and determine whether:

1. They are equal;
1. One is an incomplete version of the other;
1. Nothing can be concluded.

By using this technique, student answers can be compared to model solutions. In case a student answer turns out to be equal to the model solution, we know that the answer is correct. If it turns out to be an incomplete version of the model solution, we know that the student is on the right track and can therefore offer hints. If nothing can be concluded, Ask-Elle cannot offer any hints, but it will perform property-based testing instead so the student gets feedback regarding correctness.

At the core of program matching is the idea of normalization. Before comparing the programs, they go through a series of semantics-preserving transformations that result in a normal form. This way, comparing the programs becomes as simple as comparing the normal forms for equality. In this comparison, there is an option to make holes match everything, in order to enable incomplete programs to be matched to complete ones.

As an example, consider the function `double :: [Int] -> [Int]`, which doubles each element in a list of integers. The implementations of the model solution, student answer and normalized version are shown below.

```haskell
-- Model solution
double = map (* 2)

-- Student answer
double = map (\x -> 2 * x)

-- Normalized version (similar for both)
double = map (*) 2
```

## Limitations

A typical use case for Ask-Elle is to aid teaching in an introductory course on functional programming. On this regard, there are some limitations that stand out. We will discuss them below.

### No feedback regarding style

Right now, the only criterion followed by Ask-Elle when providing hints is whether the next step brings the program closer to one of the model solutions. On a fundamental level, Ask-Elle does not care about the beauty of the code.

In this light, the current approach to encourage a good programming style consists of enforcing it through model solutions. If the style of the solutions is good, the hints will lead to a program with good style.

The aforementioned approach is limited, because it only provides hints when the student is already on the right track. Only when an incomplete program adheres to the style of the model solution, Ask-Elle can suggest which code should be written next.

However, when the incomplete program does not follow the style of the model solution, Ask-Elle is unable to come up with suggestions or hints. This happens because Ask-Elle can only reason about what needs to be *added* to a program, while style improvements often require *refactoring* a program.

This is an undesirable limitation, considering that teaching good style is an integral part of programming courses and that style is often an important factor in the grading. Lack of knowledge may lead beginners to write complex code, while simpler solutions exist. Style-oriented hints seem particularly useful for them, as they would help remove unnecessary complexity.

### Insufficient normalization

The normalization procedure used by Ask-Elle is not yet very robust. Because of this, Ask-Elle is often unable to match two functions that are clearly identical. Take for instance a new version of the `double` function mentioned before:

```haskell
-- Original program
double = map (\x -> x * 2)

-- Normalized
double = map (\x -> (*) x 2)

-- Desired normalization
double = map (*) 2
```

In this example, the desired normalization involves performing eta reduction. However, eta reduction in this case requires reordering the arguments from `(*) x 2` to `(*) 2 x`. This is perfectly fine, because `(*)` is commutative, but Ask-Elle is unable to leverage that information.

Besides insufficient transformations to match programs that are clearly equivalent, Ask-Elle lacks transformations that enable matching more complex programs. For instance, it is unable remove superfluous pattern matching:

```haskell
-- Original program
double [] = []
double xs = map (* 2) xs

-- Normalized
double = \y1 -> case y1 of
                    [] -> []
                    x1 -> map (*) 2 x1

-- Desired normalization
double = map (*) 2
```

Considering that superfluous pattern matching is a common anti-pattern among beginners, it is unfortunate that the user cannot get any hints because of these style issues. In this regard, the lack of complex transformations has a clear impact in the user experience.

## Proposed solution

In the light of the limitations mentioned above, we propose to improve Ask-Elle's normalization procedure so more programs can be matched, including those that present anti-patterns. Below we describe our approach.

### Data analysis

The first step in our research consists of analyzing a set of student programs. From this analysis, we expect to identify the main style and normalization issues that prevent Ask-Elle from doing its job.

Our dataset consists of 111 correct submissions to the first assignment of the functional programming course at Universiteit Utrecht. The assignment requires implementing 8 functions, which translate to 8 Ask-Elle exercises. In total, we get 888 programs we can feed to Ask-Elle.

### Adding new transformations

Based on the analysis mentioned above, we intend to come up with new transformations that improve Ask-Elle's ability to match programs. In this stage, we will focus on finding transformations that help in two fronts:

* Make normalization behave as expected in simple cases.
* Improve normalization of programs with style issues.

Besides discovering interesting transformations, it will be necessary to figure out how they fit in Ask-Elle's architecture and implement them.

### Measuring

Measuring the quality of Ask-Elle's normalization is done on a per-exercise basis. For each exercise, we will take the 111 submissions, normalize them and cluster them.

Since all submissions are correct programs, an ideal normalization strategy should result in a few big clusters, which means that all similar programs have been transformed to the same normal form. Because of this, we are going to measure the amount of clusters per exercise and their size.

With this information, we can compare the state of Ask-Elle's normalization at two different points in time. We only need to generate the clusters for each exercise and then compare the results. We expect our research to contribute to a lower amount of clusters while increasing their size.
