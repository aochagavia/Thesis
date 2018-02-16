# Introduction

Ask-Elle [@2017askelle] is a programming tutor designed to help students learn the Haskell programming language. With Ask-Elle, teachers can specify exercises along with model solutions and students can solve them interactively. During the process, the system is able to check whether a student is on the right track and, in case they get stuck, it can provide relevant hints. Afterwards, it can check whether the provided solution is correct.

The exercises supported by Ask-Elle ask to implement functions. For instance, an exercise to teach basic concepts around lists and recursion could be to implement the `length` function.

One of the strengths of Ask-Elle is its ability to provide feedback and hints based only on a model solution. The teacher writes a solution for the exercise and Ask-Elle does the rest. This is very convenient, because it minimizes the work to set up the exercises.

## Program matching

In order to provide hints and check the correctness of student answers, Ask-Elle relies on program matching. That is, a mechanism to compare two programs and determine whether:

1. They are equal;
1. One is an incomplete version of the other;
1. Nothing can be concluded.

By using this technique, student answers can be compared to model solutions. In case a student answer turns out to be equal to the model solution, we know that the answer is correct. If it turns out to be an incomplete version of the model solution, we know that the student is on the right track and can offer them hints. If nothing can be concluded, Ask-Elle cannot offer any hints, but it will perform property-based testing so the student gets feedback regarding correctness.

At the core of program matching is the idea of normalization. Before comparing the programs, they go through a series of semantics-preserving transformations that result in a normal form. This way, comparing the programs becomes as simple as comparing the resulting normal forms for equality. In this comparison, there is an option to make holes match everything, so that incomplete programs can be matched to complete ones.

As an example, consider the function `double :: [Int] -> [Int]`, which doubles each element in a list of integers. The model solution, student answer and normalized version are shown below.

```haskell
-- Model solution
double = map (* 2)

-- Student answer
double = map (\x -> 2 * x)

-- Normalized version (similar for both)
double = map ((*) 2)
```

## Limitations

A typical use case for Ask-Elle is to aid teaching in an introductory course on functional programming. For this purpose, there are some limitations that stand out. We will discuss them below.

### No feedback regarding style

Right now, the only criterion followed by Ask-Elle when providing hints is whether the next step brings the program closer to one of the model solutions. On a fundamental level, Ask-Elle does not care about the beauty of the code, as normalization is blind to it.

In this light, the current approach to encourage a good programming style consists of enforcing it through well-chosen model solutions. If the style of a model solution is good, the hints will lead to a program with good style.

The aforementioned approach is limited, because it only provides hints when a student is already on the right track. However, when an incomplete program does not follow the style of the model solution, Ask-Elle is unable to come up with suggestions or hints. This happens because Ask-Elle can only reason about what needs to be *added* to a program, while style improvements often require *refactoring* a program.

In cases where a given anti-pattern is widespread between the students, it is possible to work around the previous limitation by adding a model solution that makes use of said anti-pattern. This way, a student can receive hints even when their solution does not adhere to the style guidelines. Still, this is hardly a viable option as it requires predicting all the style problems that a program may present.

Ask-Elle's blindness to style is an undesirable limitation, considering that teaching good style is an integral part of programming courses and that style is often an important factor in the grading. Lack of knowledge may lead beginners to write complex code, while simpler solutions exist. Style-oriented hints seem particularly useful for them, as they would help remove unnecessary complexity.

### Style-unaware normalization

Ask-Elle's normalization procedure is designed to handle only small syntactical differences. Two programs can only be transformed to the same normal form if they follow the same style. As we saw above, this causes problems when attempting to provide hints for a program that diverges too much from its model solution. Let us consider again the `double` function, this time implemented in a recursive way:

```haskell
-- Original program
double [] = []
double (x:xs) = (x * 2) : double xs

-- Normalized
double = \y1 -> case y1 of
    [] -> []
    (x1 : x2) -> (:) ((*) x1 2) (double x2)
```

It takes little time to see that this is basically a specialized reimplementation of map. Therefore it would be interesting to see it normalized as such:

```haskell
-- Desired normalization
double = map ((*) 2)
```

Considering that reimplementing map is a common anti-pattern among beginners, it is unfortunate that they cannot get any hints in the presence of this style issue. In this regard, the lack of style-aware transformations has a clear impact on the user experience.

## Proposed solution

On the long term, it seems worthwhile to enhance Ask-Elle with style-aware feedback and normalizations. Both features are closely related, as they both need to recognize the presence of style anti-patterns: the former, to report them; the latter, to normalize them. From the two, we think that normalization has a higher priority, since it enables giving hints in situations that were previously unsupported. We also expect most of the work on normalization to be reused when implementing style-aware feedback.

In light of the previous considerations, we propose to:

1. Identify the main style issues that prevent Ask-Elle from matching semantically equivalent programs;
1. Add new transformations to improve normalization of programs that present those issues;
1. Assess the improvements in program matching derived from the new transformations.

Below we describe our methodology.

### Data analysis

The steps described above require having a set of programs, which we can analyze to find style issues and which we can feed to Ask-Elle to assess the quality of program matching. In our investigation, we rely on a dataset of 111 correct submissions to the first assignment of the functional programming course at Universiteit Utrecht. The assignment requires implementing 8 functions, which translate to 8 Ask-Elle exercises. This means that we get 888 programs we can feed to Ask-Elle.

Measuring the quality of Ask-Elle's normalization is done on a per-exercise basis. For each exercise, we take the 111 submissions, normalize them and cluster them in groups that have the same normal form. Since all submissions are correct programs, an ideal normalization strategy should result in a few big clusters, which means that all similar programs have been transformed to the same normal form. This is equivalent to saying that Ask-Elle should be able to match many programs with a small number of model solutions. Because of this, we measure the amount of clusters per exercise and the size of each cluster.

The clustering information mentioned in the previous paragraph serves as a basis to:

1. Identify programs that form their own cluster because of style issues: from these programs we expect to infer transformations that would improve normalization;
1. Compare the quality of the normalization procedure across different versions of Ask-Elle: this is particularly useful when assessing the effect of adding a transformation.

A first measurement of the amount of clusters and their size can be found in [Chapter 3](#results).

### Adding transformations

The measurements described in the previous section focus only on the effect of a transformation, instead of its quality. From this perspective, there is no distinction between a transformation that is tailored to our dataset and one that is applicable to a broader range of programs. Since the goal of this project is to improve Ask-Elle's normalization in the general case, it is crucial to avoid ad-hoc transformations. For this purpose, we only consider adding a transformation when it is:

* Mentioned in the literature or;
* Recognized as general-purpose by a group of experts.
