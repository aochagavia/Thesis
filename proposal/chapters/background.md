# Related work

On a fundamental level, our research is aimed at improving program classification. Our goal is to identify groups of programs that are similar to each other, despite their syntactical differences. A key element in this process is program analysis, which in our case makes heavy use of normalization techniques.

Since we are using semantics-preserving transformations in order to compare programs, it is also necessary to make a choice about the transformations we want Ask-Elle to support. This project focuses mainly on style-related transformations, which leads us to draw inspiration from related work on linting and refactoring.

## Related work in program analysis

We are interested in proving whether two programs are equivalent. This requires not only having a certain definition of equality, but also obtaining information from the programs in order to check whether the conditions for equality hold. Gathering information about a program is the subject of program analysis, which is used pervasively in automated assessment tools  [@2005alasurvey].

Since program analysis can be classified as static or dynamic, the same holds for the feedback tools that rely on it. While in the past there used to be a clear distinction between static and dynamic tools [@2016feedbackreview], it is nowadays less clear, as many tools are combining both approaches. For instance, Codewebs [@2014codewebs] and the tool presented by Huang et al. [@2013huang] are based on a combination of testing and static analysis. Another example is OverCode [@2015overcode], which collects data from the execution of Python programs (dynamic) and combines it with data derived from the source code (static). Ask-Elle also uses a mixed approach, since it first tries a normalization approach (static) and resorts to property-based testing (dynamic) whenever the former is insufficient [@2017askelle].

This project is focused towards improving Ask-Elle's static analysis capabilities, particularly its normalization procedure. From the tools mentioned above, Codewebs and Huang rely on AST-based comparisons instead of transformations. OverCode goes a bit further, by renaming variables based on the output of dynamic analysis, but this is still far from proper normalization.

Relevant research on automatic assessment tools that rely on program normalization include Xu and Chee's work [@2003transformation], who apply this method to compare Smalltalk programs. Also interesting is the work of Wang et al. [@2007wang], who implement a normalization-based tool to assess C programs. A more modern approach is given by the ITAP tutoring system [@2017ITAP], targeting Python programs, which uses normalization as well.

From the tools mentioned, the most interesting for our purposes is ITAP, because it also supports style-based transformations tailored to beginners. This is exactly one of the features we want Ask-Elle to support. Besides this, ITAP supports reconstructing the original AST from its normalized form in order to produce localized feedback, including suggestions to refactor the code. This is something we could draw inspiration from when implementing style-related hints.

## Related work in normalization

The Haskell language can be seen as a user-friendly version of the lambda calculus, with a series of extensions and syntactic sugar to facilitate programming. In fact, GHC Core, the intermediate language used by the Glasgow Haskell Compiler, is itself an implementation of the lambda calculus variant known as System FC [@2007systemfc]. This means we can leverage research on normalization of lambda terms when adding new transformations to Ask-Elle.

An overview of the properties of the lambda calculus is given by Barendregt et al. [@2013lambda]. They focus on the logical properties of the lambda calculus, which give us a clear idea of the limits within we operate. The main consequence for our work is that true normalization is impossible to achieve, because it is undecidable.

Even though normalization is undecidable in general, it can still be decided in some particular cases. Our own research is a clear example of this, as Ask-Elle relies on the ability to match student programs and model solutions. Different approaches have been tried to determine whether two lambda terms are equivalent, each one with its own trade-offs. For instance, Grabmayer and Rochel [@2014letrec] describe an advanced mechanism based on program transformations and graph comparison. We expect to find inspiration in this kind of research to improve Ask-Elle's own normalization.

## Related work in linting and refactoring

The topic of Haskell refactoring has been extensively handled by Brown [@2008brown], together with Thompson and Li [@2013thompson], though mainly from the perspective of a refactoring tool. This is reflected in some of the refactorings that are described, such as adding a constructor to a data type or introducing pattern matching. They also mention some style refactorings, like finding and replacing redefinitions of common functions.

More interesting to our purposes is HLint [@HLint], a tool that analyzes Haskell programs and reports ways in which style can be improved. It includes a broad corpus of style-refactorings, from detecting list anti-patterns to suggesting using foldr whenever certain recursion strategies are detected. Ask-Elle can draw a lot of inspiration from HLint on the area of style-aware feedback and can even go a step further, by giving hints on programs that contain holes.
