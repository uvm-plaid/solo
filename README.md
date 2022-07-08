# Solo: A Lightweight Static Analysis for Differential Privacy

----

# Artifact Overview

Solo is a Haskell library for static verification of differential
privacy. Our paper makes the following claims supported by the
artifact:

- Solo encodes sensitivity and privacy analysis in Haskell's types
- Solo can be used to write and verify useful differentially private programs
- Solo does not impose significant annotation burden on the programmer

# Getting Started

Requirements:
- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)

To build Solo:

```
stack build
```

# Step by Step Instructions

Solo's verification happens at compile time; compilation without errors
indicates that each function satisfies the differential privacy guarantee
indicated by its type. This artifact is a demonstration of (sound)
sensitivity/privacy typeability only for the design of Solo, and it is not
possible to actually run any of the programs to observe computed output. There
are thus no additional experiments to run, beyond `stack build`. 

To verify the claims of the paper, we suggest that reviewers:

1. Check the examples provided in `src/Main.hs` against the simple
   examples in Sections 4, 5, and 6 of the paper
2. Check the sensitivity primitives provided in `src/Primitives.hs`
   against the primitives described in Section 7 of the paper
3. Check the library functions provided in `src/StdLib.hs` against the
   library functions described in Section 7 of the paper (in
   particular, note that each function in `StdLib.hs` has an
   accompanying definition that is checked against its type)
4. Check the case study definitions provided in `src/Main.hs` against
   the case studies described in Section 8 of the paper
5. Uncomment the commented examples on lines 53 and 61 of
   `src/Main.hs`, and verify that compilation fails due to a type
   error. Both failures demonstrate Solo's ability to find privacy
   bugs.

We have tried to make the implementation minimal and understandable,
while ensuring that each example in the paper is described in the
code.
