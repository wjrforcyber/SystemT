# Implementing System T in Haskell

![Build and Test](https://github.com/wjrforcyber/SystemT/actions/workflows/ci.yml/badge.svg)
![Linelint](https://github.com/wjrforcyber/SystemT/actions/workflows/lint.yml/badge.svg)

## Abstract
Computers are general-purpose computing devices, and programming languages enable programmers to talk to them. In the field of programming languages, we study the meta-theory of programming languages, such as, their expressivity, robustness, and efficiency.

In this project, we perform a case study in the design and implementation of programming languages. We design a toy language based on System T, and implement it using Haskell. Our language has higher-order functions and recursion. We design a bi-directional type system and an operational semantics for it. We evaluate our language implementation by verifying important properties of the type system and operational semantics. We write several example programs in our language and run them, comparing the performance against their native implementation.

## Folder Structure
<pre>
+---.github
|   \---workflows
+---benchmarks                                    # benchmark by using criterion
+---src                                           # main cource file
|   +---Common                                    # Common types
|   \---Lang                                      # Language from L1 to L6
|       +---L1
|       +---L2
|       +---L3
|       |   +---Eval
|       |   \---Syntax
|       +---L4
|       |   +---Eval
|       |   \---Syntax
|       +---L5
|       |   +---Eval
|       |   \---Syntax
|       \---L6                                    # Final Language L6
|           +---Eval                              # Evaluator
|           +---Examples                          # Example test function set
|           \---Syntax                            # Syntax
\---tests                                         # Tasty test framework
    \---Lang
        +---L1
        +---L2
        +---L3
        +---L4
        +---L5
        \---L6
</pre>
