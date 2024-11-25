# Implementing System T in Haskell

![Build and Test](https://github.com/wjrforcyber/SystemT/actions/workflows/ci.yml/badge.svg)
![Linelint](https://github.com/wjrforcyber/SystemT/actions/workflows/lint.yml/badge.svg)

## 📃Thesis
- [Implementing System T in Haskell](https://wjrforcyber.github.io/files/ISIH.pdf)

## ✍Abstract
Computers are general-purpose computing devices, and programming languages enable programmers to talk to them. In the field of programming languages, we study the meta-theory of programming languages, such as, their expressivity, robustness, and efficiency.

In this project, we perform a case study in the design and implementation of programming languages. We design a toy language based on System T, and implement it using Haskell. Our language has higher-order functions and recursion. We design a bi-directional type system and an operational semantics for it. We evaluate our language implementation by verifying important properties of the type system and operational semantics. We write several example programs in our language and run them, comparing the performance against their native implementation.

## 📁Folder Structure
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

## 🛠Usage
This project is built under GHC 8.10.7 and Cabal 3.4.0.0.
<br>Other GHC and Cabal version may work as well.
<br>A general way to run the project is:
>1. Clone this repo.
>2. Run `$ cabal build` to build the whole project.
>3. Run `$ cabal run test` to run all the property and unit tests(This may take a long time).
>4. Run `$ cabal run test -- -p "L6"` will run all the test in "L6", you could change this to any test group name defined in the test file.
>5. Run `$ cabal run test -- -p "every generated expression is well-scoped" --quickcheck-verbose` will show all the details in test "every generated expression is well-scoped", >since we used prettyprinter in our project, you could check the test in a user-friendly way.
>6. Run `$ cabal run bench -- -o result.html` will run all the functions in `Bench.hs` and show all the performance in a html report.
>7. Run `$cabal run bench -- --csv result.csv` will generate a general csv file describe the performance but is not as detailed as html report.

## 📚Related framework and library
Related framework and library used in this project contain
- [Tasty](https://github.com/UnkindPartition/tasty)
- [Criterion](https://github.com/haskell/criterion)
- [PrettyPrinter](https://github.com/quchen/prettyprinter).
 
