module Main where

import Control.DeepSeq
import Control.Exception
import Criterion.Main
import Lang.L1.Eval as L1
import Lang.L1.Syntax as L1
import Lang.L5.Eval.EEval as L5EE
import Lang.L5.Syntax.Extrinsic as L5
import Lang.L6.Eval.EEval as LL6
import Lang.L6.Examples.Add as L6
import Lang.L6.Examples.Base as L6
import Lang.L6.Examples.Double as L6
import Lang.L6.Examples.Expo as L6
import Lang.L6.Examples.Fac as L6
import Lang.L6.Examples.Fib as L6
import Lang.L6.Examples.IsZero as L6
import Lang.L6.Examples.Mul as L6
import Lang.L6.Examples.Pred as L6
import Lang.L6.Examples.Tet as L6
import Lang.L6.Examples.Acker as L6

setupEnv :: IO (L1.Exp, L1.Exp, L1.Exp)
setupEnv = do
  testExp <- evaluate . force $ L1.unsafeParse "42 * 35 * 27 + 12 * 41"
  testOptExp <- evaluate . force $ L1.opt testExp
  testOpt2Exp <- evaluate . force $ L1.opt2 testExp
  return (testExp, testOptExp, testOpt2Exp)

main :: IO ()
main =
  defaultMain
    [ env setupEnv $ \ ~(testExp, testOptExp, testOpt2Exp) ->
        bgroup
          "eval"
          [ bench "eval" $ nf L1.eval testExp,
            bench "opt" $ nf L1.eval testOptExp,
            bench "opt2" $ nf L1.eval testOpt2Exp
          ],
      bgroup
        "isZero"
        [ bench "isZeroHs" $ nf L6.isZeroHs 10,
          bench "*isZeroExp" $ nf LL6.evalStar (L6.EApp isZeroExp (L6.fromNat 10)),
          bench "isZeroExp" $ nf (LL6.runEval (LL6.eval (L6.EApp isZeroExp (L6.fromNat 10)))) LL6.Emp
        ],
      bgroup
        "pred"
        [ bench "predHs" $ nf L6.predHs 10,
          bench "*predExp" $ nf LL6.evalStar (L6.EApp predExp (L6.fromNat 10)),
          bench "predExp" $ nf (LL6.runEval (LL6.eval (L6.EApp predExp (L6.fromNat 10)))) LL6.Emp
        ],
      bgroup
        "fib"
        [ bench "fibHs" $ nf L6.fibHs 10,
          bench "*fibExp" $ nf LL6.evalStar (L6.EApp fibExp (L6.fromNat 10)),
          bench "fibExp" $ nf (LL6.runEval (LL6.eval (L6.EApp fibExp (L6.fromNat 10)))) LL6.Emp
        ],
      bgroup
        "add"
        [ bench "addHs" $ nf (L6.addHs 10) 20,
          bench "*addExp" $ nf LL6.evalStar (L6.EApp (L6.EApp addExp (L6.fromNat 10)) (L6.fromNat 20)),
          bench "addExp" $ nf (LL6.runEval (LL6.eval (L6.EApp (L6.EApp addExp (L6.fromNat 10)) (L6.fromNat 20)))) LL6.Emp
        ],
      bgroup
        "double"
        [ bench "doubleHs" $ nf L6.doubleHs 10,
          bench "*doubleExp" $ nf LL6.evalStar (L6.EApp doubleExp (L6.fromNat 10)),
          bench "doubleExp" $ nf (LL6.runEval (LL6.eval (L6.EApp doubleExp (L6.fromNat 10)))) LL6.Emp
        ],
      bgroup
        "expo"
        [ bench "expoHs" $ nf (L6.expoHs 2) 3,
          bench "*expoExp" $ nf LL6.evalStar (L6.EApp (L6.EApp expoExp (L6.fromNat 2)) (L6.fromNat 3)),
          bench "expoExp" $ nf (LL6.runEval (LL6.eval (L6.EApp (L6.EApp expoExp (L6.fromNat 2)) (L6.fromNat 3)))) LL6.Emp
        ],
      bgroup
        "fac"
        [ bench "facHs" $ nf L6.doubleHs 5,
          bench "*facExp" $ nf LL6.evalStar (L6.EApp facExp (L6.fromNat 5)),
          bench "facExp" $ nf (LL6.runEval (LL6.eval (L6.EApp facExp (L6.fromNat 5)))) LL6.Emp
        ],
      bgroup
        "mul"
        [ bench "mulHs" $ nf (L6.mulHs 10) 20,
          bench "*mulExp" $ nf LL6.evalStar (L6.EApp (L6.EApp mulExp (L6.fromNat 10)) (L6.fromNat 20)),
          bench "mulExp" $ nf (LL6.runEval (LL6.eval (L6.EApp (L6.EApp mulExp (L6.fromNat 10)) (L6.fromNat 20)))) LL6.Emp
        ],
      bgroup
        "tet"
        [ bench "tetHs" $ nf (L6.tetHs 2) 3,
          bench "*tetExp" $ nf LL6.evalStar (L6.EApp (L6.EApp tetExp (L6.fromNat 2)) (L6.fromNat 3)),
          bench "tetExp" $ nf (LL6.runEval (LL6.eval (L6.EApp (L6.EApp tetExp (L6.fromNat 2)) (L6.fromNat 3)))) LL6.Emp
        ],
      bgroup
        "acker"
        [ bench "ackerHs" $ nf (L6.tetHs 2) 3,
          bench "*ackerExp" $ nf LL6.evalStar (L6.EApp (L6.EApp ackerExp (L6.fromNat 2)) (L6.fromNat 3)),
          bench "ackerExp" $ nf (LL6.runEval (LL6.eval (L6.EApp (L6.EApp ackerExp (L6.fromNat 2)) (L6.fromNat 3)))) LL6.Emp
        ],
      bgroup
        "Add in L5 VS Add in L6"
        [ bench "L5 Add" $ nf (L5EE.runEval (L5EE.eval (L5.EAdd (L5EE.fromNat 10) (L5EE.fromNat 20)))) L5EE.Emp,
          bench "*L6 Add" $ nf LL6.evalStar (L6.EApp (L6.EApp addExp (L6.fromNat 10)) (L6.fromNat 20)),
          bench "L6 Add" $ nf (LL6.runEval (LL6.eval (L6.EApp (L6.EApp addExp (L6.fromNat 10)) (L6.fromNat 20)))) LL6.Emp
        ]
    ]
