module Main where

import Control.DeepSeq
import Control.Exception
import Criterion.Main
import Lang.L1.Eval as L1
import Lang.L1.Syntax as L1
import Lang.L6.Eval.EEval as L6
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
          bench "isZeroExp" $ nf L6.eval (EApp isZeroExp (fromNat 10))
        ],
      bgroup
        "pred"
        [ bench "predHs" $ nf L6.predHs 10,
          bench "predExp" $ nf L6.eval (EApp predExp (fromNat 10))
        ],
      bgroup
        "fib"
        [ bench "fibHs" $ nf L6.fibHs 10,
          bench "fibExp" $ nf L6.eval (EApp fibExp (fromNat 10))
        ],
      bgroup
        "add"
        [ bench "addHs" $ nf (L6.addHs 10) 20,
          bench "addExp" $ nf L6.eval (EApp (EApp addExp (fromNat 10)) (fromNat 20))
        ],
      bgroup
        "double"
        [ bench "doubleHs" $ nf L6.doubleHs 10,
          bench "doubleExp" $ nf L6.eval (EApp doubleExp (fromNat 10))
        ],
      bgroup
        "expo"
        [ bench "expoHs" $ nf (L6.expoHs 2) 3,
          bench "doubleExp" $ nf L6.eval (EApp (EApp expoExp (fromNat 2)) (fromNat 3))
        ],
      bgroup
        "fac"
        [ bench "facHs" $ nf L6.doubleHs 5,
          bench "facExp" $ nf L6.eval (EApp facExp (fromNat 5))
        ],
      bgroup
        "mul"
        [ bench "mulHs" $ nf (L6.mulHs 10) 20,
          bench "mulExp" $ nf L6.eval (EApp (EApp mulExp (fromNat 10)) (fromNat 20))
        ],
      bgroup
        "tet"
        [ bench "tetHs" $ nf (L6.tetHs 2) 3,
          bench "tetExp" $ nf L6.eval (EApp (EApp tetExp (fromNat 2)) (fromNat 3))
        ]
    ]
