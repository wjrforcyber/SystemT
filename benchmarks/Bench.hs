module Main where

import Control.DeepSeq
import Control.Exception
import Criterion.Main
import Lang.L1.Eval as L1
import Lang.L1.Syntax as L1
import Lang.L6.Eval.EEval as L6
import Lang.L6.Syntax.Extrinsic as L6
import Lang.L6.Examples.Fib as L6
import Lang.L6.Examples.Base as L6

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
        "fib"
        [ bench "fibHs" $ nf L6.fibHs 10,
          bench "fibExp" $ nf L6.eval (EApp fibExp (fromNat 10))
        ]
    ]
