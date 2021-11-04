module Main where

import Control.DeepSeq
import Control.Exception
import Criterion.Main
import Lang.L1.Eval
import Lang.L1.Syntax

setupEnv :: IO (Exp, Exp, Exp)
setupEnv = do
  testExp <- evaluate . force $ unsafeParse "42 * 35 * 27 + 12 * 41"
  testOptExp <- evaluate . force $ opt testExp
  testOpt2Exp <- evaluate . force $ opt2 testExp
  return (testExp, testOptExp, testOpt2Exp)

main :: IO ()
main =
  defaultMain
    [ env setupEnv $ \ ~(testExp, testOptExp, testOpt2Exp) ->
        bgroup
          "eval"
          [ bench "eval" $ nf eval testExp,
            bench "opt" $ nf eval testOptExp,
            bench "opt2" $ nf eval testOpt2Exp
          ]
    ]
