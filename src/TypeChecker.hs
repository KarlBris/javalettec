module TypeChecker where

import Grammar.Abs
import Grammar.Print
import Grammar.ErrM

typecheck :: Program -> Err Program
typecheck p = undefined