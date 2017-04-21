module ExRuntime exposing (..)

import Regex





regex_explodesWhenCalled : () -> Regex.Regex
regex_explodesWhenCalled _ =
  Regex.regex "(invalid"






func_explodesWhenCalled : () -> Bool
func_explodesWhenCalled _ =
  funcA == funcB

func_fineButWeird : () -> Bool
func_fineButWeird _ =
  funcA == funcA

funcA : Int -> Int
funcA n = n + 1

funcB : Int -> Int
funcB n = n + 1
