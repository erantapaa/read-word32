module Main where

import qualified Lib

import Control.Monad
import Options.Applicative.Simple

main = 
  do (opts,runCmd) <-
       simpleOptions "ver"
                     "header"
                     "desc"
                     (pure ()) $
       do addCommand "v1" "run find_C0_v1"
                     (Lib.find_C0_v1 >=> print)
                     (argument str (metavar "FILE"))
          addCommand "v2" "run find_C0_v2"
                     (Lib.find_C0_v2 >=> print)
                     (argument str (metavar "FILE"))
          addCommand "v3" "run find_C0_v3"
                     (Lib.find_C0_v3 >=> print)
                     (argument str (metavar "FILE"))

     runCmd

