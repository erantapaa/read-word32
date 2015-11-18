module Main where

import qualified Lib

import Control.Monad
import Options.Applicative.Simple
import Options.Applicative

parseInt :: ReadM Int
parseInt = eitherReader $ \arg -> case reads arg of
  [(r, suffix)] -> return r
  _             -> Left $ "invalid number: " ++ arg

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

          addCommand "create" "create a sample file"
                     (uncurry Lib.create_C0_file)
                     ( (,) <$> (argument str (metavar "FILE"))
                           <*> (argument parseInt (metavar "COUNT"))
                     )
     runCmd

