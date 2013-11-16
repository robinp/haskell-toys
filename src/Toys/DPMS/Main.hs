import Control.Applicative ((<$>))
import Control.Monad ((=<<))
import Graphics.XHB
import Graphics.XHB.Gen.DPMS as DPMS
import System.Exit

-- * Small script to read monitor DPMS state and return with different
--   exit codes depending on the state.

exitFail :: String -> Int -> IO ()
exitFail s code = do
  putStrLn s
  exitWith (ExitFailure code)

main :: IO ()
main = do
  mw <- connect
  case mw of
    Nothing -> exitFail "NoConn" 1
    Just w -> do
      rep <- DPMS.info w >>= getReply
      either
        (const$ exitFail "NoDPMS" 2)
        (putStrLn . show . power_level_InfoReply)
        rep
