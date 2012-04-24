import Bot
import Network
import System.IO
import Control.Monad.Reader
import Control.OldException
import Prelude hiding (catch)

main :: IO ()
main = bracket connect disconnect loop
  where
    loop st    = catch (runReaderT run st) (const $ return ())
