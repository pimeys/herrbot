import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import System.Time
import Control.Arrow
import Control.Monad.Reader
import Control.OldException
import Prelude hiding (catch)

server = "irc.freenode.org"
port   = 6667
chan   = "#herrbot-testing"
nick   = "herrbot"

data Bot = Bot { socket :: Handle, starttime :: ClockTime }
type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = catch (runReaderT run st) (const $ return ())

connect :: IO Bot
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h t)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :tutorial bot")
    write "JOIN" chan
    asks socket >>= listen

listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    io (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    forever a = do a; forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

eval :: String -> Net ()
eval "!quit"                   = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
eval "!uptime"                 = uptime >>= privmsg
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval _                         = return ()

uptime :: Net String
uptime = do
    now <- io getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

pretty :: TimeDiff -> String
pretty td =
    unwords $ map (uncurry (++) . first show) $
    if null diffs then [(0,"s")] else diffs
    where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                      in (tot',(sec',typ):acc)
          metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
          diffs = filter ((/= 0) . fst) $ reverse $ snd $
                  foldl' merge (tdSec td,[]) metrics

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

io :: IO a -> Net a
io = liftIO
