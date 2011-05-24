import Data.Char
import Data.List
import Control.Monad
import qualified GHC.IO.Device as Ghc
import System.IO
import System.Posix.IO

main = do  
       --hSetBuffering stdin $ BlockBuffering (Just 4)
       hSetBuffering stdout NoBuffering
--Read 4 bytes + a line Break
       (userInput, bytesRead) <- fdRead stdInput 5
       let sendReply = checkInput userInput
       fdWrite stdOutput sendReply 
       --hFlush stdout
       main
       
       
checkInput input 
  | check input == True =  "pong\n"
  | otherwise = "Haskell"
  where check inp = "ping" `isInfixOf` inp
       

--takeReq = Ghc.read 8
            
processReq input = do
                    unlines $ map (\x -> if x == "ping" then "pong" else "Hello") . lines $ input

