module Volca.Key where

import Sound.OSC.FD
import Sound.Tidal.Context
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception as E
import Data.Maybe
import Volca.Base

makeCtrl path = do m <- newMVar silence
                   return (path, m)

keystart = 
  do s <- openUDP "127.0.0.1" 3030
     noteM <- newMVar silence
     ctrlMs <- mapM makeCtrl ["/portamento", "/expression", 
                              "/voice", "/octave", "/detune", 
                              "/vcoegint", "/cutoff", "/vcfegint", 
                              "/lforate", "/lfopitchint", "/lfocutoffint", 
                              "/attack", "/decay", "/sustain",
                              "/delaytime" , "/delayfeedback"
                             ]
     forkIO $ clocked (ot s noteM ctrlMs)
     return ((swapMVar noteM, map (swapMVar . snd) ctrlMs, stop noteM ctrlMs))
  where stop noteM ctrlMs = do swapMVar noteM silence
                               mapM_ (\x -> swapMVar (snd x) silence) ctrlMs
                               return ()
            

ot :: UDP -> MVar (Pattern Int) -> [(String, MVar (Pattern Int))] -> Tempo -> Int -> IO ()
ot s noteM ctrlMs change beats
  = do note <- readMVar noteM
       ctrls <- mapM readCtrl ctrlMs
       let tpb' = 1
           beats' = (fromIntegral beats) :: Integer
           a = beats' % tpb'
           b = (beats' + 1) % tpb'
           messages = map 
                      (toNoteMessage s change beats) 
                      (seqToRelOnsets (a, b) note)
           messages' = concatMap (\(path, p) -> map (toCtrlMessage path s change beats) (seqToRelOnsets (a, b) p)) ctrls
                       
       E.catch (sequence_ (messages ++ messages')) (\msg -> putStrLn $ "oops " ++ show (msg :: E.SomeException))
       return ()
    where readCtrl (path, mv) = do p <- readMVar mv
                                   return (path, p)

toNoteMessage :: UDP -> Tempo -> Int -> (Double, Int) -> IO ()
toNoteMessage s change ticks (o, number) 
  = doAt logicalOnset $ sendOSC s $ Message ("/note") [int32 number, int32 64]  
  where beat = fromIntegral ticks / 1
        logicalNow = (logicalTime change beat)
        beat' = (fromIntegral ticks + 1) / 1
        logicalPeriod = (logicalTime change (beat + 1)) - logicalNow
        logicalOnset = logicalNow + (logicalPeriod * o) + Volca.Base.latency

toCtrlMessage :: String -> UDP -> Tempo -> Int -> (Double, Int) -> IO ()
toCtrlMessage path s change ticks (o, v) 
  = doAt logicalOnset $ sendOSC s $ Message (path) [int32 v] 
  where beat = fromIntegral ticks / 1
        logicalNow = (logicalTime change beat)
        beat' = (fromIntegral ticks + 1) / 1
        logicalPeriod = (logicalTime change (beat + 1)) - logicalNow
        logicalOnset = logicalNow + (logicalPeriod * o) + Volca.Base.latency



sine128 :: Pattern Int
sine128 = (floor . (* 128)) <$> sine1

discretise s = (\x y -> y) <$> (density 32 (pure 1)) <*> s
