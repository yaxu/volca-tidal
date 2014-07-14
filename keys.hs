
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.Sequencer.Connect as Connect
import Control.Concurrent (threadDelay, forkIO)
import System.Environment (getArgs, )
import Data.Maybe
import GHC.Word
import GHC.Int
import Sound.OSC.FD
import Sound.OSC.Type

channel = Event.Channel 0

parseIntegral c ps = do p <- listToMaybe ps
                        (i) <- d_get p
                        return $ c (fromIntegral (i :: Int))

parse :: String -> [Datum] -> Maybe Control

parse "/note" = \(val:vel:[]) ->
  do val' <- d_get val
     vel' <- d_get vel
     return $ Note (fromIntegral $ (val' :: Int)) (fromIntegral $ (vel' :: Int))

parse "/portamento"    = parseIntegral (Portamento)
parse "/expression"    = parseIntegral (Expression)
parse "/voice"         = parseIntegral (Voice)
parse "/octave"        = parseIntegral (Octave)
parse "/detune"        = parseIntegral (Detune)
parse "/vcoegint"      = parseIntegral (VCOEgInt)
parse "/cutoff"        = parseIntegral (Cutoff)
parse "/vcfegint"      = parseIntegral (VCFEgInt)
parse "/lforate"       = parseIntegral (LFORate)
parse "/lfopitchint"   = parseIntegral (LFOPitchInt)
parse "/lfocutoffint"  = parseIntegral (LFOCutoffInt)
parse "/attack"        = parseIntegral (Attack)
parse "/decay"         = parseIntegral (Decay)
parse "/sustain"       = parseIntegral (Sustain)
parse "/delaytime"     = parseIntegral (DelayTime)
parse "/delayfeedback" = parseIntegral (DelayFeedback)
parse _                = const Nothing

data Control = Note {note :: Word8, velocity :: Word8}
             | Portamento {value :: Int32}
             | Expression {value :: Int32}
             | Voice {value :: Int32}
             | Octave {value :: Int32}
             | Detune {value :: Int32}
             | VCOEgInt {value :: Int32}
             | Cutoff {value :: Int32}
             | VCFEgInt {value :: Int32}
             | LFORate {value :: Int32}
             | LFOPitchInt {value :: Int32}
             | LFOCutoffInt {value :: Int32}
             | Attack {value :: Int32}
             | Decay {value :: Int32}
             | Sustain {value :: Int32}
             | DelayTime {value :: Int32}
             | DelayFeedback {value :: Int32}

messageToVolca :: Maybe Message -> Maybe Control
messageToVolca Nothing = Nothing
messageToVolca m = do (Message path ps) <- m
                      parse path ps
                      
ctrlN (Portamento v)    = (5, v)
ctrlN (Expression v)    = (11, v)
ctrlN (Voice v)         = (40, v)
ctrlN (Octave v)        = (41, v)
ctrlN (Detune v)        = (42, v)
ctrlN (VCOEgInt v)      = (43, v)
ctrlN (Cutoff v)        = (44, v)
ctrlN (VCFEgInt v)      = (45, v)
ctrlN (LFORate v)       = (46, v)
ctrlN (LFOPitchInt v)   = (47, v)
ctrlN (LFOCutoffInt v)  = (48, v)
ctrlN (Attack v)        = (49, v)
ctrlN (Decay v)         = (50, v)
ctrlN (Sustain v)       = (51, v)
ctrlN (DelayTime v)     = (52, v)
ctrlN (DelayFeedback v) = (53, v)
ctrlN _                  = (0,  0)

main :: IO ()
main = do x <- udpServer "127.0.0.1" 3030
          h <- SndSeq.openDefault SndSeq.Block
          Client.setName (h :: SndSeq.T SndSeq.OutputMode) "Tidal"
          c <- Client.getId h
          p <- Port.createSimple h "out"
               (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
          as <- getArgs
          let dev = fromMaybe "20:0" $ listToMaybe as
          conn <- Connect.createTo h p =<< Addr.parse h dev
          mainLoop h x conn
  `AlsaExc.catch` \e ->
     putStrLn $ "alsa_exception: " ++ AlsaExc.show e
     where mainLoop h x conn = do m <- recvMessage x
                                  act h x conn (messageToVolca m)
                                  mainLoop h x conn
           act h x conn (Just (Note val vel)) = 
             do Event.outputDirect h $ noteOn conn val vel
                forkIO $ do threadDelay 10000
                            Event.outputDirect h $ noteOff conn val
                            return ()
                return ()
           act h x conn (Just c) = 
             do Event.outputDirect h $ makeCtrl conn (ctrlN c)
                return ()
           act _ _ _ Nothing = return ()


noteOn :: Connect.T -> Word8 -> Word8 -> Event.T
noteOn conn val vel = 
  Event.forConnection conn 
  $ Event.NoteEv Event.NoteOn
  $ Event.simpleNote channel
                     (Event.Pitch (val))
                     (Event.Velocity vel)

noteOff :: Connect.T -> Word8 -> Event.T
noteOff conn val = 
  Event.forConnection conn 
  $ Event.NoteEv Event.NoteOff
  $ Event.simpleNote channel
                     (Event.Pitch (val))
                     (Event.normalVelocity)

makeCtrl :: Connect.T -> (Word32, GHC.Int.Int32) -> Event.T
makeCtrl conn (c, n) = 
  Event.forConnection conn 
  $ Event.CtrlEv Event.Controller $ Event.Ctrl 
                                    channel 
                                    (Event.Parameter c) 
                                    (Event.Value n)

