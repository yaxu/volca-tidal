import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.Sequencer.Connect as Connect
import Control.Concurrent (threadDelay)
import System.Environment (getArgs, )
import Data.Maybe
import GHC.Word
import GHC.Int
import Sound.OSC.FD
import Sound.OSC.Type

channel = Event.Channel 9

parseIntegral c ps = do p <- listToMaybe ps
                        (i) <- d_get p
                        return $ c (fromIntegral (i :: Int))

parse "/note/kick"    = Just . const (Note Kick)
parse "/note/snare"   = Just . const (Note Snare)
parse "/note/lotom"   = Just . const (Note LoTom)
parse "/note/hitom"   = Just . const (Note HiTom)
parse "/note/clhat"   = Just . const (Note ClHat)
parse "/note/ophat"   = Just . const (Note OpHat)
parse "/note/clap"    = Just . const (Note Clap)
parse "/note/claves"  = Just . const (Note Claves)
parse "/note/agogo"   = Just . const (Note Agogo)
parse "/note/crash"   = Just . const (Note Crash)

parse "/level/kick"    = parseIntegral (Level Kick)
parse "/level/snare"   = parseIntegral (Level Snare)
parse "/level/lotom"   = parseIntegral (Level LoTom)
parse "/level/hitom"   = parseIntegral (Level HiTom)
parse "/level/clhat"   = parseIntegral (Level ClHat)
parse "/level/ophat"   = parseIntegral (Level OpHat)
parse "/level/clap"    = parseIntegral (Level Clap)
parse "/level/claves"  = parseIntegral (Level Claves)
parse "/level/agogo"   = parseIntegral (Level Agogo)
parse "/level/crash"   = parseIntegral (Level Crash)

parse "/speed/clap"    = parseIntegral (Speed Clap)
parse "/speed/claves"  = parseIntegral (Speed Claves)
parse "/speed/agogo"   = parseIntegral (Speed Agogo)
parse "/speed/crash"   = parseIntegral (Speed Crash)

parse "/stutter/time"  = parseIntegral (StutterTime)
parse "/stutter/depth" = parseIntegral (StutterDepth)
parse "/decay/tom"     = parseIntegral (TomDecay)
parse "/decay/clhat"   = parseIntegral (ClosedHatDecay)
parse "/decay/ophat"   = parseIntegral (OpenHatDecay)
parse "/grain/hat"     = parseIntegral (HatGrain)
parse _                = const Nothing

data Control = Note Drum
             | Level Drum Int32
             | Speed Drum Int32
             | StutterTime Int32
             | StutterDepth Int32
             | TomDecay Int32
             | ClosedHatDecay Int32
             | OpenHatDecay Int32
             | HatGrain Int32

messageToVolca :: Maybe Message -> Maybe Control
messageToVolca Nothing = Nothing
messageToVolca m = do (Message path ps) <- m
                      parse path ps
                      
ctrlN (Level Kick v)     = (40, v)
ctrlN (Level Snare v)    = (41, v)
ctrlN (Level LoTom v)    = (42, v)
ctrlN (Level HiTom v)    = (43, v)
ctrlN (Level ClHat v)    = (44, v)
ctrlN (Level OpHat v)    = (45, v)
ctrlN (Level Clap v)     = (46, v)
ctrlN (Level Claves v)   = (47, v)
ctrlN (Level Agogo v)    = (48, v)
ctrlN (Level Crash v)    = (49, v)
ctrlN (Speed Clap v)     = (50, v)
ctrlN (Speed Claves v)   = (51, v)
ctrlN (Speed Agogo v)    = (52, v)
ctrlN (Speed Crash v)    = (53, v)
ctrlN (StutterTime v)    = (54, v)
ctrlN (StutterDepth v)   = (55, v)
ctrlN (TomDecay v)       = (56, v)
ctrlN (ClosedHatDecay v) = (57, v)
ctrlN (OpenHatDecay v)   = (58, v)
ctrlN (HatGrain v)       = (59, v)
ctrlN _                  = (0,  0)


data Drum = Kick
           | Snare
           | LoTom
           | HiTom
           | ClHat
           | OpHat
           | Clap
           | Claves
           | Agogo  
           | Crash

noteN :: Drum -> Word8
noteN Kick   = 36 
noteN Snare  = 38
noteN LoTom  = 43
noteN HiTom  = 50
noteN ClHat  = 42
noteN OpHat  = 46
noteN Clap   = 39
noteN Claves = 75
noteN Agogo  = 67
noteN Crash  = 49

main :: IO ()
main = do x <- udpServer "127.0.0.1" 9090
          h <- SndSeq.openDefault SndSeq.Block
          Client.setName (h :: SndSeq.T SndSeq.OutputMode) "Tidal"
          c <- Client.getId h
          p <- Port.createSimple h "out"
               (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
          as <- getArgs
          let dev = fromMaybe "28:0" $ listToMaybe as
          conn <- Connect.createTo h p =<< Addr.parse h dev
          mainLoop h x conn
  `AlsaExc.catch` \e ->
     putStrLn $ "alsa_exception: " ++ AlsaExc.show e
     where mainLoop h x conn = do m <- recvMessage x
                                  act h x conn (messageToVolca m)
                                  mainLoop h x conn
           act h x conn (Just (Note s)) = 
             do Event.outputDirect h $ noteOn conn (noteN s)
                return ()
           act h x conn (Just c) = 
             do Event.outputDirect h $ makeCtrl conn (ctrlN c)
                return ()
           act _ _ _ Nothing = return ()


noteOn :: Connect.T -> Word8 -> Event.T
noteOn conn n = 
  Event.forConnection conn 
  $ Event.NoteEv Event.NoteOn
  $ Event.simpleNote channel
                     (Event.Pitch (n))
                     (Event.Velocity 1)

makeCtrl :: Connect.T -> (Word32, GHC.Int.Int32) -> Event.T
makeCtrl conn (c, n) = 
  Event.forConnection conn 
  $ Event.CtrlEv Event.Controller $ Event.Ctrl 
                                    channel 
                                    (Event.Parameter c) 
                                    (Event.Value n)

