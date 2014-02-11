import qualified Data.ByteString.Char8 as B
import Data.Word (Word8)
import System.Hardware.Serialport
    (SerialPort, CommSpeed(..), Parity(..), StopBits(..), SerialPortSettings(..),
     openSerial, send, recv, closeSerial, defaultSerialSettings)

import Data.List (foldl')
import Data.Maybe (isJust, fromJust)
import Text.Regex.Posix ((=~))

import System.Environment (getArgs)
import Control.Monad (void, forever, when)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
    (Chan, newChan, dupChan, readChan, writeChan)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (text)

main = do
    args <- fmap parseArgs getArgs
    putStrLn "Using settings:"

    let guiConfig = defaultConfig {
            tpPort = 10000,
            tpStatic = Just "static",
            tpLog  = const $ return ()
         }
        serialConfig = defaultSerialSettings {
            commSpeed = baud args,
            bitsPerWord = dataSize args,
            stopb = stopSize args,
            parity = paren args
         }

    incoming <- newChan
    outgoing <- newChan
    serial <- openSerial (filePath args) serialConfig
    reader <- forkIO $ forever $ recvLn serial >>= writeChan incoming
    writer <- forkIO $ forever $ send   serial =<< readChan  outgoing

    putStrLn "Press ENTER to exit."
    ui <- forkIO $ startGUI guiConfig $ setup incoming outgoing
    void getLine

    killThread ui
    killThread reader
    closeSerial serial

-- Sets up the UI for a session. This gets called each time a client connects
-- to the Threepenny server.
setup :: Bus -> Bus -> Window -> UI ()
setup incoming outgoing window = void $ do
    return window # set title "Serial"
    addStyleSheets window ["bootstrap.min.css", "console.css"]

    console <- UI.new #. "console"
    input   <- UI.input
    navbar  <- mkNavbar [element input]
    getBody window #+ [element navbar, mkContainer [element console]]

    incoming' <- liftIO $ dupChan incoming
    reader <- liftIO $ forkIO $ forever $ readChan incoming' >>= addUpdate window console
    on UI.disconnect window $ const $ liftIO $ killThread reader

    outgoing' <- liftIO $ dupChan outgoing
    on UI.sendValue input $ \content -> do
        element input # set value ""
        when (not (null content)) $ liftIO $ do
            writeChan outgoing' (B.pack content)

addUpdate w e u = atomic w $ runUI w $ element e #+
    [UI.div #. "line" #+ [string $ B.unpack u]]

recvLn :: SerialPort -> IO B.ByteString
recvLn s = do
    first <- recv s 1
    rest <- if first == B.pack "\n"
        then return $ B.pack ""
        else recvLn s
    return $ first `B.append` rest

mkNavbar contents = UI.div #. "navbar navbar-inverse navbar-fixed-top"
                           #+ [mkContainer contents]

mkNavbarRightForm contents = UI.div #. "navbar-form navbar-right" #+ contents

mkContainer contents = UI.div #. "container" #+ contents

mkButton text = UI.button #. "btn btn-success" # set UI.text text

data Args = Args {
    filePath :: FilePath,
    stopSize :: StopBits,
    dataSize :: Word8,
    paren    :: Parity,
    baud     :: CommSpeed
 }

defaultArgs = Args "COM1" One 8 NoParity CS9600

parseArgs :: [String] -> Args
parseArgs = foldl' go defaultArgs
    where go acc arg
            | isCommSpeed = acc { baud = bd }
            | isBits      = acc { stopSize = sb, dataSize = db, paren = pb }
            | otherwise   = acc { filePath = arg }
            where isCommSpeed = isJust $ maybeReadCS arg
                  isBits = arg =~ "[0-9](E|O|N)[12]" :: Bool
                  bd = fromJust $ maybeReadCS arg
                  db = read [arg !! 0] :: Word8
                  pb = case (arg !! 1) of
                    'N' -> NoParity
                    'E' -> Even
                    'O' -> Odd
                  sb = case (arg !! 2) of
                    '1' -> One
                    '2' -> Two

type Bus = Chan B.ByteString

addStyleSheets w = mapM $ UI.addStyleSheet w

maybeReadCS s = case s of
    "CS110" -> Just CS110
    "CS300" -> Just CS300
    "CS600" -> Just CS600
    "CS1200" -> Just CS1200
    "CS2400" -> Just CS2400
    "CS4800" -> Just CS4800
    "CS9600" -> Just CS9600
    "CS19200" -> Just CS19200
    "CS38400" -> Just CS38400
    "CS57600" -> Just CS57600
    "CS115200" -> Just CS115200
    "110" -> Just CS110
    "300" -> Just CS300
    "600" -> Just CS600
    "1200" -> Just CS1200
    "2400" -> Just CS2400
    "4800" -> Just CS4800
    "9600" -> Just CS9600
    "19200" -> Just CS19200
    "38400" -> Just CS38400
    "57600" -> Just CS57600
    "115200" -> Just CS115200
    otherwise -> Nothing
