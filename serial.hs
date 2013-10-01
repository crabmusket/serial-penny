import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
    (SerialPort, openSerial, recv, closeSerial, defaultSerialSettings)

import Control.Monad (void, forever, mapM_)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
    (Chan, newChan, dupChan, writeChan, getChanContents)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import System.Cmd (system)

main = do
    bus <- newChan
    serial <- openSerial "COM4" defaultSerialSettings -- TODO: set port from command-line arguments
    reads <- forkIO $ forever $ recvLn serial >>= writeChan bus

    putStrLn "Press ENTER to exit."
    let port = 10000 -- TODO: set port from command-line arguments
    ui <- forkIO $ startGUI defaultConfig
        { tpPort = port
        , tpLog  = const $ return ()
        } $ setup bus
    system $ "start \"\" \"http://localhost:" ++ show port ++ "\""

    void getLine
    killThread ui
    killThread reads
    closeSerial serial

setup :: Bus -> Window -> IO ()
setup globalBus window = void $ do
    bus <- dupChan globalBus

    updateList <- UI.div #. "updates"
    return window # set title "Serial"
    getBody window #+
        [ UI.h1 #+ [string "Serial thing"]
        , element updateList
        ]

    listener <- forkIO $ getChanContents bus >>= mapM_ (addUpdate window updateList)
    on UI.disconnect window $ const $ killThread listener

addUpdate w e u = atomic w $ element e #+
    [UI.div #. "update" #+ [string $ B.unpack u]]

recvLn :: SerialPort -> IO B.ByteString
recvLn s = do
    first <- recv s 1
    rest <- if first == B.pack "\n"
        then return $ B.pack ""
        else recvLn s
    return $ first `B.append` rest

type Bus = Chan B.ByteString
