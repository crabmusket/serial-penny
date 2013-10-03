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
    serial <- openSerial "COM4" defaultSerialSettings
    reads <- forkIO $ forever $ recvLn serial >>= writeChan bus

    putStrLn "Press ENTER to exit."
    let port = 10000
    ui <- forkIO $ startGUI defaultConfig
        { tpPort = port
        , tpStatic = Just "static"
        , tpLog  = const $ return ()
        } $ setup bus
    system $ "start \"\" \"http://localhost:" ++ show port ++ "\""

    discard getLine
    killThread ui
    killThread reads
    closeSerial serial

setup :: Bus -> Window -> IO ()
setup globalBus window = void $ do
    return window # set title "Serial"
    addStyleSheets window ["bootstrap.min.css", "console.css"]

    console <- UI.new #. "console"
    getBody window #+
        [ bsNavbar "fixed-top inverse" $ UI.span # set text "Serial console"
        , bsContainer [element console]
        ]

    bus <- dupChan globalBus
    listener <- forkIO $ getChanContents bus >>= mapM_ (addUpdate window console)
    on UI.disconnect window $ const $ killThread listener

addUpdate w e u = atomic w $ element e #+
    [UI.div #. "line" #+ [string $ B.unpack u]]

recvLn :: SerialPort -> IO B.ByteString
recvLn s = do
    first <- recv s 1
    rest <- if first == B.pack "\n"
        then return $ B.pack ""
        else recvLn s
    return $ first `B.append` rest

bsNavbar styles title = UI.div #. styles' #+ title' where
    styles' = unwords $ "navbar" : (map ("navbar-" ++) $ words styles)
    title'  = [bsContainer [UI.div #. "navbar-header" #+ [title] #. "navbar-brand"]]

bsContainer contents = UI.div #. "container" #+ contents

type Bus = Chan B.ByteString

discard = void

addStyleSheets w = mapM $ UI.addStyleSheet w
