import qualified Data.ByteString.Char8 as B
import System.Hardware.Serialport
    (SerialPort, openSerial, recv, closeSerial, defaultSerialSettings)

import Control.Monad (void, forever, mapM_)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
    (Chan, newChan, dupChan, writeChan, getChanContents)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (text)

main = do
    incoming <- newChan
    serial <- openSerial "COM4" defaultSerialSettings
    reader <- forkIO $ forever $ recvLn serial >>= writeChan incoming

    let config = defaultConfig {
            tpPort = 10000,
            tpStatic = Just "static",
            tpLog  = const $ return ()
         }

    putStrLn "Press ENTER to exit."
    ui <- forkIO $ startGUI config $ setup incoming
    void getLine

    killThread ui
    killThread reader
    closeSerial serial

-- Sets up the UI for a session. This gets called each time a client connects
-- to the Threepenny server.
setup :: Bus -> Window -> UI ()
setup incoming window = void $ do
    return window # set title "Serial"
    addStyleSheets window ["bootstrap.min.css", "console.css"]

    console <- UI.new #. "console"
    connect <- mkButton "Connect"
    navbar  <- mkNavbar "Serial console" [] $ [mkNavbarRightForm [element connect]]
    getBody window #+ [element navbar, mkContainer [element console]]

    incoming' <- liftIO $ dupChan incoming
    reader <- liftIO $ forkIO $ getChanContents incoming' >>= mapM_ (addUpdate window console)
    on UI.disconnect window $ const $ liftIO $ killThread reader

addUpdate w e u = atomic w $ runUI w $ element e #+
    [UI.div #. "line" #+ [string $ B.unpack u]]

recvLn :: SerialPort -> IO B.ByteString
recvLn s = do
    first <- recv s 1
    rest <- if first == B.pack "\n"
        then return $ B.pack ""
        else recvLn s
    return $ first `B.append` rest

mkNavbar title left right = UI.div #.
    "navbar navbar-inverse navbar-fixed-top" #+
    [mkContainer $
        [ UI.div #. "navbar-header" #+ [UI.span # set UI.text title] #. "navbar-brand" ]
        ++ left ++ right
    ]

mkNavbarRightForm contents = UI.div #. "navbar-form navbar-right" #+ contents

mkContainer contents = UI.div #. "container" #+ contents

mkButton text = UI.button #. "btn btn-success" # set UI.text text

type Bus = Chan B.ByteString

addStyleSheets w = mapM $ UI.addStyleSheet w
