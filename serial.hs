import qualified Data.ByteString.Char8 as B
import System.Environment (getArgs)
import System.Hardware.Serialport
    (SerialPort, openSerial, send, recv, closeSerial, defaultSerialSettings)

import Control.Monad (void, forever, when)
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan
    (Chan, newChan, dupChan, readChan, writeChan)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (text)

main = do
    args <- fmap parseArgs getArgs

    incoming <- newChan
    outgoing <- newChan
    serial <- openSerial (head args) defaultSerialSettings
    reader <- forkIO $ forever $ recvLn serial >>= writeChan incoming
    writer <- forkIO $ forever $ send serial   =<< readChan  outgoing

    let config = defaultConfig {
            tpPort = 10000,
            tpStatic = Just "static",
            tpLog  = const $ return ()
         }

    putStrLn "Press ENTER to exit."
    ui <- forkIO $ startGUI config $ setup incoming outgoing
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
    connect <- mkButton "Connect"
    navbar  <- mkNavbar "Serial console" [] $ [mkNavbarRightForm [element connect]]
    input   <- UI.textarea
    getBody window #+ [element navbar, mkContainer [element console, element input]]

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
