{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Client(clientMain) where

import GopherTypes

import qualified Data.ByteString.Char8 as BSC
import Control.Exception (finally, try)
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Except ()
import Control.Monad.Trans.Maybe
import Data.Bits
import Data.Data
import Data.Kind (Type)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import Foreign
import Foreign.C
import Network.Socket
import Network.Socket.ByteString
import System.FilePath
import System.Posix.Directory
import System.Posix.Files
import System.Posix.User (getEffectiveUserID, getUserEntryForID, homeDirectory)
import Text.Read (readMaybe)

type ConstPtr :: Type -> Type
type role ConstPtr phantom
newtype ConstPtr a = ConstPtr { _unConstPtr :: Ptr a }
        deriving stock (Data)
        deriving newtype (Eq, Ord, Storable)

instance Show (ConstPtr a) where
        showsPrec d (ConstPtr p) = showParen (d > 10) $
                showString "ConstPtr " . showsPrec 11 p

data NewtComponentStruct
newtype NewtComponent = NewtComponent (Ptr NewtComponentStruct) deriving Eq

#define NEWT_FLAG_RETURNEXIT    (1 `shiftL` 0)
#define NEWT_FLAG_HIDDEN        (1 `shiftL` 1)
#define NEWT_FLAG_SCROLL        (1 `shiftL` 2)
#define NEWT_FLAG_DISABLED      (1 `shiftL` 3)
#define NEWT_FLAG_BORDER        (1 `shiftL` 5)
#define NEWT_FLAG_WRAP          (1 `shiftL` 6)
#define NEWT_FLAG_NOF12         (1 `shiftL` 7)
#define NEWT_FLAG_MULTIPLE      (1 `shiftL` 8)
#define NEWT_FLAG_SELECTED      (1 `shiftL` 9)
#define NEWT_FLAG_CHECKBOX      (1 `shiftL` 10)
#define NEWT_FLAG_PASSWORD      (1 `shiftL` 11)
#define NEWT_FLAG_SHOWCURSOR    (1 `shiftL` 12)

foreign import capi "newt.h newtInit" newtInit :: IO ()
foreign import capi "newt.h newtFinished" newtFinished :: IO ()
foreign import capi "newt.h newtCls" newtCls :: IO ()
foreign import capi "newt.h newtPopWindow" newtPopWindow :: IO ()
foreign import capi "newt.h newtCenteredWindow" newtCenteredWindow :: CInt -> CInt -> CString -> IO ()
foreign import capi "newt.h newtForm" newtForm :: NewtComponent -> Ptr () -> CInt -> IO NewtComponent
foreign import capi "newt.h newtFormAddComponent" newtFormAddComponent :: NewtComponent -> NewtComponent -> IO ()
foreign import capi "newt.h newtRunForm" newtRunForm :: NewtComponent -> IO NewtComponent
foreign import capi "newt.h newtFormDestroy" newtFormDestroy :: NewtComponent -> IO ()
foreign import capi "newt.h newtLabel" newtLabel :: CInt -> CInt -> CString -> IO NewtComponent
foreign import capi "newt.h newtButton" newtButton :: CInt -> CInt -> CString -> IO NewtComponent
foreign import capi "newt.h newtListbox" newtListbox :: CInt -> CInt -> CInt -> CInt -> IO NewtComponent
foreign import capi "newt.h newtListboxAddEntry" newtListboxAddEntry :: NewtComponent -> CString -> Ptr () -> IO CInt
foreign import capi "newt.h newtListboxSetWidth" newtListboxSetWidth :: NewtComponent -> CInt -> IO ()
foreign import capi "newt.h newtListboxGetCurrent" newtListboxGetCurrent :: NewtComponent -> IO (Ptr ())
foreign import capi "newt.h newtEntry" newtEntry :: CInt -> CInt -> CString -> CInt -> Ptr (ConstPtr CChar)-> CInt -> IO NewtComponent
foreign import capi "newt.h newtEntryGetValue" newtEntryGetValue :: NewtComponent -> IO CString
foreign import capi "newt.h newtWinMessage" newtWinMessage :: CString -> CString -> CString -> IO ()
foreign import capi "newt.h newtTextbox" newtTextbox :: CInt -> CInt -> CInt -> CInt -> CInt -> IO NewtComponent
foreign import capi "newt.h newtTextboxSetText" newtTextboxSetText :: NewtComponent -> CString -> IO ()

requestConnection :: MaybeT IO (HostName, String)
requestConnection = do
        csConnect <- liftIO $ newCString "Connect"
        csInput <- liftIO $ newCString "Input hostname and port number below:"
        csOK <- liftIO $ newCString "OK"
        csCancel <- liftIO $ newCString "Cancel"

        liftIO $ newtCenteredWindow 60 8 csConnect
        
        instr <- liftIO $ newtLabel 1 1 csInput
        hostnameE <- liftIO $ newtEntry 1 2 nullPtr 48 nullPtr 0
        portE <- liftIO $ newtEntry 50 2 nullPtr 9 nullPtr 0

        ok <- liftIO $ newtButton 1 4 csOK
        cancel <- liftIO $ newtButton 9 4 csCancel

        form <- liftIO $ newtForm (NewtComponent nullPtr) nullPtr 0
        liftIO $ do
                newtFormAddComponent form instr
                newtFormAddComponent form hostnameE
                newtFormAddComponent form portE
                newtFormAddComponent form ok
                newtFormAddComponent form cancel

        result <- liftIO $ newtRunForm form
        hostname :: HostName <- liftIO (peekCString =<< newtEntryGetValue hostnameE)
        port <- liftIO (peekCString =<< newtEntryGetValue portE)
        liftIO $ newtFormDestroy form
        liftIO newtPopWindow
        liftIO $ free csConnect
        liftIO $ free csInput
        liftIO $ free csOK
        liftIO $ free csCancel

        when (result == cancel) mzero
        pure (hostname, port)

ifHasAddress :: (HostName -> PortNumber -> IO ()) -> IO ()
ifHasAddress action = runMaybeT requestConnection >>= \case
        Just (h, p') -> case readMaybe p' of
                Just p -> action h p
                Nothing -> winMessage "I couldn't understand the hostname or port."
                        *> ifHasAddress action
        Nothing -> pure ()

getGopher :: HostName -> PortNumber -> String -> ExceptT String IO BSC.ByteString 
getGopher hostname port selector = do
        sock <- liftIO $ socket AF_INET Stream defaultProtocol
        liftIO $ setSockOpt sock RecvTimeOut (SocketTimeout 8000000)
        let addrHints = defaultHints {
                addrFamily = AF_INET,
                addrSocketType = Stream
        }

        as <- liftIO (map addrAddress <$>
                getAddrInfo (Just addrHints) (Just hostname) (Just $ show port))

        addr <- case as of
            [] -> throwError "Unable to resolve hostname."
            (x : _) -> pure x

        liftIO $ connect sock addr
        liftIO $ sendAll sock (BSC.pack $! selector <> "\r\n")
        reply <- liftIO $ recvGopher sock
        liftIO $ close sock
        pure reply

gopher :: HostName -> PortNumber -> String -> ExceptT String IO [Element]
gopher hostname port selector = mapMaybe parseLine . splitOn "\r\n" . BSC.unpack
       <$> getGopher hostname port selector

explore :: Element -> IO ()
explore el = case elType el of
        Directory -> runExceptT (uncurry gopher (elHost el) (elRsrc el)) >>= \case
                Right xs -> browse xs
                Left err -> winMessage err
        Information -> pure ()
        _ -> endpoint el

elementLine :: Element -> IO CString
elementLine el = newCString $ icon (elType el) : ' ' : ' ' : elName el
        where icon :: ElementType -> Char
              icon PlainText = '\xEA7B'
              icon Directory = '\xF413'
              icon PhoneBook = '\xF095'
              icon Error = '\xEA87'
              icon Macintosh = '\xE711'
              icon DOS = '\xE62A'
              icon Unix = '\xF0EC0'
              icon IndexSearch = '\xF0349'
              icon Telnet = '\xEA85'
              icon Binary = '\xEAE8'
              icon Gif = '\xF02E9'
              icon Image = '\xF02E9'
              icon Information = ' '
              icon _ = '?'

browse :: [Element] -> IO ()
browse es = do
        ns <- mapM elementLine es
        ps <- mapM newStablePtr es
        let es' = zip ns ps
            height = fromIntegral . min 36 . max 16 $ length es
            width = fromIntegral . min 132 . max 52 
                $ foldr (max . (+14) . length . elName) 0 es

        csMink <- newCString "Mink"
        csBack <- newCString "Back"
        newtCenteredWindow width height csMink

        list <- newtListbox 2 1 (height - 6) 
                (NEWT_FLAG_SCROLL .|. NEWT_FLAG_BORDER .|. NEWT_FLAG_RETURNEXIT)
        newtListboxSetWidth list (width - 4) 
        forM_ es' $ \(n, p) -> newtListboxAddEntry list n (castStablePtrToPtr p)
        button <- newtButton (width `div` 2 - 4) (height - 4) csBack

        form <- newtForm (NewtComponent nullPtr) nullPtr 0
        newtFormAddComponent form list
        newtFormAddComponent form button

        Control.Monad.void . runMaybeT . forever $ do
                target <- liftIO (newtRunForm form)
                if target == button then mzero else do
                        item' <- liftIO $ newtListboxGetCurrent list
                        let el' :: StablePtr Element
                            el' = castPtrToStablePtr item'
                        el <- liftIO $ deRefStablePtr el'
                        liftIO $ explore el

        newtFormDestroy form
        newtPopWindow
        forM_ ns free
        free csMink
        free csBack

        forM_ ps freeStablePtr

runEndpoint :: Element -> (BSC.ByteString -> IO ()) -> IO ()
runEndpoint el action = runExceptT (uncurry getGopher (elHost el) (elRsrc el)) >>= \case
        Right xs -> action xs
        Left err -> winMessage err

endpoint :: Element -> IO ()
endpoint el = case elType el of
        PlainText -> runEndpoint el
                $ longTextbox (elName el) . filter (/= '\r') . BSC.unpack
        Directory -> winMessage "Haskell bug lmao"
        IndexSearch -> Control.Monad.void . runMaybeT $ do
                Just query <- liftIO $ readPrompt "Enter search query below:"
                liftIO $ explore el {
                        elRsrc = elRsrc el <> "\t" <> query,
                        elType = Directory
                }
        Macintosh -> saveFileEndpoint el
        DOS -> saveFileEndpoint el
        Unix -> saveFileEndpoint el
        Binary -> saveFileEndpoint el
        Image -> saveFileEndpoint el
        Gif -> saveFileEndpoint el
        x -> winMessage $ "Preview not available for " ++ show x ++ "."

saveFileEndpoint :: Element -> IO ()
saveFileEndpoint el = do
        you <- getEffectiveUserID >>= getUserEntryForID
        let home = homeDirectory you
        let paths = [home </> "Downloads", home </> "Documents", home, "/"]
        path <- fromMaybe "/" . listToMaybe <$> filterM fileExist paths
        runEndpoint el $ saveFilePrompt path (elName el)

winMessage :: String -> IO ()
winMessage text = do
        a <- newCString "Mink"
        b <- newCString "Ok"
        c <- newCString text
        newtWinMessage a b c
        free a
        free b
        free c

longTextbox :: String -> String -> IO ()
longTextbox name xs = do
        let height = fromIntegral . min 36 . max 12 . length $ lines xs
            width = fromIntegral . min 110 . (+ 8) . maximum
                . map length $ lines xs

        csName <- newCString name
        newtCenteredWindow width height csName

        csBack <- newCString "Back"
        csText <- newCString xs
        box <- newtTextbox 2 1 (width - 6) (height - 6)
                (NEWT_FLAG_SCROLL .|. NEWT_FLAG_WRAP)
        newtTextboxSetText box csText

        button <- newtButton (width `div` 2 - 4) (height - 4) csBack
        form <- newtForm (NewtComponent nullPtr) nullPtr 0
        newtFormAddComponent form box
        newtFormAddComponent form button

        _ <- newtRunForm form

        newtFormDestroy form
        newtPopWindow
        free csText
        free csBack
        free csName

readPrompt :: String -> IO (Maybe String)
readPrompt prompt = do
        csPrompt <- newCString prompt
        csMink <- newCString "Mink"
        csOK <- newCString "OK"
        csCancel <- newCString "Cancel"

        newtCenteredWindow 50 8 csMink
        
        msg <- newtLabel 1 1 csPrompt
        inputE <- newtEntry 1 2 nullPtr 48 nullPtr 0

        ok <- newtButton 1 4 csOK
        cancel <- newtButton 9 4 csCancel

        form <- newtForm (NewtComponent nullPtr) nullPtr 0
        newtFormAddComponent form msg
        newtFormAddComponent form inputE
        newtFormAddComponent form ok
        newtFormAddComponent form cancel

        result <- newtRunForm form
        input <- peekCString =<< newtEntryGetValue inputE
        newtFormDestroy form
        newtPopWindow

        free csMink
        free csOK
        free csCancel
        free csPrompt

        pure $! if result == cancel then Nothing else Just input

getFileString :: FilePath -> FilePath -> IO CString
getFileString dir fp = do
    s' :: Either IOError FileStatus <- try $ getFileStatus (dir </> fp)
    let icon = case s' of
            (Left _) -> ' '
            (Right s) -> if isDirectory s then '\xF413' else '\xEA7B'

    newCString $ icon : "  " <> fp

fromDirStream :: DirStream -> IO [FilePath]
fromDirStream ds = readDirStreamMaybe ds >>= \case
        Nothing -> pure []
        Just e -> (e : ) <$> fromDirStream ds

saveFilePrompt :: FilePath -> FilePath -> BSC.ByteString -> IO ()
saveFilePrompt dir name contents = do
        csTitle <- newCString "Save file to disk"
        csOK <- newCString "OK"
        csCancel <- newCString "Cancel"
        csName <- newCString name

        files <- do
                ds <- openDirStream dir
                ns <- fromDirStream ds
                closeDirStream ds
                pure $ sort ns

        ns <- mapM (getFileString dir) files
        ps <- mapM newStablePtr files
        let es' = zip ns ps
            height = fromIntegral . min 36 . max 16 $ length files
            width = fromIntegral . min 132 . max 50 
                $ foldr (max . (+14) . length) 0 files

        newtCenteredWindow width height csTitle
        list <- newtListbox 2 1 (height - 9) 
                (NEWT_FLAG_SCROLL .|. NEWT_FLAG_BORDER .|. NEWT_FLAG_RETURNEXIT)

        newtListboxSetWidth list (width - 4) 
        forM_ es' $ \(n, p) -> newtListboxAddEntry list n (castStablePtrToPtr p)

        inputE <- newtEntry 2 (height - 7) csName (width - 4) nullPtr 0

        ok <- newtButton 2 (height - 4) csOK
        cancel <- newtButton 10 (height - 4) csCancel

        form <- newtForm (NewtComponent nullPtr) nullPtr 0
        newtFormAddComponent form list
        newtFormAddComponent form inputE
        newtFormAddComponent form ok
        newtFormAddComponent form cancel

        result <- newtRunForm form
        input <- peekCString =<< newtEntryGetValue inputE

        free csTitle
        free csOK
        free csCancel
        free csName
        newtPopWindow

        when (result == list) $ do
                item' <- newtListboxGetCurrent list
                let path' :: StablePtr FilePath
                    path' = castPtrToStablePtr item'
                path <- (dir </>) <$> deRefStablePtr path'
                saveFilePrompt path name contents

        when (result == ok) $ do
                BSC.writeFile (dir </> input) contents

        newtFormDestroy form
        forM_ ns free
        forM_ ps freeStablePtr

clientMain :: IO ()
clientMain = setup *> program `finally` cleanup
        where setup = newtInit *> newtCls
              cleanup = newtFinished
              program = ifHasAddress $ \n p -> runExceptT (gopher n p "") >>= \case
                Left err -> winMessage err
                Right xs -> browse xs
