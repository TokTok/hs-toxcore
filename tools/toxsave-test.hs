module Main (main) where

import qualified Data.Binary             as Binary
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import           Foreign.C.String        (CString, peekCString)
import           Foreign.C.Types         (CInt (..), CSize (..))
import           Foreign.Ptr             (FunPtr, Ptr, nullPtr)
import           Network.Tox.SaveData    (SaveData)
import           Test.QuickCheck         (Args (..), Property, quickCheckWith,
                                          stdArgs)
import           Test.QuickCheck.Monadic (assert, monadicIO, run)

foreign import ccall tox_options_new :: Ptr () -> IO (Ptr ())
foreign import ccall tox_options_free :: Ptr () -> IO ()

foreign import ccall tox_options_set_savedata_type :: Ptr () -> CInt -> IO ()
foreign import ccall tox_options_set_savedata_data :: Ptr () -> CString -> CSize -> IO ()

foreign import ccall tox_new :: Ptr () -> Ptr () -> IO (Ptr ())
foreign import ccall tox_kill :: Ptr () -> IO ()

type LogCb = Ptr () -> CInt -> CString -> CInt -> CString -> CString -> Ptr () -> IO ()
foreign import ccall tox_options_set_log_callback :: Ptr () -> FunPtr LogCb -> IO ()
foreign import ccall "wrapper" wrapLogCb :: LogCb -> IO (FunPtr LogCb)

logLevelName :: CInt -> Char
logLevelName 0 = 'T'
logLevelName 1 = 'D'
logLevelName 2 = 'I'
logLevelName 3 = 'W'
logLevelName 4 = 'E'
logLevelName _ = '?'

logHandler :: LogCb
logHandler _ cLevel cFile line cFunc cMsg _ = do
    file <- peekCString cFile
    func <- peekCString cFunc
    msg <- peekCString cMsg
    case cLevel of
        0 -> return ()
        _ -> putStrLn $ logLevelName cLevel : ' ' : file <> ":" <> show line <> "(" <> func <> "): " <> msg

prop_Save :: SaveData -> Property
prop_Save save = monadicIO $ do
    ok <- run $ BS.useAsCStringLen (LBS.toStrict (Binary.encode save)) $ \(saveData, saveLenInt) -> do
        putStrLn $ "\nsavedata size: " <> show saveLenInt
        opts <- tox_options_new nullPtr
        tox_options_set_savedata_type opts 1
        tox_options_set_savedata_data opts saveData (fromIntegral saveLenInt)
        tox_options_set_log_callback opts =<< wrapLogCb logHandler
        tox <- tox_new opts nullPtr
        tox_kill tox
        tox_options_free opts
        return $ tox /= nullPtr
    assert ok


main :: IO ()
main = quickCheckWith stdArgs{maxSuccess=100, maxSize=30} prop_Save
