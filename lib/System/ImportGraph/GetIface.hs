module System.ImportGraph.GetIface where
import           BinIface       (CheckHiWay (..), TraceBinIFaceReading (..), readBinIface)

import           GHC            (Ghc, ModIface, getSession, getSessionDynFlags, setSessionDynFlags)
import           Prelude        (IO, lines, mapM, ($), (.), (=<<))
import           System.IO      (FilePath)
import           System.Process (readProcess)
import           TcRnMonad      (initTcRnIf, liftIO, (<$>))

findIfaces :: Ghc [ModIface]
findIfaces = do
    his <- liftIO findHiFiles
    _ <- setSessionDynFlags =<< getSessionDynFlags
    mapM getIface his

findHiFiles :: IO [FilePath]
findHiFiles = lines <$> readProcess "find" ["-iname", "*.hi"] ""

getIface :: FilePath -> Ghc ModIface
getIface filename = do
    hsc_env <- getSession
    liftIO . initTcRnIf 's' hsc_env () () $ readBinIface IgnoreHiWay QuietBinIFaceReading filename
