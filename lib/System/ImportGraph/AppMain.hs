module System.ImportGraph.AppMain where
import           ClassyPrelude
import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Printing            (renderDot)
import qualified Data.GraphViz.Types.Generalised   as G
import           Data.GraphViz.Types.Monadic
import qualified Data.Text.Lazy                    as L
import qualified Data.Text.Lazy.IO                 as L
import           GHC
import           System.ImportGraph.GetIface
import           System.ImportGraph.ModuleCluster
import           System.Process

appMain :: IO ()
appMain = do
    args <- getArgs
    libDir <- getLibDirReadProcess
    runGhc libDir $
        case args of
            ["-h"] -> liftIO $ L.putStrLn help
            ["--help"] -> liftIO $ L.putStrLn help
            [] -> do
                ifaces <- findIfaces
                liftIO . L.putStrLn $ renderGraph ifaces
            [hiPath] -> do
                iface <- getIface $ unpack hiPath
                liftIO . L.putStrLn $ renderGraph [iface]
            _ -> liftIO $ L.putStrLn help

getLibDirReadProcess :: IO (Maybe FilePath)
getLibDirReadProcess = listToMaybe . lines <$> readProcess "ghc" ["--print-libdir"] ""

help :: L.Text
help = unlines [ "Usage: cabal build && haskell-import-graph"
               , "or:    cabal build && haskell-import-graph dist/build/foo/foo-tmp/bar.hi"
               ]

renderGraph :: [ModIface] -> L.Text
renderGraph = renderDot . toDot . importGraph (Str "haskell-import-graph")

importGraph :: GraphID -> [ModIface] -> G.DotGraph L.Text
importGraph graphName mods = digraph graphName $ do
    graphAttrs [Compound True, RankDir FromLeft]
    mapM moduleCluster mods
