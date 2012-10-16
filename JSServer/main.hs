import Network.Wai.Handler.Warp
import Network.Wai

import qualified Database.Redis as R

import Control.Monad.Reader

import JSServer.App

main :: IO ()
main = do
  conn <- R.connect R.defaultConnectInfo{R.connectHost="127.0.0.1"}
  run 3001 (\rq -> runReaderT (app rq) conn)

