import qualified Data.Map as Map
import qualified Control.Concurrent.STM as STM
import qualified Network.WebSockets as WS


type ConnKey = Int

data ReporterMessage = Aha Int | Total Int | Reset

-- reporterBroadcastChan
data Reporter = Reporter
  { reporterNextConnKey :: STM.TVar ConnKey
  , reporterConnections :: STM.TVar (Map.Map ConnKey WS.Connection)
  , reporterAha         :: STM.TVar Int
  , reporterSendChan    :: STM.TChan ReporterMessage
  }


data ViewerMessage = VTotal Int | VReset

-- viewerBroadcastChan
data Viewer = Viewer
  { viewerConnection :: WS.Connection
  , viewerSendChan   :: STM.TChan ViewerMessage
  }


type Caption = String
type ViewerKey = Int
type ReporterKey = String

data Board = Board
  { boardCaption       :: Caption
  , boardNextViewerKey :: STM.TVar ViewerKey
  , boardViewers       :: STM.TVar (Map.Map ViewerKey Viewer)
  , boardReporters     :: STM.TVar (Map.Map ReporterKey Reporter)
  }


type BoardSecretKey = String
type BoardPublicKey = String
  
data Server = Server
  { serverBoards    :: STM.TVar (Map.Map BoardPublicKey Board)
  , serverBoardKeys :: STM.TVar (Map.Map BoardSecretKey BoardPublicKey)
  }




