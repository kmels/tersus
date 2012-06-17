module TersusCluster.MessageBackend where

import Prelude
import Control.Monad (forever)
import Control.Concurrent (forkIO,threadDelay)
import Remote
import TersusCluster.Types
import Model (TMessage)


-- Function to match a process id sent from another process
matchProcesses :: MatchM [ProcessId] ()
matchProcesses = match return

-- Function to match a Message sent from another process                        
matchTMessage :: MatchM TMessage ()
matchTMessage = match return

-- Function that handles messaging among the multiple Tersus instances
runTersusMessaging :: TMessageQueue -> TMessageQueue -> ActionsChannel -> MailBoxTable -> AddressTable -> TMessageStatusTable -> Int -> ProcessM ()
runTersusMessaging sendChan recvChan addresses mailBoxes actionsChan numThreads = undefined

                                                                     --            do 
                                                                     -- initMessageDeliver sendChan addresses numThreads >>
                                                                     -- initMessageReceiver recvChan mailBoxes 
