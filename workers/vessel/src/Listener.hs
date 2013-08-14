
main = listenTo (fleetEvents currentHost) fleetEventHandler

fleetEventHandler :: FleetEvent -> IO ()
fleetEventHandler (VesselEvent e roles) = case e of
	RolesRemoved
	RolesAdded
	Start
	Shutdown
	Halt
	Reload
	UpdateContainers
	UpdateSelf
	TopologyUpdated
	HandleRoleInstructions role instructions -> do
    mContainer <- find role <$> listContainers
		case mContainer of
			Nothing -> return ()
			Just c -> do
				inputStream <- attachContainer c
				write inputStream instructions

data Role
  = PostgresMaster
	| PostgresSlave
	| RedisMaster
	| RedisSlave
	| RabbitCluster
	| ElasticSearchCluster
	| RabbitWorker
	| NginxActive
	| NginxPassive
	| HAProxy
	| Site
	| Scheduler
