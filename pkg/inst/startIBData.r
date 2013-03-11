require(IBData)
port <- 7789L
clientId <- 2L
client <- eClient$new(eWrapperGenerator=eWrapper.IBData, clientId=clientId, wrapperArgs=list(port=port,startServer=TRUE))

# Connect client to TWS, and start main client listen event loop
client$connect()
client$run()