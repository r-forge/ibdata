################################################################################################
###################################### Basic functionality #####################################
################################################################################################

# Load IBData packages
require(IBData)

# Set client parameters and create eClient. 
# This also creates an eWrapper object for the client to use. Option to use the default 'eWrapper' implementation(which does nothing) 
# or custom eWrapper, in this case 'eWrapper.IBData'. Use wrapperArgs to pass initialization arguments to the custom eWrapper.
clientId <- 1
client <- eClient$new(eWrapperGenerator=eWrapper.IBData, clientId=clientId, wrapperArgs=list(startServer=FALSE))

# Connect client to TWS
client$connect()

# Show client information 
client

#client$trace(subscribeToContracts,browser)
#client$trace(reqMktData,browser)
client$trace(reqHistoricalData,browser)

# You can provide contracts manually and subscribe to them
#futureContracts <- list(twsFuture("ES","GLOBEX","201212"),twsFuture("GC","NYMEX","201212"),twsFuture("GC","NYMEX","201212"))
equityContracts <- list(twsEquity("SPY"),twsEquity("QQQ"),twsEquity("IWM"))
#client$subscribeToContracts(futureContracts)
#client$subscribeToContracts(equityContracts)
client$reqHistoricalData(equityContracts[[1]])

# Start client listening event-loop, hit esc to stop
client$run()

# Examine symbol data
.IBData[["SPY"]] #or...
client$getWrapper()$getSymbolData("SPY")

# You can also use the FinancialInstrument package to define your contracts, although this is far from a complete integration with FI
# First, load your instruments and then subscribe to the symbols. 
# The eClient will look for and try to use instrument definitions for those symbols
require(IBData) ##
require(FinancialInstrument)
load.instruments(system.file("currencies.csv",package="IBData"))

port <- 7789 ##
clientId <- 1 ##
client <- eClient$new(eWrapperGenerator=eWrapper.IBData, clientId=clientId, wrapperArgs=list(port=as.integer(port),startServer=TRUE)) ##
client$connect()
client ##

#client$trace(addSymbolToIdList,browser) ##
#client$trace(reqMktData,browser) ##

client$subscribeToSymbols(ls_exchange_rates())

# Start client listening event-loop, hit esc to stop
client$run()

# And again examine symbol data
.IBData[["AUD.JPY"]]
.IBData[["EUR.USD"]]

# Disconnect client from TWS
client$disconnect()


################################################################################################
################################## IBData server functionality #################################
################################################################################################

# Create client object, providing an Rshare port (if none provided, defaults to 7789)
# Also note startServer = TRUE (the default for 'eWrapper.IBData' objects)
require(IBData)
port <- 7789
clientId <- 1
client <- eClient$new(eWrapperGenerator=eWrapper.IBData, clientId=clientId, wrapperArgs=list(port=as.integer(port),startServer=TRUE))

# Connect client to TWS, and start main client listen event loop
client$connect()
client$run()


# In a new, separate R session, load the IBData package and connect to the IBData server provided by the client
require(IBData)
subscribeToIBDataServer(port=7789)

# Define some contracts and have the client subscribe to them
equityContracts <- list(twsEquity("SPY"),twsEquity("QQQ"),twsEquity("IWM"))
clientSubscribeToContracts(equityContracts)

# Watch data as it updates live
clientGetSymbolData(c("SPY","QQQ","IWM"))

# Disconnect from IBData server
disconnectIBDataClient()