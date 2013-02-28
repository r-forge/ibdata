#' Start IBData Server
#'
#' This function is basically a higher level wrapper to the eClient. It creates an eClient object and initializes it with the eWrapper.IBData wrapper. 
#' This function also initializes the IBData server, allowing other R sessions to subscribe to market data from this eClient as well as issue commands to the client.
#' @export
startIBDataServer <- function(Contracts = NULL, use.instruments = FALSE, clientId = 1, port = 7789, startServer = FALSE, stopOnExit = FALSE) {
	# if (!use.instruments && is.null(Contracts)) stop("Contracts must be provided if use.instruments is FALSE")
	if (use.instruments) require(FinancialInstrument)
	
	# Initialize eClient using IBData eWrapper and connect to TWS
	client <- eClient$new(eWrapperGenerator=eWrapper.IBData, clientId=clientId, wrapperArgs=list(port=as.integer(port),startServer=startServer))
	client$connect()
	
	if (stopOnExit) on.exit(stopIBDataServer(client))
	
	if (!is.null(Contracts)) {
		client$subscribeToContracts(Contracts)
	} else if (use.instruments) {
		client$subscribeToSymbols(ls_instruments())
	}
	
	# Initiate callback
	client$run()
}

#' @export
stopIBDataServer <- function(client = .IBData$client) {
	# probably should stop Rshare too
	client$disconnect()
}

#' Subscribe to the Quote Server
#' 
#' @param refreshRate the minimum time in seconds between consecutive exchanges of data between the quote server session and this session
#' @param port the port number of IBData server / Rshare session
#' @export
subscribeToIBDataServer <- function(port = 7789, quietly = FALSE) {
	result <- try(startRshare(port=port, client.only=TRUE),silent=TRUE)
	
	if (inherits(result,"try-error")) {
		if (isTRUE(quietly)) {
			ret <- FALSE
		} else stop(paste("Unable to connect to IBData server on port",port))
	} else {
		# Set as subscribed
		set.subscribed(port, TRUE)
		ret <- TRUE
	}
	invisible(ret)
}

set.subscribed <- function(port, val = TRUE) {
	if (!is.logical(val)) stop("subscribed must be a logical value")
	.IBData$port <- port
	.IBData$subscribed <- val
}

#' @export
is.subscribed <- function() {
	if (is.null(.IBData$subscribed)) subscribed <- FALSE else subscribed <- .IBData$subscribed
	subscribed
}

# Client requests
#' @export
clientGetSymbolData <- function(Symbols = NULL, fields = NULL, port = .IBData$port) {
	if (!is.subscribed()) stop("must be subscribed to IBData server")
	
	req <- structure(list(Symbols = Symbols, fields = fields), class="symbolDataReq")
	res <- sendRshare(req, port, block=TRUE)
	res
}

#' @export
clientReqContractDetails <- function (Contracts, reqId = "1", ..., port = .IBData$port) {
	if (!is.subscribed()) stop("must be subscribed to IBData server")
	
	req <- structure(c(list(Contracts = Contracts, reqId = reqId), list(...)), class="contractDetailsReq")
	res <- sendRshare(req, port, block=TRUE)
	res
}

#' @export
clientSubscribeToContracts <- function (Contracts, ..., port = .IBData$port) {
	if (!is.subscribed()) stop("must be subscribed to IBData server")
	
	req <- structure(c(list(Contracts = Contracts), list(...)), class="subscribeToContractsReq")
	res <- sendRshare(req, port, block=FALSE)
	res
}

# reqCurrentTime = function() #ret
# reqFAIds = function() #ret
# reqIds = function(numIds = 1) #ret
# cancelAccountUpdates = function(acctCode="1") 
# reqAccountUpdates = function(acctCode="1") 
# reqMktData = function(Contracts, tickGenerics = "100,101,104,106,165,221,233,236", ...) 
# cancelMktData = function(tickerIds) 
# reqContractDetails = function (Contracts, reqId = "1", verbose = FALSE, ...) #ret
# reqManagedAccts = function()  #ret
# requestFA = function(faDataType = "groups") #ret
# replaceFA = function(faDataType = "groups", xml) 
# subscribeToSymbols
# subscribeToContracts
# unsubscribeFromSymbols
# unsubscribeFromContracts