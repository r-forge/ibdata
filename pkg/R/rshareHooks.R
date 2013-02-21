# Hook for objects of type "symbolDataReq" -- simple quote request
symbolDataReqHook <- function(obj, port) {
	Symbols <- obj$Symbols
	fields <- obj$fields
	
	#Symbols <- Symbols[which(Symbols %in% ls(.IBData))]
	symData <- as.list(.IBData)[Symbols]
	names(symData) <- Symbols
	
	if (!is.null(fields)) symData[fields] else symData
}

contractDetailsReqHook <- function (obj, port) {
	do.call(.IBData$client$reqContractDetails,obj)
}

## TODO: implement these request hooks
# subscribeToSymbols
# subscribeToContracts

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
			