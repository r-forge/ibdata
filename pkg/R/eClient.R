
#' @export
eWrapper.client <- setRefClass("eWrapper.client",
	methods = list(
		# IBData package-specific
		getParent = function() { 
			.IBData$client 
		},

		# small and simple client eWrapper for certain requests/methods providing simple return data processing/aggregating and error handler
		errorMessage = function(curMsg, msg, con, 
								OK = NULL, 
								verbose = TRUE, ...) {
			errNum <- as.numeric(msg[3])
			errMsg <- msg[4]
			
			if (errNum %in% OK || errNum >= 1000) { # allowed error message
				if (isTRUE(verbose)) message(errMsg)
				return(TRUE)
			} else { # disallowed error message
				if (isTRUE(verbose)) warning(errMsg)
				return(FALSE)
			}
		},
		nextValidId = function(curMsg, msg, ...) {
			# just return formatted response
			nextId <- as.integer(msg[2])
			nextId
		},
		managedAccounts = function(curMsg, msg, ...) {
			# just return formatted response
			faAccts <- unlist(strsplit(msg[2],","))
			faAccts
		},
		currentTime = function(curMsg, msg, ...) {
			
		},
		contractDetails = function(curMsg, msg,  ...) {
				# the result is handled within the request itself, so no need to do anything other than return the contractDetails object
				details <- IBrokers:::twsContractDetails(version = msg[1], 
									contract = twsContract(conId = msg[12 + 1], 
										symbol = msg[3], 
										sectype = msg[4], 
										expiry = msg[5], 
										primary = msg[21], 
										strike = msg[5 + 1], 
										right = msg[6 + 1], 
										exch = msg[7 + 1], 
										currency = msg[8 + 1], 
										multiplier = msg[14 + 1], 
										include_expired = "", 
										combo_legs_desc = "", 
										comboleg = "", 
										local = msg[9 + 1]), 
									marketName = msg[10 + 1], 
									tradingClass = msg[11 + 1], 
									conId = msg[12 + 1], 
									minTick = msg[13 + 1],
									orderTypes = unlist(strsplit(msg[15 + 1], ",")), 
									validExchanges = unlist(strsplit(msg[16 + 1], ",")),
									priceMagnifier = msg[17 + 1], 
									underConId = msg[18 + 1], 
									longName = msg[19 + 1], 
									contractMonth = msg[22], 
									industry = msg[23], 
									category = msg[24],
									subcategory = msg[25], 
									timeZoneId = msg[26], 
									tradingHours = msg[27], 
									liquidHours = msg[28])
				details
		}
	), contains = "eWrapper"
)

#' @export
eClient <- setRefClass("eClient",
	fields = list(
		#eWrapperGenerator = "refObjectGenerator",
		wrapper = "eWrapper",
		reader = "eReader",
		clientWrapper = "eWrapper.client",
		clientParameters = "list",
		contracts = "list"),
	methods = list(
		initialize = function(...) {
			# assign eClient to .IBData environment
			assign("client",.self,pos=.IBData) 
			
			# deal with parameters, all dot arguments other than specific start parametersbesides eWrapperGenerator and wrapperArgs are to be stored as parameters
			params <- list(...)
			
			# main eWrapper parameters
			if (is.null(params$eWrapperGenerator)) eWrapperGenerator <- eWrapper else eWrapperGenerator <- params$eWrapperGenerator
			if (is.null(params$wrapperArgs)) wrapperArgs <- list() else wrapperArgs <- params$wrapperArgs
			params$eWrapperGenerator <- NULL
			params$wrapperArgs <- NULL
			
			# FI instruments to contracts?
			#if (isTRUE(params$load.instruments)) .self$addContracts()
			#params$load.instruments <- NULL
			
			if (is.null(params$clientId)) params$clientId <- 1L else params$clientId <- as.integer(params$clientId) # default clientId is 1
			if (is.null(params$verbose)) params$verbose <- TRUE else params$verbose <- as.logical(params$verbose) # default verbose is TRUE
			clientParameters <<- params
			
			# Create eClient-specific eWrapper for certain requests / methods
			clientWrapper <<- eWrapper.client$new()
			
			# Create main user-supplied eWrapper
			wrapper <<- do.call(eWrapperGenerator$new, wrapperArgs)
			
			# Create eReader
			reader <<- eReader$new()
		},
		finalize = function() {
			# Destroy eWrapper?
			# wrapper <<- NULL
			
			# Disconnect from TWS if needed
			.self$eDisconnect()
		},
		show = function() {
			cat("Interactive Brokers eClient \n")
			if (.self$isConnected()) {
				cat("TWS status: Connected","\n")
				if (.self$isSimulated()) cat(" *** SIMULATED TRADING ***\n")
				cat(paste("\t","Connected at:",format(.self$getTws()$connected.at,format="%Y-%m-%d %H:%M:%S %Z")),"\n")
				cat(paste("\t","Port:",.self$getTws()$port),"\n")
				cat(paste("\t","Account Id:",.self$getAccountId()),"\n")
				if (.self$getFAStatus()) {
					cat("* Financial Advisor *\n")
					cat("Managed Accounts:",paste(.self$getFAAccts(),collapse=", "),"\n")
				}
			} else cat("TWS status: Disconnected","\n")
		},
		
		# Parameter functions -- mimic options() and getOption()
		parameters = function(...) {
			dots <- list(...)
			nms <- names(dots)
			if (length(nms) != length(dots)) stop("all arguments to parameters must be named")
			
			retlist <- list()
			params <- .self$clientParameters
			for (i in 1:length(dots)) {
				# mimic options() behavior for NULL params - return them
				if (is.null(params[[nms[i]]])) retlist <- c(retlist, structure(list(NULL), .Names=nms[i])) else retlist[[i]] <- params[[nms[i]]]
				names(retlist)[i] <- nms[i]
				params[[nms[i]]] <- dots[[nms[i]]]
			}
			clientParameters <<- params
			
			invisible(retlist)
		},
		getParameter = function(x, default = NULL) {
			param <- .self$clientParameters[[x]]
			if (is.null(param)) param <- default
			param
		},
		
		# Helper functions for certain parameters
		isSimulated = function() {
			.self$getParameter("simulated")
		},
		getAccountId = function() {
			.self$getParameter("accountId")
		},
		getClientId = function() {
			.self$getParameter("clientId")
		},
		setClientId = function(id) {
			id <- suppressWarnings(as.integer(id))
			if (is.na(id)) stop("id must be coercible to an integer")
			.self$parameters(clientId=id)
		},
		
		# Ticker Id stuff: bind each ticker Id to a symbol
		getNextTickerId = function() {
			idList <- .self$getIdList()
			if (length(idList) > 0) {
				nextId <- as.character(max(0,as.integer(unlist(lapply(idList,"[[","tickerId")))) + 1)
			} else nextId <- as.character(1L)
		},
		getIdList = function() {
			.self$getParameter("idList")
		},
		setIdList = function(idList) {
			.self$parameters(idList = idList)
		},
		
		# TWS connection methods
		isConnected = function() {
			.self$getReader()$connected()
		},
		checkConnected = function(fname) {
			if (!.self$isConnected()) {
				if (!is.null(fname)) stop(paste("Error in ",fname,": must be connected to TWS",sep=""), call.=FALSE) else stop("Error : must be connected to TWS", call.=FALSE)
			}
		},
		getConnection = function() {
			.self$getReader()$getConnection()
		},
		getTws = function() {
			.self$getReader()$getTws()
		},
		eConnect = function(client = c("TWS","IBG"), host = "localhost", port = NULL, timeout = 5, verbose=.self$getParameter("verbose")) {
			if (.self$isConnected()) message("already connected to TWS")
			
			# connect to TWS
			.self$getReader()$connect(.self$getParameter("clientId"), client=client, host=host, port=port, timeout=timeout)
			
			if (verbose) message("Successfully connected to TWS")
		},
		eDisconnect = function(verbose = .self$getParameter("verbose")) {
			if (.self$isConnected()) .self$getReader()$disconnect()
			if (verbose) message("Disconnected from TWS")
		},
		connect = function(client = c("TWS","IBG"), host = "localhost", port = NULL, timeout = 5, verbose = .self$getParameter("verbose")) {
			.self$eConnect(client, host=host, port=port, timeout=timeout, verbose=verbose)
		},
		disconnect = function(verbose = .self$getParameter("verbose")) {
			.self$eDisconnect(verbose)
		},
		
		# eReader pass through methods
		run = function(...) {
			if (isTRUE(.self$getParameter("verbose"))) message("eClient callback loop initated...")
			.self$checkConnected("run")
			.self$getReader()$run(...)
		},
		processMsg = function(curMsg, con, eWrapper=.self$getClientWrapper(), ...) {
			# pass through to eReader method, but using clientWrapper by default (for client requests and such)
			.self$getReader()$processMsg(curMsg, con, eWrapper=eWrapper, ...)
		},
		
		# Advisor methods
		getFAAccts = function() {
			.self$getParameter("advisor")$faAccts
		},
		setFAAccts = function(accts) {
			adv <- .self$getParameter("advisor")
			adv$faAccts <- accts
			.self$parameters(advisor = adv)
		},
		getFAStatus = function() {
			.self$getParameter("advisor")$status
		},
		setFAStatus = function(status) {
			adv <- .self$getParameter("advisor")
			adv$status <- status
			.self$parameters(advisor = adv)
		},
		isAdvisor = function() {
			if (.self$isConnected()) {
				status <- .self$getFAStatus()
				if (is.null(status)) FALSE else status
			} else FALSE
		},
		isFA = function() {
			.self$isAdvisor()
		},
		
		# TickerId list: symbol-tickerId mapping and symbol mkt data subscription status
		addSymbolToIdList = function(symbol, tickerId = .self$getNextTickerId(), set.subscribed = FALSE) {
			tickerId <- as.character(tickerId)
			if (length(tickerId) == 0) stop("invalid tickerId")
			if (length(tickerId) > 1 || length(symbol) > 1) stop("symbol and tickerId may only be of length 1")
			idList <- .self$getIdList()
			
			if (length(idList) > 0) {
				if (symbol %in% names(idList)) stop(paste("symbol",symbol,"already in the ticker id list"))
				if (tickerId %in% lapply(idList,"[[","tickerId")) stop(paste("tickerId",tickerId,"already in the ticker id list"))
			}
			
			idList[[symbol]] <- list(tickerId = tickerId, subscribed=set.subscribed)
			.self$setIdList(idList)
			invisible(tickerId)
		},
		removeSymbolFromIdList = function(symbol) {
			idList <- .self$getIdList()
			if (length(idList) == 0) return(invisible(FALSE))
			
			idx <- which(names(idList) == symbol)
			if (length(idx) == 0) stop(paste(symbol,"is not in the ticker id list"))
			
			idList <- idList[-idx]
			.self$setIdList(idList)
			invisible(TRUE)
		},
		removeTickerIdFromIdList = function(tickerId) {
			tickerId <- as.character(tickerId)
			idList <- .self$getIdList()
			if (length(idList) == 0) return(invisible(FALSE))
			
			idx <- which(idList == tickerId)
			if (length(idx) == 0) stop(paste(tickerId,"is not in the ticker id list"))
			
			idList <- idList[-idx]
			.self$setIdList(idList)
			invisible(TRUE)
		},
		symbolToTickerId = function(symbol) {
			idList <- .self$getIdList()
			if (length(idList) == 0) return(NULL)
			
			idx <- which(names(idList) == symbol)
			if (length(idx) == 0) {
				return(NULL) #stop(paste(symbol,"is not in the ticker id list"))
			}
			as.character(idList[[symbol]]$tickerId) 
		},
		tickerIdToSymbol = function(tickerId) {
			tickerId <- as.character(tickerId)
			idList <- .self$getIdList()
			if (length(idList) == 0) return(NULL)
			
			idx <- which(lapply(idList,"[[","tickerId") == tickerId)
			if (length(idx) == 0) return(NULL) #stop(paste(tickerId,"is not in the ticker id list"))
			
			names(idList)[idx]
		},
		setSubscribed = function(symbol) {
			idList <- .self$getIdList()
		},
		isSubscribed = function(symbol) {
			idList <- .self$getIdList()
			if (is.null(idList[[symbol]]$subscribed)) return(FALSE) else return(idList[[symbol]]$subscribed)
		},
		getSubscribedSymbols = function() {
			idList <- .self$getIdList()
			if (length(idList) == 0) return(NULL)
			
			idx <- which(unlist(lapply(idList,"[[","subscribed"),use.names=FALSE))
			names(idList)[idx]
		},
		inSymbolList = function(symbol) {
			symbol %in% names(.self$getIdList())
		},
		
		
		# Subcription methods
		getContractByConId = function(conId) {
			Contracts <- .self$getContracts()
			contract <- Contracts[which(lapply(Contracts,"[[","conId") == as.character(conId))]
			if (length(contract) == 0) return(NULL)
			contract
		},
		getContractBySymbol = function(symbol) {
			Contracts <- .self$getContracts()
			shortContracts <- lapply(Contracts,"[[","contract")
			contract <- Contracts[which(lapply(shortContracts,"[[","local") == symbol)]
			if (length(contract) == 0) {
				# try to search by generic symbol
				contract <- Contracts[which(lapply(shortContracts,"[[","symbol") == symbol)]
				# what to do for more than one match? 1) stop with error, 2) warn and return all matching contracts 3) just return top contract? not sure which is best
				if (length(contract) > 1) stop(paste("ambiguous symbol ",symbol,": more than one contract found",sep="")) 
				if (length(contract) == 0) return(NULL)
			}
			contract[[1]]
		},
		subscribeToSymbols = function(Symbols, requestData = TRUE) {
			if (!is.character(Symbols)) stop("Symbols must be of type 'character'")
			
			Contracts <- sapply(Symbols,.self$getContractBySymbol,simplify=FALSE)
			idx <- unlist(lapply(Contracts,is.null))
			
			# check if we don't have contracts for some symbols
			if (any(idx)) Contracts[idx] <- symbolsToContracts(Symbols[idx])
			
			.self$subscribeToContracts(Contracts, requestData=requestData)
		},
		subscribeToContracts = function(Contracts, requestData = TRUE, verbose=.self$getParamater("verbose"), ...) {
			.self$checkConnected("subscribeToContracts")
			
			# check Contracts
			if (!is.list(Contracts)) Contracts <- list(Contracts)
			if (!all(sapply(Contracts,function(x) {is.twsContract(x) || inherits(x,"twsContractDetails")}))) stop("Contracts must be of a list of objects of type 'twsContract' or 'twsContractDetails'")
			
			# fetch contract details from TWS if needed
			newContracts <- list()
			for (i in 1:length(Contracts)) {
				if (is.twsContract(Contracts[[i]])) newContracts[[i]] <- .self$reqContractDetails(Contracts[[i]]) else newContracts[[i]] <- Contracts[[i]]
			}
			newConIds <- unlist(lapply(newContracts,"[[","conId"),use.names=FALSE)
			currentContracts <- .self$getContracts()
			currentConIds <- unlist(lapply(currentContracts,"[[","conId"),use.names=FALSE)
			
			# overwrite stored contracts with existing conId and append the rest
			idx <- which(newConIds %in% currentConIds)
			if (length(idx) > 0) {
				finalContracts <- currentContracts
				for (i in 1:length(idx)) {
					cIdx <- which(currentConIds == newConIds[idx[i]])
					finalContracts[[cIdx]] <- newContracts[[idx[i]]]
				}
				finalContracts <- c(finalContracts, newContracts[-idx])
			} else finalContracts <- c(currentContracts, newContracts)
			
			.self$setContracts(finalContracts)
			
			# we are subscribing to contracts in newContracts, check if subscribed, if not reqMktData
			for (i in 1:length(newContracts)) {
				contract <- newContracts[[i]]$contract
				symbol <- ifelse(is.null(contract$local), contract$symbol, contract$local)
				if (!.self$inSymbolList(symbol)) {
					tickerId <- .self$addSymbolToIdList(symbol)
				} else {
					tickerId <- symbolToTickerId(symbol)
				}
				if (isTRUE(requestData)) {
					if (!.self$isSubscribed(symbol)) {
						.self$reqMktData(contract, tickerId=tickerId, ...)
						.self$setSubscribed(symbol)
					} else if (verbose) {
						warning(paste("already subscribed to symbol",symbol))
					}
				}
			}
		},
		unsubscribeFromContracts = function(Contracts) {
			if (!all(sapply(Contracts,function(con) inherits(con,"twsContract")))) stop("Contracts must be of type 'twsContract'")
			
			Symbols <- .self$contractsToSymbols(Contracts)
			tickerIds <- sapply(Symbols,symbolToTickerId)
			
		},	
		
		# TWS API Requests
		reqCurrentTime = function() {
			.self$checkConnected("reqCurrentTime")
			
			con <- .self$getConnection()
			writeBin(.twsOutgoingMSG$REQ_CURRENT_TIME,con)
			writeBin('1',con)
		},
		reqFAIds = function() {
			.self$checkConnected("reqFAIds")
			
			con <- .self$getConnection()
			VERSION <- "1"
			writeBin(.twsOutgoingMSG$REQ_MANAGED_ACCTS, con)
			writeBin(VERSION, con)
			
			faIds <- NULL
			while (TRUE) {
				socketSelect(list(con), FALSE, NULL)
				curMsg <- readBin(con, character(), 1)
				if (curMsg != .twsIncomingMSG$MANAGED_ACCOUNTS) {
					if (curMsg == .twsIncomingMSG$ERR_MSG) {
						## TODO: write proper error handler for this
						if (!IBrokers:::errorHandler(con, verbose=.self$getParameter("verbose"), OK = c(165, 300, 366, 2104, 2106, 2107))) {
							warning("error in nextValidId")
							break
						}
					} else {
						# just deal with other message in usual way
						.self$processMsg(curMsg, con)
					}
				} else { 
					# return
					faIds <- c(faIds, .self$processMsg(curMsg, con))
					break
					#if (length(faIds) >= numIds) break
				}
			}
			faIds
		},
		reqIds = function(numIds = 1) {
			.self$checkConnected("reqIds")
			con <- .self$getConnection()
			
			VERSION <- "1"
			writeBin(.twsOutgoingMSG$REQ_IDS, con)
			writeBin(VERSION, con)
			writeBin(as.character(numIds), con)
			
			nextIds <- NULL
			while (TRUE) {
				socketSelect(list(con), FALSE, NULL)
				curMsg <- readBin(con, character(), 1)
				if (curMsg != .twsIncomingMSG$NEXT_VALID_ID) {
					if (curMsg == .twsIncomingMSG$ERR_MSG) {
						## TODO: write proper error handler for this
						if (!IBrokers:::errorHandler(con, verbose=.self$getParameter("verbose"), OK = c(165, 300, 366, 2104, 2106, 2107))) {
							warning("error in nextValidId")
							break
						}
					} else {
						# just deal with other message using normal eWrapper
						.self$processMsg(curMsg, con, eWrapper=.self$getWrapper())
					}
				} else { 
					# return
					nextIds <- c(nextIds, .self$processMsg(curMsg, con))
					if (length(nextIds) >= numIds) break
				}
			}
			nextIds
		},
		cancelAccountUpdates = function(acctCode="1") {
			.self$checkConnected("cancelAccountUpdates")
			conn <- getTws()
			
			.reqAccountUpdates(conn, "0", acctCode)
		},
		reqAccountUpdates = function(acctCode="1", stay.subscribed = FALSE) {
			.self$checkConnected("reqAccountUpdates")
			conn <- getTws()
			
			VERSION <- "2"
			.reqAccountUpdates(conn, "1", acctCode) # subscribe
			if (!stay.subscribed) .reqAccountUpdates(conn, "0", acctCode) # and then unsubscribe
		},
		reqMktData = function(Contracts, tickerId = NULL, tickGenerics = "100,101,104,106,165,221,233,236") {
			.self$checkConnected("reqMktData")
			con <- .self$getConnection()
			
			if (is.twsContract(Contracts)) Contracts <- list(Contracts)
			if (!all(sapply(Contracts,function(contract) is.twsContract(contract)))) stop("Contracts must be of a list of objects of type 'twsContract'")
			
			snapshot <- "0"
			VERSION <- "9"
			
			# request contracts
			ret <- character(length(Contracts))
			for (i in 1:length(Contracts)) {
				# if tickerId NULL try to get from client list
				symbol <- ifelse(is.null(Contracts[[i]]$local), Contracts[[i]]$symbol, Contracts[[i]]$local)
				if (!.self$inSymbolList(symbol)) {
					if (!is.null(tickerId)) {
						if (length(tickerId) == length(Contracts)) {
							contractTickerId <- .self$addSymbolToIdList(symbol, tickerId=as.character(tickerId[i]))
						} else if (i == 1) {
							contractTickerId <- .self$addSymbolToIdList(symbol, tickerId=as.character(tickerId[1]))
						} else {
							contractTickerId <- .self$addSymbolToIdList(symbol)
						}
					} else contractTickerId <- .self$addSymbolToIdList(symbol)
				} else {
					if (.self$isSubscribed(symbol)) {
						warning(paste("already subscribed to symbol",symbol))
						next
					}
					contractTickerId <- symbolToTickerId(symbol) #just use symbol tickerId
				}
			
				req <- c(.twsOutgoingMSG$REQ_MKT_DATA, VERSION, contractTickerId, 
					Contracts[[i]]$conId, Contracts[[i]]$symbol, 
					Contracts[[i]]$sectype, Contracts[[i]]$expiry, 
					Contracts[[i]]$strike, Contracts[[i]]$right, Contracts[[i]]$multiplier, 
					Contracts[[i]]$exch, Contracts[[i]]$primary, Contracts[[i]]$currency, 
					Contracts[[i]]$local, "0", tickGenerics, snapshot)
				writeBin(req, con)
				ret[i] <- contractTickerId
			}
			invisible(ret)
		},
		cancelMktData = function(tickerIds, flagUnsubscribed = TRUE) {
			.self$checkConnected("cancelMktData")
			con <- .self$getConnection()
			
			for (i in 1:length(tickerIds)) {
				writeBin(.twsOutgoingMSG$CANCEL_MKT_DATA, con)
				writeBin("1", con)
				writeBin(tickerIds[i], con)
				
				if (flagUnsubscribed) .self$removeTickerIdFromIdList(tickerId)
			}
			
		},
		reqContractDetails = function (Contracts, reqId = "1", verbose = .self$getParameter("verbose"), ...) {
			.self$checkConnected("reqContractDetails")
			
			if (is.twsContract(Contracts)) Contracts <- list(Contracts)
			if (!all(sapply(Contracts,function(con) is.twsContract(con)))) stop("Contracts must be of a list of objects of type 'twsContract'")
			
			con <- .self$getConnection()
			
			for (Contract in Contracts) {
				VERSION <- "6"

				msg <- c(.twsOutgoingMSG$REQ_CONTRACT_DATA,
					VERSION,
					reqId,
					Contract$conId,
					Contract$symbol,
					Contract$sectype,
					Contract$expiry,
					Contract$strike,
					Contract$right,
					Contract$multiplier,
					Contract$exch,
					Contract$currency,
					Contract$local,
					Contract$include_expired,
					Contract$secIdType,
					Contract$secId)
				writeBin(as.character(msg), con)
				#reqId <- as.character(as.integer(reqId)+1L)
			}

			contractDetails <- list()
			badSymbols <- NULL
			while (TRUE) {
				socketSelect(list(con), FALSE, NULL)
				curMsg <- readBin(con, character(), 1)
				if (curMsg != .twsIncomingMSG$CONTRACT_DATA) {
					if (curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
						.self$processMsg(curMsg, con, eWrapper=.self$getClientWrapper())
						
						if (length(contractDetails) >= length(Contracts)) break
					} else if (curMsg == .twsIncomingMSG$ERR_MSG) {
						## TODO: write proper error handler for this
						badSymbols <- c(badSymbols,Contracts[[length(contractDetails) + length(badSymbols) + 1]]$symbol)
						if (!.self$processMsg(curMsg, con, eWrapper=.self$getClientWrapper(), OK = c(165, 300, 366), verbose=verbose)) {
							# warning("error in contract details")
							break
						}
					} else {
						# just deal with other message in usual way
						.self$processMsg(curMsg, con, eWrapper=.self$getWrapper())
					}
				} else { 
					# contractDetails object returned from clientWrapper
					contractDetails[[length(contractDetails) + 1]] <- .self$processMsg(curMsg, con, eWrapper=.self$getClientWrapper())
					#if (length(contractDetails) == length(Contracts)) break
				}
			}
			if (length(contractDetails) == 1) contractDetails[[1]] else contractDetails
		},
		reqFundamentalData = function(Contract, reportType, reqId = "1") {
			.self$checkConnected("reqFundamentalData")
			
			if (!is.twsContract(Contract)) stop("Contract must be of type 'twsContract'")
			
			con <- .self$getConnection()
			
			VERSION <- "1"
			msg <- c(.twsOutgoingMSG$REQ_FUNDAMENTAL_DATA, VERSION, reqId,
				Contract$symbol, Contract$sectype, Contract$exch, Contract$primary,
				Contract$currency, Contract$local, reportType)
			writeBin(as.character(msg), con)
		},
		cancelFundamentalData = function(reqId = "1") {
			.self$checkConnected("cancelFundamentalData")
			
			con <- .self$getConnection()
			
			VERSION <- "1"
			msg <- c(.twsOutgoingMSG$CANCEL_FUNDAMENTAL_DATA, VERSION, reqId)
			writeBin(as.character(msg), con)
		},
		reqHistoricalData = function(Contract, endDateTime, barSize = "1 day", duration = "1 M",
			useRTH = "1", whatToShow = "TRADES",timeFormat = "1", tzone = "",
			verbose = .self$getParameter("verbose"), tickerId = NULL, eventHistoricalData, file) {
		
			.self$checkConnected("reqHistoricalData")
			
			if (!is.twsContract(Contract)) stop("Contract must be of type 'twsContract'")
			
			con <- .self$getConnection()
			
			if (is.null(tickerId)) tickerId <- .self$symbolToTickerId(ifelse(is.null(Contracts$local),Contracts$symbol,Contracts$local))
			if (is.null(tickerId)) { # no tickerId provided, so generate one and then increment
				tickerId <- .self$getNextTickerId()
				autoIncrement <- TRUE
			} else { #tickerId provided
				tickerId <- as.character(tickerId)
				autoId <- FALSE
				if (length(tickerId) > 1) { # multiple tickerIds provided, don't increment or generate
					if (length(tickerId) != length(Contracts)) stop("tickerId length does not match Contracts length")
					tickerIds <- tickerId
					tickerId <- tickerIds[1]
					autoIncrement <- FALSE
				} else { # single tickerIdProvided, increment each time
					autoIncrement <- TRUE
				}
			}
			
			if (!missing(endDateTime) && length(endDateTime) > 1) {
				if (!timeBased(endDateTime)) stop("endDateTime length greater than 2 needs to be timeBased")
				sleep <- 0
				rHDargs <- list(Contract=Contract,
					barSize=barSize, duration=duration,
					useRTH=useRTH, whatToShow=whatToShow,
					timeFormat=timeFormat, tzone=tzone, verbose=verbose, tickerId=as.character(tickerId))
				if (!missing(eventHistoricalData)) rHDargs$eventHistoricalData <- eventHistoricalData
				if (!missing(file)) rHDargs$file <- file
				x <- lapply(format(endDateTime,"%Y%m%d 23:59:59"),
					function(eDT) {
						rHDargs$endDateTime <- eDT
						xx <- try(do.call(.self$reqHistoricalData, rHDargs), silent=TRUE)
						Sys.sleep(10)
						if (inherits(xx, "try-error")) return(NULL)
						return(xx)
					})
				x <- do.call("rbind.xts",x)
				return(x[-which(duplicated(.index(x)))])
			}
			
			on.exit(.self$cancelHistoricalData(as.character(tickerId)))

			validBarSize <- c("1 secs","5 secs","15 secs","30 secs",
				"1 min", "2 mins","3 mins","5 mins","15 mins",
				"30 mins","1 hour","1 day","1 week","1 month",
				"3 months","1 year")
			if (!barSize %in% validBarSize) stop(paste("unknown barSize try: ",paste(validBarSize,sep=";")))

			if (missing(endDateTime) || is.null(endDateTime)) {
				endDateTime <- strftime(as.POSIXlt(as.POSIXct("1970-01-01")+as.numeric(.self$reqCurrentTime())), format="%Y%m%d %H:%M:%S",usetz=FALSE)
			}
			
			VERSION <- "4"
			msg <- c(.twsOutgoingMSG$REQ_HISTORICAL_DATA, 
			VERSION,
			as.character(tickerId),
				Contract$symbol, Contract$sectype,
				Contract$expiry, Contract$strike,
				Contract$right,  Contract$multiplier,
				Contract$exch,   Contract$primary,
				Contract$currency, Contract$local,
				Contract$include_expired,
				endDateTime, barSize, duration, useRTH,
				whatToShow, timeFormat)
				#  msg <- c("20","4","1",
				#               "QQQQ","STK","",
				#               "0.0","","",
				#               "SMART","ISLAND","USD",
				#               "","0","20080219 21:11:41 GMT",
				#               "1 day","1 M","1",
				#               "TRADES","1")

			writeBin(msg, con)

			waiting <- TRUE           # waiting for valid response?
			response <- character(0)  # currently read response

			if (verbose) {
				cat("waiting for TWS reply on",Contract$local,"...")
				iter <- 1
				flush.console()
			}

			while (waiting) {
				if (!socketSelect(list(con), FALSE, 0.25)) next
				
				curMsg <- readBin(con,character(),1)
				if (verbose) {
					cat(".")
					if (iter %% 30 == 0) cat("\n")
					flush.console()
					iter <- iter + 1
					#Sys.sleep(0.25)
				}

				if (length(curMsg) > 0) {
					# watch for error messages
					if (curMsg == .twsIncomingMSG$ERR_MSG) {
						if (!errorHandler(con,verbose=verbose,OK=c(165,300,366,2104,2106,2107))) {
						cat("failed.\n")
						#stop("Unable to complete historical data request", call.=FALSE)
						on.exit()
						invisible(return())
						}
					}
					# watch for historical data start
					if (curMsg == .twsIncomingMSG$HISTORICAL_DATA) {
						header <- readBin(con,character(),5)
						nbin <- as.numeric(header[5])*9
						req.from <- header[3]
						req.to   <- header[4]
						Sys.sleep(2) # add delay for Windows issues - readBin on M$ is bad, bad, bad...
						response <- readBin(con,character(),nbin)
						waiting <- FALSE
						if (verbose) {
							cat(" done.\n")
							flush.console()
						}
						on.exit()
					}
				}
			}

			if (missing(eventHistoricalData)) {
				# the default: return an xts object
				cm <- matrix(response,ncol=9,byrow=TRUE)
				cm[,8] <- ifelse(cm[,8]=="false",0,1)
				if (timeFormat==2 && !nchar(cm[1,1]) > 8) {  # IB ignores the timeFormat if daily returns
					dts <- structure(as.numeric(cm[,1]), class=c("POSIXct","POSIXt"), tzone=tzone)
				} else {
					dts <- structure(as.numeric(as.POSIXlt(gsub("(\\d{4})(\\d{2})(\\d{2})","\\1-\\2-\\3",cm[,1],perl=TRUE))),
					class=c("POSIXct","POSIXt"), tzone=tzone)
				}

				# if file is specified - dump to file instead
				if (!missing(file)) {
					cm[,1] <- dts
					write.table(cm,
						file=file,
						quote=FALSE,
						row.names=FALSE,
						col.names=FALSE,
						sep=",")
					invisible(return())
				}

				#x <- xts(matrix(as.numeric(cm[,-1]),nc=8),order.by=structure(as.numeric(as.POSIXlt(dts)), class=c("POSIXt", "POSIXct")))
				x <- xts(matrix(as.numeric(cm[,-1]),ncol=8),order.by=dts, tzone=tzone)
				localsymbol <- .self$reqContractDetails(Contract)$contract$local
				colnames(x) <- paste(localsymbol, c("Open","High","Low","Close","Volume","WAP","hasGaps","Count"), sep=".")
				xtsAttributes(x) <- list(from=req.from, to=req.to, src="IB", updated=Sys.time())
				return(x)
			} else if (is.null(eventHistoricalData)) {
				# return raw TWS data including header
				return(c(header,response))
			} else {
				# pass to callback function
				FUN <- match.fun(eventHistoricalData)
				return(FUN(c(header,response)))
			}
		},
		cancelHistoricalData = function(tickerId) {
			.self$checkConnected("cancelHistoricalData")
			
			con <- .self$getConnection()
			
			writeBin(.twsOutgoingMSG$CANCEL_HISTORICAL_DATA, con)
			writeBin("1", con)
			writeBin(as.character(tickerId), con)
		},
		reqHistory = function(Contract, barSize="1 min", ...) {
			if (barSize == "1 min") {
				endDateTime <- Sys.Date()-seq(360, 0, -5)
				duration <- "5 D"
			} else if (barSize == "15 mins") {
				endDateTime <- Sys.Date()-seq(360, 0, -10)
				duration <- "10 D"
			}
			.self$reqHistoricalData(Contract, barSize=barSize, duration=duration, endDateTime=endDateTime, ...)
		},

		# IB FA requests
		reqManagedAccts = function() {
			.self$checkConnected("reqManagedAccts")
			con <- .self$getConnection()

			VERSION <- "1"
			writeBin(c(.twsOutgoingMSG$REQ_MANAGED_ACCTS, VERSION), con)
			
			faAccts <- NULL
			while (TRUE) {
				socketSelect(list(con), FALSE, NULL)
				curMsg <- readBin(con, character(), 1)
				if (curMsg != .twsIncomingMSG$MANAGED_ACCTS) {
					if (curMsg == .twsIncomingMSG$ERR_MSG) {
						# send to clientWrapper error handler
						if (!.self$processMsg(curMsg, con, eWrapper=.self$getClientWrapper(), OK = c(165, 300, 366), verbose=.self$getParameter("verbose"))) {
							#stop("unkown error in reqManagedAccts")
							break
						}
					} else {
						# deal with other message using main eWrapper
						.self$processMsg(curMsg, con)
					}
				} else { 
					# managed acct data -- use clientWrapper
					faAccts <- .self$processMsg(curMsg, con, eWrapper=.self$getClientWrapper())
					break
				}
			}
			return(faAccts)
		},
		requestFA = function(faDataType = "groups") {
			if (!is.element("XML",installed.packages()[,1])) stop("package 'XML' must be installed to use FA account allocation profile and group requests")

			.self$checkConnected("requestFA")
			con <- .self$getConnection()

			if (!tolower(faDataType) %in% c("groups","profile","aliases")) stop("faDatatype must be one of 'groups', 'profile' or 'aliases'")
			faVals <- c("groups","profile","aliases")
			faDataVal <- which(faVals == match.arg(faDataType, faVals))

			VERSION <- "1"
			writeBin(c(.twsOutgoingMSG$REQ_FA, VERSION, as.character(faDataVal)), con)
			
			faResponse <- NULL
			while (TRUE) {
				socketSelect(list(con), FALSE, NULL)
				curMsg <- readBin(con, character(), 1)
				if (curMsg != .twsIncomingMSG$RECEIVE_FA) {
					if (curMsg == .twsIncomingMSG$ERR_MSG) {
						## TODO: write proper error handler for this
						if (!.self$processMsg(curMsg, con, eWrapper=.self$getClientWrapper(), OK = c(165, 300, 366), verbose=.self$getParameter("verbose"))) {
							#warning("error in requestFA.IBData")
							break
						}
					} else {
						# deal with other message using main eWrapper
						.self$processMsg(curMsg, con)
					}
				} else { 
					# managed acct data -- use clientWrapper
					faResponse <- .self$processMsg(curMsg, con, eWrapper=.self$getClientWrapper())
					break
				}
			}
			faResponse
		},
		replaceFA = function(faDataType = "groups", xml) {
			.self$checkConnected("replaceFA")
			con <- .self$getConnection()

			faVals <- c("groups","profile","aliases")
			faDataVal <- which(faVals == match.arg(faDataType, faVals))

			VERSION <- "1"
			writeBin(c(.twsOutgoingMSG$REPLACE_FA, VERSION, as.character(faDataVal), as.character(xml)), con)
		}
	)
)
eClient$accessors("reader","wrapper","clientWrapper","contracts")
eClient$lock("reader","wrapper","clientWrapper")
