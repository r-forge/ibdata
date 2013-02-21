
#' @export
eWrapper <- setRefClass("eWrapper",
	methods = list(
	# IBData package-specific
	getParent = function() { .IBData$eClient },

	# IB API
	tickPrice = function(curMsg, msg, ...) {},
	tickSize = function(curMsg, msg, ...) {},
	tickOptionComputation = function(curMsg, msg, ...) {},
	tickGeneric = function(curMsg, msg, ...) {},
	tickString = function(curMsg, msg, ...) {},
	tickEFP = function(curMsg, msg, ...) {},
	orderStatus = function(curMsg, msg,  ...) {},
	errorMessage = function(curMsg, msg, twsconn, ...) {},
	openOrder = function(curMsg, msg,  ...) {},
	openOrderEnd = function(curMsg, msg,  ...) {},
	updateAccountValue = function(curMsg, msg,  ...) {},
	updatePortfolio = function(curMsg, msg,  ...) {},
	updateAccountTime = function(curMsg, msg,  ...) {},
	accountDownloadEnd = function(curMsg, msg,  ...) {},
	nextValidId = function(curMsg, msg,  ...) {},
	contractDetails = function(curMsg, msg,  ...) {},
	bondContractDetails = function(curMsg, msg,  ...) {},
	contractDetailsEnd = function(curMsg, msg,  ...) {},
	execDetails = function(curMsg, msg,  ...) {},
	execDetailsEnd = function(curMsg, msg,  ...) {},
	updateMktDepth = function(curMsg, msg,  ...) {},
	updateMktDepthL2 = function(curMsg, msg, ...) {},
	updateNewsBulletin = function(curMsg, msg,  ...) {},
	managedAccounts = function(curMsg, msg,  ...) {},
	receiveFA = function(curMsg, msg,  ...) {},
	historicalData = function(curMsg, msg,  ...) {},
	scannerParameters = function(curMsg, msg,  ...) {},
	scannerData = function(curMsg, reqId, rank, contract, distance, benchmark, projection, legsStr) {},
	scannerDataEnd = function(curMsg, msg,  ...) {},
	realtimeBars = function(curMsg, msg,  ...) {},
	currentTime = function(curMsg, msg,  ...) {},
	fundamentalData = function(curMsg, msg,  ...) {},
	deltaNeutralValidation = function(curMsg, msg,  ...) {},
	tickSnapshotEnd = function(curMsg, msg,  ...) {})
)

#' @export
eWrapper.IBData <- setRefClass("eWrapper.IBData",
	fields = list(
		port = "integer"),
	methods = list(
		initialize = function(...) {
			# initialize fields
			params <- list(...)
			if (is.null(params$verbose)) verbose <- TRUE else verbose <- params$verbose
			
			if (is.null(params$port)) {
				if (verbose) message("no Rshare port specified: using default port of 7789")
				port <<- 7789L
			} else port <<- as.integer(params$port)
			
			# try to start Rshare server
			if (is.null(params$startServer)) startServer <- TRUE else startServer <- as.logical(params$startServer)
			if (startServer) { # should be its own function probably
				res <- try(startRshare(port=port, server.only=TRUE, verbose=verbose), silent=TRUE)
				if (inherits(res,"try-error")) stop(paste("Error initializing eWrapper : unable to start Rshare server on port",port),call.=FALSE)
				
				# register Rshare hooks for accessing market data and controlling client
				registerRshareHook("symbolDataReq",symbolDataReqHook,port=port,doResponse=TRUE)
				registerRshareHook("contractDetailsReq",contractDetailsReqHook,port=port,doResponse=TRUE)
			}
			
		},
		finalize = function() {
			# Stop Rshare server if object is destroyed / R session is ended
			stopRshare(port=.self$getPort())
		},
		show = function() {
			cat(paste(classLabel(class(.self)),"object sharing data on port",.self$port),"\n") 
		},
		getParent = function() {
			.IBData$client
		},
		
		# Data handling functions
		addSymbolData = function(symbol) {
			if (!exists(symbol,.IBData)) {
				.IBData[[symbol]] <- symbolData(symbol)
			}
		},
		removeSymbolData = function(symbol) {
			.IBData[[symbol]] <- NULL
		},
		getDataSymbols = function() {
			syms <- ls(.IBData,all.names=FALSE)
			syms <- syms[-which(syms == "client")]
		},
		updateSymbolData = function(symbol, field, value) {
			if (is.null(.IBData[[symbol]])) .self$addSymbolData(symbol)
			.IBData[[symbol]][[field]] <- value
		},
		getSymbolData = function(symbol, field = NULL, remove.na = TRUE) {
			symbolData <- .IBData[[symbol]]
			if (!is.null(field)) {
				symbolData <- symbolData[[field]] 
			} else if (remove.na) {
				symbolData[!is.na(symbolData)] }
			else symbolData
		},
		
		# Symbol to tickerId conversion -- pass thru to eClient
		symbolToTickerId = function(symbol) {
			.self$getParent()$symbolToTickerId(symbol)
		},
		tickerIdToSymbol = function(tickerId) {
			.self$getParent()$tickerIdToSymbol(tickerId)
		},
		
		# eWrapper msg handling functions
		tickPrice = function(curMsg, msg, ...) {			
			id <- msg[2]
			symbol <- .self$tickerIdToSymbol(id)
			# contract <- .self$getParent()$getContractBySymbol(symbol)
			tickType <- msg[3]
			
			if (tickType == .twsTickType$BID) {
				.self$updateSymbolData(symbol,"BidPrice",as.numeric(msg[4]))
				.self$updateSymbolData(symbol,"BidSize",as.numeric(msg[5]))
				
				# Bid/ask change - update forex last price?
				# if (contract[[id]]$sectype == "CASH") { 
					# cl <- .self$getSymbolData(symbol,"ClosePrice")
					# bp <- as.numeric(msg[4])
					# ap <- .self$getSymbolData(symbol,"AskPrice")
					# if (!is.na(ap)) {
						# lt <- mean(bp,ap) #last is midpoint of bid/ask
						# .self$updateSymbolData(symbol,"LastPrice",lt)
						# .self$updateSymbolData(symbol,"Change",(lt - cl))
						# .self$updateSymbolData(symbol,"PctChange",(lt - cl) / cl)
					# }
				# }
			} else if (tickType == .twsTickType$ASK) {
				.self$updateSymbolData(symbol,"AskPrice",as.numeric(msg[4]))
				.self$updateSymbolData(symbol,"AskSize",as.numeric(msg[5]))
				
				# Bid/ask change - update forex last price?
				# if (contract[[id]]$sectype == "CASH") {
					# cl <- .self$getSymbolData(symbol,"ClosePrice")
					# ap <- as.numeric(msg[4])
					# bp <- .self$getSymbolData(symbol,"BidPrice")
					# if (!is.na(bp)) {
						# lt <- mean(bp,ap) #last is midpoint of bid/ask
						# .self$updateSymbolData(symbol,"LastPrice",lt)
						# .self$updateSymbolData(symbol,"Change",(lt - cl))
						# .self$updateSymbolData(symbol,"PctChange",(lt - cl) / cl)
					# }
				# }
			} else if (tickType == .twsTickType$LAST) {
				cl <- getSymbolData(symbol,"ClosePrice")
				lt <- as.numeric(msg[4])
				.self$updateSymbolData(symbol,"LastPrice",lt)
				.self$updateSymbolData(symbol,"Change",(lt - cl))
				.self$updateSymbolData(symbol,"PctChange",(lt - cl) / cl)
				
				# Update last timestamp?
				
			} else if (tickType == .twsTickType$HIGH) {
				.self$updateSymbolData(symbol,"HighPrice",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$LOW) {
				.self$updateSymbolData(symbol,"LowPrice",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$CLOSE) {
				.self$updateSymbolData(symbol,"ClosePrice",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$OPEN) {
				.self$updateSymbolData(symbol,"OpenPrice",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$LOW_13_WEEK) {
				.self$updateSymbolData(symbol,"13WeekLow",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$HIGH_13_WEEK) {
				.self$updateSymbolData(symbol,"13WeekHigh",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$LOW_26_WEEK) {
				.self$updateSymbolData(symbol,"26WeekLow",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$HIGH_26_WEEK) {
				.self$updateSymbolData(symbol,"26WeekHigh",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$LOW_52_WEEK) {
				.self$updateSymbolData(symbol,"52WeekLow",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$HIGH_52_WEEK) {
				.self$updateSymbolData(symbol,"52WeekHigh",as.numeric(msg[4]))
			} else {
				# something missed?? 
				cat('<Unknown tickPrice> ')
				cat(paste(msg),'\n')
			}
		},
		tickSize = function(curMsg, msg, ...) {
			id <- msg[2]
			symbol <- .self$tickerIdToSymbol(id)
			tickType <- msg[3]
			
			if (tickType == .twsTickType$BID_SIZE) {
				.self$updateSymbolData(symbol,"BidSize",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$ASK_SIZE) {
				.self$updateSymbolData(symbol,"AskSize",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$LAST_SIZE) {
				.self$updateSymbolData(symbol,"LastSize",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$VOLUME) {
				.self$updateSymbolData(symbol,"Volume",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$AVG_VOLUME) {
				.self$updateSymbolData(symbol,"AverageVolume",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$OPTION_CALL_OPEN_INTEREST) {
				.self$updateSymbolData(symbol,"CallOpenInterest",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$OPTION_PUT_OPEN_INTEREST) {
				.self$updateSymbolData(symbol,"PutOpenInterest",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$OPTION_CALL_VOLUME) {
				.self$updateSymbolData(symbol,"CallVolume",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$OPTION_PUT_VOLUME) {
				.self$updateSymbolData(symbol,"PutVolume",as.numeric(msg[4]))
			} else {
				cat('<Unknown tickSize> ')
				cat(paste(msg),'\n')
			}
		},
		tickOptionComputation = function(curMsg, msg, ...) {
			id <- msg[2]
			symbol <- .self$tickerIdToSymbol(id)
			tickType <- msg[3]
			
			if (tickType == .twsTickType$BID_OPTION) { #10
				cat('bidOption:',msg[4],msg[5],'\n')
			} else if (tickType == .twsTickType$ASK_OPTION) { #11
				cat('askOption:',msg[4],msg[5],'\n')
			} else if (tickType == .twsTickType$LAST_OPTION) { #12
				cat('lastOption:',msg[4],msg[5],'\n')
			} else if (tickType == .twsTickType$MODEL_OPTION) { #13
				cat('modelOption: impVol: ',msg[4],' delta:',msg[5],
				' modelPrice: ',msg[6],' pvDiv: ',msg[7],
				' gamma: ',msg[8],' vega: ',msg[9],
				' theta: ',msg[10],' undPrice: ',msg[11],'\n')
			} else {
				cat('<Unknown option> ')
				cat(paste(msg),'\n')
			}
		},		
		tickGeneric = function(curMsg, msg, ...) {
			id <- msg[2]
			symbol <- .self$tickerIdToSymbol(id)
			tickType <- msg[3]
			
			if (tickType == .twsTickType$OPTION_IMPLIED_VOL) { #24
				.self$updateSymbolData(symbol,"ImpliedVol",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$OPTION_HISTORICAL_VOL) { #23
				.self$updateSymbolData(symbol,"HistoricalVol",as.numeric(msg[4]))
			} else if (tickType == .twsTickType$INDEX_FUTURE_PREMIUM) { #31
				# DEPRECATED
				# .self$updateSymbolData(symbol,"AskSize",as.numeric(msg[4]))
				cat('indexFuturePremium:',msg[4],msg[5],'\n')
			} else if (tickType == .twsTickType$SHORTABLE) { #46
				value <- as.numeric(msg[4])
				if (value > 2.5) { # 3.0, at least 1000 shares available for short sale
					shortable <- TRUE
				} else if (value > 1.5) { # 2.0 -- available if shares can be located, should this be TRUE of FALSE?
					shortable <- FALSE
				} else if (value > 0.5) { # 1.0 -- not available for short sale
					shortable <- FALSE
				} else shortable <- FALSE # unknown value, default to FALSE?
				
				.self$updateSymbolData(symbol,"Shortable",shortable)
			} else if (tickType == .twsTickType$HALTED) { #49
				halted <- ifelse(msg[4] == "1",TRUE,FALSE)
				.self$updateSymbolData(symbol,"Halted",halted)
			} else {
				cat('<Unknown tickGeneric>')
				cat(paste(msg),'\n')
			}
		},		
		tickString = function(curMsg, msg, ...) {
			id <- msg[2]
			symbol <- .self$tickerIdToSymbol(id)
			tickType <- msg[3]
			
			if (tickType == .twsTickType$BID_EXCH) { #32
				cat('bidExchange:',msg[4],'\n')
			} else if (tickType == .twsTickType$ASK_EXCH) { #33
				cat('askExchange:',msg[4],'\n')
			} else if (tickType == .twsTickType$LAST_TIMESTAMP) { #45
				timestr <- substr(msg[4],1,10) # need to shorten timestamp strings if they include subsecond times
				timestamp <- as.numeric(timestr)
				#timestamp <- as.POSIXct(as.numeric(timestr),origin="1970-01-01",tz="GMT")
				.self$updateSymbolData(symbol,"LastTimestamp",timestamp)
			} else if (tickType == .twsTickType$RT_VOLUME) { #48
				rtv <- unlist(strsplit(msg[4],";"),use.names=FALSE)
				
				cl <- getSymbolData(symbol,"ClosePrice")
				lt <- as.numeric(rtv[1])
				.self$updateSymbolData(symbol,"LastPrice",lt)
				.self$updateSymbolData(symbol,"Change",(lt - cl))
				.self$updateSymbolData(symbol,"PctChange",(lt - cl) / cl )
				
				.self$updateSymbolData(symbol,"LastSize",as.numeric(rtv[2]))
				
				timestr <- substr(rtv[3],1,10) # need to shorten timestamp strings if they include subsecond times
				timestamp <- as.numeric(timestr)
				#timestamp <- as.POSIXct(as.numeric(timestr),origin="1970-01-01",tz="GMT")
				.self$updateSymbolData(symbol,"LastTimestamp",timestamp)
				
				.self$updateSymbolData(symbol,"Volume",as.numeric(rtv[4]))
				.self$updateSymbolData(symbol,"VWAP",as.numeric(rtv[5]))
			} else {
				cat('<Unknown tickString>')
				cat(paste(msg),'\n')
			}
		},
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
		updateAccountValue = function(curMsg, msg,  ...) {
			# data <- get.IBData("data")
			# data[[msg[2]]] <- c(value = msg[3], currency = msg[4])
			# assign.IBData("data", data)
		},
		updatePortfolio = function(curMsg, msg,  ...) {
			version <- as.numeric(msg[1])
			contract <- twsContract(conId = msg[2], symbol = msg[3], 
				sectype = msg[4], exch = NULL, primary = msg[9], 
				expiry = msg[5], strike = msg[6], currency = msg[10], 
				right = msg[7], local = msg[11], multiplier = msg[8], 
				combo_legs_desc = NULL, comboleg = NULL, include_expired = NULL)
			portfolioValue <- list()
			portfolioValue$position <- as.numeric(msg[12])
			portfolioValue$marketPrice <- as.numeric(msg[13])
			portfolioValue$marketValue <- as.numeric(msg[14])
			portfolioValue$averageCost <- as.numeric(msg[15])
			portfolioValue$unrealizedPNL <- as.numeric(msg[16])
			portfolioValue$realizedPNL <- as.numeric(msg[17])
			portfolioValue$accountName <- msg[18]
			p <- structure(list(contract = contract, portfolioValue = portfolioValue), 
				class = "eventPortfolioValue")
			p
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
		},
		nextValidId = function(curMsg, msg, ...) {
			nextId <- as.integer(msg[2])
			nextId
		},
		managedAccounts = function(curMsg, msg, ...) {
			faAccts <- unlist(strsplit(msg[2],","))
			faAccts
		},
		receiveFA = function(curMsg, msg,  ...) {
			require(XML)
			faDataType <- msg[2]
			xml <- msg[3]
			
			dom <- xmlInternalTreeParse(xml, asText=TRUE)
			if (faDataType == 1) { # groups
				group.xml <- getNodeSet(dom, "/ListOfGroups/Group")
				groups <- list()
				for (i in 1:length(group.xml)) {
					name <- unlist(xpathApply(group.xml[[i]],"name",xmlValue))
					accts <- unlist(xpathApply(group.xml[[i]],"ListOfAccts/String",xmlValue))
					defaultMethod <- unlist(xpathApply(group.xml[[i]],"defaultMethod",xmlValue))
					
					groups[[name]] <- structure(list(name=name, accts=accts, defaultMethod=defaultMethod), class="twsGroupFA")
				}
				return(groups)
			} else if (faDataType == 2) { # profiles
				profile.xml <-  getNodeSet(dom, "/ListOfAllocationProfiles/AllocationProfile")
				profiles <- list()
				for (i in 1:length(profile.xml)) {
					name <- unlist(xpathApply(profile.xml[[i]],"name",xmlValue))
					type <- names(.faAllocationTypes)[as.numeric(unlist(xpathApply(profile.xml[[i]],"type",xmlValue)))]
					
					allocation.xml <- getNodeSet(profile.xml[[i]],"ListOfAllocations/Allocation")
					allocations <- list()
					for (i in 1:length(allocation.xml)) {
						acct <- unlist(xpathApply(allocation.xml[[i]],"acct",xmlValue))
						amount <- as.numeric(unlist(xpathApply(allocation.xml[[i]],"amount",xmlValue)))
						posEff <- unlist(xpathApply(allocation.xml[[i]],"posEff",xmlValue))
						allocations[[acct]] <- structure(list(acct=acct, amount=amount, posEff=posEff))
					}
					
					profiles[[name]] <- structure(list(name=name,type=type, allocations=allocations), class="twsProfileFA")
				}
				return(profiles)
			} else if (faDataType == 3) { # aliases
				alias.xml <- getNodeSet(dom, "/ListOfAccountAliases/AccountAlias")
				aliases <- list()
				for (i in 1:length(alias.xml)) {
					account <- unlist(xpathApply(alias.xml[[i]],"account",xmlValue))
					alias <- unlist(xpathApply(alias.xml[[i]],"alias",xmlValue))
					
					aliases[[account]] <- alias
				}
				aliases <- structure(aliases, class="twsAliasFA")
				return(aliases)
			} else stop("error: unrecognized faDataType")
		}
	), contains = "eWrapper"
)
eWrapper.IBData$accessors("port")
eWrapper.IBData$lock("port")
