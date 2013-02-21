# Set 'twsconn' so as not to throw an error in eReader initialization
setClass("twsconn")

#' @export
eReader <- setRefClass("eReader",
	fields = list(
		tws = "twsconn",
		clientVersion = "character"),
	methods = list(
		initialize = function(...) {
			# initialize fields
			initFields(...)
			
			clientVersion <<- "47"
		},
		finalize = function() {
			
		},
		show = function() {
			cat(paste(classLabel(class(.self)),"object"),"\n")
		},
		getParent = function() {
			.IBData$client
		},
		getClientWrapper = function() {
			.self$getParent()$getClientWrapper()
		},
		getWrapper = function() {
			.self$getParent()$getWrapper()
		},
		
		# TWS connection functions (much borrowed from IBrokers)
		connected = function() {
			if (inherits(.self$getTws(),"twsconn")) {
				res <- try(isOpen(.self$getTws()[[1]]), silent = TRUE)
				if (inherits(res, "try-error")) res <- FALSE
			} else res <- FALSE
			res
		},
		connect = function(clientId = 1, client=c("TWS","IBG"), host = "localhost", 
							port = NULL, timeout = 5, 
							blocking = .Platform$OS.type=="windows",
							verbose = TRUE) {
			if (is.null(port)) port <- switch(client[1], TWS = 7496, IBG = 4001)
			
			.self$clientConnect(clientId, host=host, port=port, timeout=timeout, blocking=blocking, verbose=verbose)
		},
		clientConnect = function(clientId = 1, host = "localhost",
							port = 7496, timeout = 5, 
							blocking = .Platform$OS.type=="windows",
							verbose = TRUE) {
			
			if (.self$connected()) stop("Error in clientConnect : already connected to TWS", call.=FALSE)
			if (is.null(getOption('digits.secs'))) options(digits.secs=6)
			
			# Try to connect to TWS
			s <- try(suppressWarnings(socketConnection(host = host, port = port, open='ab', blocking=blocking, timeout=timeout)),silent=TRUE)
			if (inherits(s,"try-error")) stop(paste("connection attempt to TWS on port",port,"timed-out after",timeout,"seconds"),call.=FALSE)
			
			on.exit(close(s))
			if (!isOpen(s)) { 
				close(s)
				stop(paste("couldn't connect to TWS on port",port),call.=FALSE)
			}

			writeBin(c(.self$getClientVersion(),as.character(clientId)), s)
			
			start.time <- Sys.time()
			serverVersion <- nextValidId <- faAccts <- NULL
			while (TRUE) {
				if (!is.null(nextValidId)) {
					if (!socketSelect(list(s), FALSE, 0.1)) break
				} 
				
				curMsg <- readBin(s, character(), 1)
				
				if (is.null(serverVersion)) {
					serverVersion <- curMsg[1]
					connectionTime <- readBin(s, character(), 1)
					next
				}
				
				if ((Sys.time() - start.time) > timeout) {
					close(s)
					stop(paste("connection attempt to TWS on port",port,"timed-out after",timeout,"seconds"),call.=FALSE)
				}
				
				if (curMsg == .twsIncomingMSG$NEXT_VALID_ID) {
					# set next valid Id for client
					nextValidId <- .self$processMsg(curMsg, s, eWrapper=.self$getParent()$getClientWrapper())
					.self$getParent()$parameters(nextTickerId = nextValidId)
					next
				} else if (curMsg == .twsIncomingMSG$MANAGED_ACCTS) {
					accts <- .self$processMsg(curMsg, s, eWrapper=.self$getParent()$getClientWrapper())
					.self$getParent()$parameters(accountId=accts[1])
					
					# check if simulated trading / paper trader account
					checkChar <- 1
					if (identical(substr(accts[1],checkChar,checkChar),"D")) {
						.self$getParent()$parameters(simulated=TRUE)
						checkChar <- 2
					} else .self$getParent()$parameters(simulated=FALSE)
					
					# check if advisor account
					if (identical(substr(accts[1],checkChar,checkChar),"F") || (length(accts) > 1)) { # advisor
						.self$getParent()$setFAStatus(TRUE)
						if (length(accts) > 1) .self$getParent()$setFAAccts(accts[2:length(accts)])
					} else {
						.self$getParent()$setFAStatus(FALSE)
					}
					next
				} else if (curMsg == .twsIncomingMSG$ERR_MSG) {
					errMsg <- readBin(s, character(), 4)
					
					if (errMsg[3] %in% c("2103","2104","2105","2106","2107","2108")) {
						# mkt data farm msg
						if (verbose) cat(errMsg[4],"\n")
						next
					} else {
						close(s)
						on.exit()
						cat("Error: ",errMsg[4],"\n")
						cat("Trying to reconnect...\n")
						if (identical(as.integer(errMsg[3]),326L)) clientId <- clientId + 1 
						return(.self$clientConnect(clientId=clientId, host=host, port=port, timeout=(timeout - (Sys.time()-start.time)), blocking=blocking, verbose=verbose))
					}
				} else {
					# just deal with other message in usual way using normal eWrapper
					.self$processMsg(curMsg, s)
				}
			}
			on.exit() # successful connection, so unregister socket close function
			
			# create connection object
			twsconn <- new.env()
			twsconn$conn <- s
			twsconn$clientId <- clientId
			twsconn$port <- port
			twsconn$server.version <- serverVersion
			twsconn$connected.at <- as.POSIXct(connectionTime,format="%Y%m%d %H:%M:%S")
			class(twsconn) <- c("twsconn","environment")
			.self$setTws(twsconn)
			invisible(TRUE)
		},
		getConnection = function() {
			if (.self$connected()) .self$getTws()[[1]] else NULL
		},
		disconnect = function() {
			if (.self$connected()) close(.self$getConnection())
		},
		
		# Callback and message handler
		run = function(...) {
			if (!.self$connected()) stop("not connected to TWS")
			
			con <- .self$getConnection()
			while (TRUE) {
				if (!socketSelect(list(con), FALSE, 0.25)) next
				curMsg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))

				.self$processMsg(curMsg, con, ...)
			}
		},
		processMsg = function(curMsg, con, eWrapper=.self$getWrapper(), ...) {
			if (curMsg == .twsIncomingMSG$TICK_PRICE) {
				msg <- .Internal(readBin(con, "character", 6L, NA_integer_, TRUE, FALSE))
				eWrapper$tickPrice(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$TICK_SIZE) {
				msg <- .Internal(readBin(con, "character", 4L, NA_integer_, TRUE, FALSE))
				eWrapper$tickSize(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$ORDER_STATUS) {
				msg <- .Internal(readBin(con, "character", 11L, NA_integer_, TRUE, FALSE))
				eWrapper$orderStatus(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$ERR_MSG) {
				msg <- .Internal(readBin(con, "character", 4L, NA_integer_, TRUE, FALSE))
				eWrapper$errorMessage(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$OPEN_ORDER) {
				msg <- .Internal(readBin(con, "character", 84L, NA_integer_, TRUE, FALSE))
				eWrapper$openOrder(curMsg, msg, ...)
			} else 	if (curMsg == .twsIncomingMSG$ACCT_VALUE) {
				msg <- .Internal(readBin(con, "character", 5L, NA_integer_, TRUE, FALSE))
				eWrapper$updateAccountValue(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$PORTFOLIO_VALUE) {
				msg <- .Internal(readBin(con, "character", 18L, NA_integer_, TRUE, FALSE))
				eWrapper$updatePortfolio(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$ACCT_UPDATE_TIME) {
				msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
				eWrapper$updateAccountTime(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$NEXT_VALID_ID) {
				msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
				eWrapper$nextValidId(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$CONTRACT_DATA) {
				msg <- .Internal(readBin(con, "character", 28L, NA_integer_, TRUE, FALSE))
				eWrapper$contractDetails(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$EXECUTION_DATA) {
				msg <- .Internal(readBin(con, "character", 24L, NA_integer_, TRUE, FALSE))
				eWrapper$execDetails(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$MARKET_DEPTH) {
				msg <- .Internal(readBin(con, "character", 7L, NA_integer_, TRUE, FALSE))
				eWrapper$updateMktDepth(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$MARKET_DEPTH_L2) {
				msg <- .Internal(readBin(con, "character", 8L, NA_integer_, TRUE, FALSE))
				eWrapper$updateMktDepthL2(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$NEWS_BULLETINS) {
				msg <- .Internal(readBin(con, "character", 5L, NA_integer_, TRUE, FALSE))
				eWrapper$newsBulletins(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$MANAGED_ACCTS) {
				msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
				eWrapper$managedAccounts(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$RECEIVE_FA) {
				msg <- .Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE))
				eWrapper$receiveFA(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$HISTORICAL_DATA) {
				header <- readBin(con, character(), 5)
				nbin <- as.numeric(header[5]) * 9
				
				msg <- .Internal(readBin(con, "character", as.integer(nbin), NA_integer_, TRUE, FALSE))
				eWrapper$historicalData(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$BOND_CONTRACT_DATA) {
				eWrapper$bondContractDetails(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$SCANNER_PARAMETERS) {
				version <- readBin(con, character(), 1L) 
				msg <- readBin(con, raw(), 1e6L)
				eWrapper$scannerParameters(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$SCANNER_DATA) {
				cD <- IBrokers:::twsContractDetails()
				version <- readBin(con, character(), 1L)
				tickerId <- readBin(con, character(), 1L)
				numberOfElements <- as.integer(readBin(con, character(), 1L))
				for(i in 1:numberOfElements) {
					msg <- readBin(con, character(), 16L)
					rank <- msg[1]
					cD$contract$conId <- msg[2]
					cD$contract$symbol <- msg[3]
					cD$contract$sectype <- msg[4]
					cD$contract$expiry <- msg[5]
					cD$contract$strike <- msg[6]
					cD$contract$right  <- msg[7]
					cD$contract$exch  <- msg[8]
					cD$contract$currency  <- msg[9]
					cD$contract$local  <- msg[10]
					cD$marketName  <- msg[11]
					cD$tradingClass  <- msg[12]
					distance <- msg[13]
					benchmark <- msg[14]
					projection <- msg[15]
					legsStr <- msg[16]
					eWrapper$scannerData(curMsg, tickerId, rank, cD, distance, benchmark, projection, legsStr)
				}
			} else if (curMsg == .twsIncomingMSG$TICK_OPTION_COMPUTATION) {
				msg <- .Internal(readBin(con, "character", 11L, NA_integer_, TRUE, FALSE))
				#    if(msg[3] == .twsTickType$MODEL_OPTION) {
				#      #msg <- c(msg, readBin(con, character(), 2))
				#      msg <- c(msg,.Internal(readBin(con, "character", 6L, NA_integer_, TRUE, FALSE)))
				#    } else msg <- c(msg,NA,NA)
				eWrapper$tickOptionComputation(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$TICK_GENERIC) {
				msg <- .Internal(readBin(con, "character", 4L, NA_integer_, TRUE, FALSE))
				eWrapper$tickGeneric(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$TICK_STRING) {
				msg <- .Internal(readBin(con, "character", 4L, NA_integer_, TRUE, FALSE))
				eWrapper$tickString(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$TICK_EFP) {
				msg <- .Internal(readBin(con, "character", 10L, NA_integer_, TRUE, FALSE))
				eWrapper$tickEFP(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$CURRENT_TIME) {
				msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
				eWrapper$currentTime(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$REAL_TIME_BARS) {
				msg <- .Internal(readBin(con, "character", 10L, NA_integer_, TRUE, FALSE))
				eWrapper$realtimeBars(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$FUNDAMENTAL_DATA) {
				msg <- .Internal(readBin(con, "character", 3L, NA_integer_, TRUE, FALSE))
				eWrapper$fundamentalData(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$CONTRACT_DATA_END) {
				msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
				eWrapper$contractDetailsEnd(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$OPEN_ORDER_END) {
				msg <- .Internal(readBin(con, "character", 1L, NA_integer_, TRUE, FALSE))
				eWrapper$openOrderEnd(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$ACCT_DOWNLOAD_END) {
				msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
				eWrapper$accountDownloadEnd(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$EXECUTION_DATA_END) {
				msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
				eWrapper$execDetailsEnd(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$DELTA_NEUTRAL_VALIDATION) {
				msg <- .Internal(readBin(con, "character", 5L, NA_integer_, TRUE, FALSE))
				eWrapper$deltaNeutralValidation(curMsg, msg, ...)
			} else if (curMsg == .twsIncomingMSG$TICK_SNAPSHOT_END) {
				msg <- .Internal(readBin(con, "character", 2L, NA_integer_, TRUE, FALSE))
				eWrapper$tickSnapshotEnd(curMsg, msg, ...)
			} else {
				## TODO: Do something graceful here. Save subscribed contracts, disconnect and reconnect and resubscribe? probably push to client
				# default handler/error
				warning(paste("Unknown incoming message: ",curMsg,". Please reset connection",sep=""), call.=FALSE)
			}
			# end of messages
		}
	)
)
eReader$accessors("tws","clientVersion")
