#' @export
instrumentsToContracts <- function(instruments = ls_instruments()) {
	contracts <- NULL
	for (inst in instruments) {
		inst.obj <- getInstrument(inst)
		
		if (is.null(inst.obj$currency)) curr <- "USD" else curr <- inst.obj$currency
		if (is.null(inst.obj$ib_id)) ib.id <- inst.obj$primary_id else ib.id <- inst.obj$ib_id
		
		## TODO: clean this section up, add future options, bonds, other instrument types. Actually integrate with FinancialInstrument package in a much better way. 
		if (inherits(inst.obj,"equity")) {
			# if (is.null(inst.obj$exchange)) exch <- "SMART" else exch <- inst.obj$exchange 
			tmp_con <- twsEquity(symbol=ib_id, exch = "SMART", primary="", strike="0.0", currency=curr, right="", local="", multiplier="", include_expired="0", conId=0) 
		
		} else if (inherits(inst.obj,"future")) {
			if (is.null(inst.obj$exchange)) exch <- "" else exch <- inst.obj$exchange 
			if (is.null(inst.obj$multiplier)) multi <- "" else multi <- inst.obj$multiplier
			if (is.null(inst.obj$expiry)) expiry <- "" else expiry <- inst.obj$expiry
			tmp_con <- twsFuture(symbol=ib_id, exch=exch, expiry=expiry, primary="", currency=curr, right="", local="", multiplier=multi, include_expired="0", conId=0) 
		
		} else if (inherits(inst.obj,"option")) {
			if (is.null(inst.obj$exchange)) exch <- "SMART" else exch <- inst.obj$exchange 
			if (is.null(inst.obj$strike)) strike <- "0.0" else strike <- inst.obj$strike
			if (is.null(inst.obj$multiplier)) multi <- "100" else multi <- inst.obj$multiplier
			if (is.null(inst.obj$expiry)) expiry <- "" else expiry <- inst.obj$expiry
			tmp_con <- twsOption(local=ib_id, expiry=expiry, strike=strike, right="", exch=exch, primary="",currency=curr, symbol="", multiplier=multi, include_expired="0", conId=0)
		
		} else if (inherits(inst.obj,"exchange_rate")) {
			if (is.null(inst.obj$counter_currency)) ib.id <- "USD" else ib.id <- inst.obj$counter_currency
			tmp_con <- twsCurrency(symbol=ib.id, currency=curr, exch='IDEALPRO', primary='', strike='0.0', right='', local=inst.obj$primary_id, multiplier='', include_expired='0', conId=0)
		
		} else next #if we are here it's not an exchange rate, just a currency so not an actual quotable instrument
		
		contracts[[length(contracts) + 1]] <- tmp_con
	}
	if (length(contracts) > 1) contracts else contracts[[1]]
}
		
symbolsToContracts <- function(Symbols) {
	if (length(Symbols) == 0) return(NULL)
	if (inherits(Symbols,"list")) Symbols <- unlist(Symbols, use.names=FALSE)
	if (!is.character(Symbols)) stop("Symbols must be of type 'character'")
	if ("FinancialInstrument" %in% loadedNamespaces()) checkFI <- TRUE else checkFI <- FALSE
	
	contracts <- NULL
	for (i in 1:length(Symbols)) {
		if (checkFI) {
			if (is.instrument.name(Symbols[i])) {
				contracts[[length(contracts) + 1]] <- instrumentsToContracts(Symbols[i])
				next
			}
		}
		## TODO: maybe check TWS contract, see if we can guess at security type? Can we look up with TWS? If so, maybe these all move to eClient instead
		tmp_con <- twsContract(conId = 0, symbol = Symbols[i], sectype = "STK", exch = "SMART", primary = "", expiry = "", 
					strike = "0.0", currency = "USD", right = "", local = "", multiplier = "", combo_legs_desc = NULL, comboleg = NULL, 
					include_expired = "")
		contracts[[length(contracts) + 1]] <- tmp_con
	}
	contracts
}

contractsToSymbols <- function(contracts) {
	for (contract in contracts) {
		if (nchar(contract$local) == 0 || is.null(contract$local)) {
			# if its forex, create pair name if local symbol isnt provided
			if (contract$sectype == "CASH") Symbols <- c(Symbols,paste(contract$symbol,contract$currency,sep=".")) else Symbols <- c(Symbols,contract$symbol)
		} else Symbols <- c(Symbols,contract$local)
	}
	Symbols
}