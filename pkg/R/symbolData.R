symbolData <- function(symbol) {
	structure(list(
		Symbol=symbol, 
		BidSize=NA, BidPrice=NA, 
		AskPrice=NA, AskSize=NA, 
		OpenPrice=NA, HighPrice=NA, 
		LowPrice=NA, ClosePrice=NA, 
		Volume=NA, LastPrice=NA, 
		LastTimestamp=NA, LastSize=NA, 
		VWAP=NA, Change=NA, PctChange=NA, 
		Halted=NA, Shortable=NA, 
		CallVolume=NA, PutVolume=NA, 
		CallOpenInterest=NA, PutOpenInterest=NA, 
		HistoricalVol=NA, ImpliedVol=NA, 
		High13Week=NA, Low13WeekLow=NA, 
		High26Week=NA, Low26Week=NA, 
		High52Week=NA, Low52Week=NA, 
		AverageVolume = NA),
		class="symbolData") 
	#.Names = c("SYMBOL", "BID_SIZE", "BID_PRICE", "ASK_PRICE", "ASK_SIZE", "OPEN_PRICE", "HIGH_PRICE", "LOW_PRICE", "CLOSE_PRICE", "VOLUME", "LAST_PRICE", "LAST_TIMESTAMP", "LAST_SIZE", "VWAP", "CHANGE", "PCT_CHANGE", "HALTED", "SHORTABLE", "OPTION_CALL_VOLUME", "OPTION_PUT_VOLUME", "OPTION_CALL_OPEN_INTEREST", "OPTION_PUT_OPEN_INTEREST", "OPTION_HISTORICAL_VOL", "OPTION_IMPLIED_VOL", "HIGH_13_WEEK", "LOW_13_WEEK", "HIGH_26_WEEK", "LOW_26_WEEK", "HIGH_52_WEEK", "LOW_52_WEEK", "AVERAGE_VOLUME"),
	#GENERIC_TICKS=c("100,101,104,106,165,221,233,236"))
}

#' @export
print.symbolData <- function(x, remove.na = TRUE) {
	## TODO: make this much better, perhaps just format elements separately and display as if its a vecotor (to account for logical types, displays w/ and w/o decimal points, etc)
	if (remove.na) idx <- !is.na(x) else idx <- rep(TRUE,length(x))
	idx["Symbol"] <- FALSE
	# print(structure(x$Symbol,.Names="Symbol"))
	print(unlist(x[idx]),scipen=22)
}

#setMethod("print","symbolData",print.symbolData)