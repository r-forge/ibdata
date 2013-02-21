
.onLoad <- function(libname, pkgname) {
	if (!exists(".IBData")) .IBData <<- new.env()
	#set.subscribed(FALSE)
	packageStartupMessage("Loaded IBData version 0.1-2: See ?IBData for more information \n")
	#packageStartupMessage("Warning: This software comes with no warranty!")
}

.Last.lib <- function(libpath) {
	if (!is.null(.IBData$client)) .IBData$client$eDisconnect()
}