\name{startIBDataServer}
\alias{startIBDataServer}
\title{Start IBData Server}
\usage{
  startIBDataServer(Contracts = NULL,
    use.instruments = FALSE, clientId = 1, port = 7789,
    startServer = FALSE, stopOnExit = FALSE)
}
\description{
  This function is basically a higher level wrapper to the
  eClient. It creates an eClient object and initializes it
  with the eWrapper.IBData wrapper. This function also
  initializes the IBData server, allowing other R sessions
  to subscribe to market data from this eClient as well as
  issue commands to the client.
}

