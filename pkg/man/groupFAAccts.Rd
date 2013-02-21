\name{groupFAAccts}
\alias{groupFAAccts}
\title{List accounts in a Financial Advisor Group}
\usage{
  groupFAAccts(name, groups = NULL,
    tws = get.IBData("tws"))
}
\arguments{
  \item{name}{the name of the group}

  \item{groups}{an object of class \code{twsGroupFA}. If
  not provided, it will be requested from TWS}

  \item{tws}{tws connection object, defaults to tws object
  stored in IBData environment}
}
\value{
  a character vector of account numbers
}
\description{
  List accounts in a Financial Advisor Group
}

