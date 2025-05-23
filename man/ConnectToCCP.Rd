% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ConnectToCCP.R
\name{ConnectToCCP}
\alias{ConnectToCCP}
\title{ConnectToCCP}
\usage{
ConnectToCCP(CCPSiteSpecifications, proxyurl = "http://localhost")
}
\arguments{
\item{CCPSiteSpecifications}{\code{data.frame} - Login data of CCP sites}
}
\value{
A list of \code{DSConnection}-objects
}
\description{
Takes credentials of CCP sites and returns a list of DSConnection-objects. Has to be executed from within a CCP bridgehead R server session.
}
\author{
Bastian Reiter
}
