% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetServerOpalInfo.R
\name{GetServerOpalInfo}
\alias{GetServerOpalInfo}
\title{GetServerOpalInfo}
\usage{
GetServerOpalInfo(CCPSiteSpecifications = NULL, DataSources = NULL)
}
\arguments{
\item{CCPSiteSpecifications}{\code{data.frame} | Same data frame used for login. Used here only for akquisition of site-specific project names (in case they are differing). | Default: NULL for virtual project}

\item{DataSources}{List of DSConnection objects}
}
\value{
A tibble
}
\description{
Check if tables are available in server Opal data bases
}
\author{
Bastian Reiter
}
