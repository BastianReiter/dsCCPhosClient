% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.GetObjectMetaData.R
\name{ds.GetObjectMetaData}
\alias{ds.GetObjectMetaData}
\title{ds.GetObjectMetaData}
\usage{
ds.GetObjectMetaData(ObjectName, DataSources)
}
\arguments{
\item{ObjectName}{String | Name of object on server}

\item{DataSources}{List of DSConnection objects}
}
\value{
A list of server returns
}
\description{
Gathers meta data about an R object.
}
\details{
Linked to server-side AGGREGATE method GetObjectMetaDataDS()
}
\author{
Bastian Reiter
}
