% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.GetSampleStatistics.R
\name{ds.GetSampleStatistics}
\alias{ds.GetSampleStatistics}
\title{ds.GetSampleStatistics}
\usage{
ds.GetSampleStatistics(
  DataSources = NULL,
  TableName,
  MetricFeatureName,
  GroupingFeatureName = NULL,
  RemoveMissings = TRUE
)
}
\arguments{
\item{DataSources}{\code{list} of DSConnection objects}

\item{TableName}{\code{string} | Name of the table containing the feature of concern}

\item{MetricFeatureName}{\code{string} | Name of feature}

\item{GroupingFeatureName}{\code{string} | Name of optional grouping feature from the same table}
}
\value{
A \code{tibble} containing parametric and non-parametric sample statistics
}
\description{
Obtain common parametric and nonparametric statistics about a metric feature.
Making use of \code{dsBaseClient::ds.meanSdGp()} and \code{dsBaseClient::ds.quantileMean()}.
}
\details{
Linked to server-side \code{AGGREGATE} function \code{dsCCPhos::GetSampleStatisticsDS()}.
}
\author{
Bastian Reiter
}
