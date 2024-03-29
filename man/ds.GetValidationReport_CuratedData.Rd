% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.GetValidationReport_CuratedData.R
\name{ds.GetValidationReport_CuratedData}
\alias{ds.GetValidationReport_CuratedData}
\title{ds.GetValidationReport_CuratedData}
\usage{
ds.GetValidationReport_CuratedData(
  Name_CurationOutput = "CurationOutput",
  DataSources = NULL
)
}
\arguments{
\item{Name_CurationOutput}{String | Name of curation output object (list) on server | Default: 'CurationOutput'}

\item{DataSources}{List of DSConnection objects}
}
\value{
A list of tibbles containing output of validation
}
\description{
Triggers validation of Curated Data Set on server and requests a report.
}
\details{
Linked to server-side AGGREGATE method GetValidationReportDS_CuratedData()
}
\author{
Bastian Reiter
}
