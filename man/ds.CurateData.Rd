% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.CurateData.R
\name{ds.CurateData}
\alias{ds.CurateData}
\title{ds.CurateData}
\usage{
ds.CurateData(
  RawDataSetName = "RawDataSet",
  Settings = list(DataHarmonization_Profile = "Default", DiagnosisRedundancy_Check =
    TRUE, DiagnosisRedundancy_Profile = "Default", DiagnosisAssociation_Check = TRUE,
    DiagnosisAssociation_Profile = "Default", FeatureObligations_Profile = "Default",
    FeatureTracking_Profile = "Default"),
  OutputName = "CurationOutput",
  DataSources = NULL
)
}
\arguments{
\item{RawDataSetName}{\code{character} | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'}

\item{Settings}{\code{list} - Settings passed to function
\itemize{\item DataHarmonization_RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_DataHarmonization}
\item DataHarmonization_Profile \code{character} - Profile name defining rule set to be used for data harmonization. Profile name must be stated in \code{RawDataHarmonization_RuleSet} - Default: 'Default'
\item DiagnosisRedundancy_Check \code{logical} - Whether or not to check for redundant diagnosis entries
\item DiagnosisRedundancy_RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_DiagnosisRedundancy}
\item DiagnosisRedundancy_Profile \code{character} - Profile name defining rule set to be used for classification of diagnosis redundancies. Profile name must be stated in \code{DiagnosisRedundancy_RuleSet} - Default: 'Default'
\item DiagnosisAssociation_Check \code{logical} - Whether or not to classify associated diagnosis entries
\item DiagnosisAssociation_RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_DiagnosisAssociation}
\item DiagnosisAssociation_Profile \code{character} - Profile name defining rule set to be used for classification of diagnosis associations. Profile name must be stated in \code{DiagnosisAssociation_RuleSet} - Default: 'Default'
\item FeatureObligations_RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_FeatureObligations}
\item FeatureObligations_Profile \code{character} - Profile name defining strict and trans-feature rules for obligatory feature content. Profile name must be stated in \code{FeatureObligations_RuleSet} - Default: 'Default',
\item FeatureTracking_RuleSet \code{data.frame} - Default: \code{dsCCPhos::Meta_FeatureTracking}
\item FeatureTracking_Profile \code{character} - Profile name defining which features should be tracked/monitored during curation process. Profile name must be stated in \code{FeatureTracking_RuleSet} - Default: 'Default'}}

\item{OutputName}{\code{character} - Name of output object to be assigned on server | Default: 'CurationOutput'}

\item{DataSources}{\code{list} of DSConnection objects}
}
\value{
\code{list} of following objects:
\itemize{\item 'Messages' - Info messages concerning completion of CurateDataDS() and assignment of the following objects on server:
\itemize{\item CurationOutput (\code{list})
\item CuratedDataSet (\code{list})
\item CurationReport (\code{list})
\item CurationMessages (\code{list})}
\item 'CurationCompletionCheck'}
}
\description{
Triggers transformation of Raw Data Set (RDS) into Curated Data Set (CDS) on server.
}
\details{
Linked to server-side ASSIGN methods dsCCPhos::CurateDataDS() and dsCCPhos::ExtractFromListDS()
}
\author{
Bastian Reiter
}
