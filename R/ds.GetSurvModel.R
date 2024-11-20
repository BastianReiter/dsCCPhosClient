
#' ds.GetSurvModel
#'
#' Get survival models
#'
#' Linked to server-side \code{AGGREGATE} function \code{dsCCPhos::GetSurvModelDS()}.
#'
#' @param DataSources List of DSConnection objects
#' @param TableName \code{string} | Name of the table containing the features of concern
#' @param TimeFeature \code{string} | Name of time feature
#' @param EventFeature \code{string} | Name of event feature
#' @param CovariateA \code{string} | Name of optional CovariateA
#' @param CovariateB \code{string} | Name of optional CovariateB
#' @param CovariateC \code{string} | Name of optional CovariateC
#' @param MinFollowUpTime \code{integer} | Optional minimum of observed follow up time
#'
#' @return A list of survival models
#'
#' @export
#' @author Bastian Reiter
ds.GetSurvModel <- function(DataSources = NULL,
                            TableName,
                            TimeFeature,
                            EventFeature,
                            CovariateA = NULL,
                            CovariateB = NULL,
                            CovariateC = NULL,
                            MinFollowUpTime = 1)
{

# For Testing Purposes
# DataSources <- CCPConnections
# TableName <- "ADS_Patients"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check argument eligibility
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!(is.character(TableName) & is.character(TimeFeature) & is.character(EventFeature)))
{
    stop("Error: Arguments 'TableName', 'TimeFeature' and 'EventFeature' must be character strings.", call. = FALSE)
}

if (is.null(DataSources))
{
    DataSources <- DSI::datashield.connections_find()
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Package requirements
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(dplyr)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check if addressed objects (Table and Feature) are eligible
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get meta data of table object
TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                      DataSources = DataSources)

if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Separate returns
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# SiteReturns: Obtain survival model for each server calling dsCCPhos::GetSurvModelDS()
ls_SiteReturns <- DSI::datashield.aggregate(conns = DataSources,
                                            expr = call("GetSurvModelDS",
                                                        TableName.S = TableName,
                                                        TimeFeature.S = TimeFeature,
                                                        EventFeature.S = EventFeature,
                                                        CovariateA.S = CovariateA,
                                                        CovariateB.S = CovariateB,
                                                        CovariateC.S = CovariateC,
                                                        MinFollowUpTime.S = MinFollowUpTime))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cumulation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SiteNames <- names(DataSources)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Return statement
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
return(ls_SiteReturns)
}
