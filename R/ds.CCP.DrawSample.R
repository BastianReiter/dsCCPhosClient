
#' ds.CCP.DrawSample
#'
#' `r lifecycle::badge("experimental")` \cr\cr
#' Draw a random sample from Raw Data Set
#'
#' Linked to server-side ASSIGN method \code{DrawSampleDS()}
#'
#' @param RawDataSetName \code{string} - Name of an RDS \code{list} object on server
#' @param SampleSize \code{integer} - Number of patients per Server
#' @param SampleName \code{string} - Option to assign subset of 'CCP.RawDataSet' a different object name on servers
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#' @param DS.async \code{logical} - Value of argument 'async' in \code{DSI::datashield.assign()} / \code{DSI::datashield.aggregate()} - Default: \code{FALSE}
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.CCP.DrawSample <- function(RawDataSetName = "CCP.RawDataSet",
                              SampleSize = 100,
                              SampleName = "RDSSample",
                              DSConnections = NULL,
                              DS.async = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---
  assert_that(is.string(RawDataSetName),
              is.count(SampleSize),
              is.string(SampleName),
              is.flag(DS.async))

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- dsFredaClient::CheckDSConnections(DSConnections)

#-------------------------------------------------------------------------------

  # Execute server-side assign function
  DSI::datashield.assign(conns = DSConnections,
                         symbol = SampleName,
                         value = call("CCP.DrawSampleDS",
                                      RawDataSetName.S = RawDataSetName,
                                      SampleSize.S = SampleSize),
                         async = DS.async)

  # Call helper function to check if object assignment succeeded
  AssignmentInfo <- ds.GetObjectStatus(SampleName,
                                       DSConnections = DSConnections,
                                       DS.async = DS.async)

#--- Print and invisibly return Messages ---------------------------------------

  # Print messages on console
  dsFredaClient::PrintMessages(AssignmentInfo)

  # Return Messages invisibly
  invisible(AssignmentInfo)
}
