
#' ds.DrawSample
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Draw a random sample from Raw Data Set
#'
#' Linked to server-side ASSIGN method \code{DrawSampleDS()}
#'
#' @param RawDataSetName \code{string} - Name of a \code{list} object on server
#' @param SampleSize \code{string} - Number of patients per Server
#' @param SampleName \code{string} - Option to assign subset of 'RawDataSet' a different object name on servers
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} of messages about object assignment for monitoring purposes
#' @export
#'
#' @author Bastian Reiter
ds.DrawSample <- function(RawDataSetName = "RawDataSet",
                          SampleSize = "100",
                          SampleName = "RawDataSet",
                          DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Construct the server-side function call
  ServerCall <- call("DrawSampleDS",
                     RawDataSetName.S = RawDataSetName,
                     SampleSize.S = SampleSize)

  # Execute server-side assign function
  DSI::datashield.assign(conns = DSConnections,
                         symbol = SampleName,
                         value = ServerCall)

  # Call helper function to check if object assignment succeeded
  AssignmentInfo <- ds.GetObjectStatus(SampleName,
                                       DSConnections = DSConnections)

  return(AssignmentInfo)
}
