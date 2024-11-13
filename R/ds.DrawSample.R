
#' ds.DrawSample
#'
#' Draw a random sample from Raw Data Set
#'
#' Linked to server-side ASSIGN method DrawSampleDS()
#'
#' @param RawDataSetName \code{string} | Name of a list object on server
#' @param SampleSize \code{string} | Number of patients per site
#' @param SampleName \code{string} | Option to assign subset of Raw Data Set a different object name on servers
#' @param DataSources List of DSConnection objects
#'
#' @return A list of messages about object assignment for monitoring purposes
#' @export
#'
#' @author Bastian Reiter
ds.DrawSample <- function(RawDataSetName = "RawDataSet",
                          SampleSize = "100",
                          SampleName = "RawDataSet",
                          DataSources = NULL)
{
    # Look for DS connections
    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }

    # Ensure DataSources is a list of DSConnection-class
    if (!(is.list(DataSources) && all(unlist(lapply(DataSources, function(d) {methods::is(d,"DSConnection")})))))
    {
        stop("'DataSources' were expected to be a list of DSConnection-class objects", call. = FALSE)
    }


    # Construct the server-side function call
    ServerCall <- call("DrawSampleDS",
                       RawDataSetName.S = RawDataSetName,
                       SampleSize.S = SampleSize)

    # Execute server-side assign function
    DSI::datashield.assign(conns = DataSources,
                           symbol = SampleName,
                           value = ServerCall)

    # Call helper function to check if object assignment succeeded
    AssignmentInfo <- ds.GetObjectStatus(SampleName,
                                         DataSources = DataSources)

    return(AssignmentInfo)
}
