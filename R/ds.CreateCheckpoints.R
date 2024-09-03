
#' ds.CreateCheckpoints
#'
#' Triggers assignment of named vector on servers containing status data on processing checkpoints
#'
#' Linked to server-side ASSIGN method CreateCheckpointsDS()
#'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of messages about object assignment for monitoring purposes
#' @export
#'
#' @author Bastian Reiter
ds.CreateCheckpoints <- function(DataSources = NULL)
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
    ServerCall <- call("CreateCheckpointsDS")

    # Execute server-side assign function
    DSI::datashield.assign(conns = DataSources,
                           symbol = "Checkpoints",
                           value = ServerCall)

    # Call helper function to check if object assignment succeeded
    AssignmentInfo <- ds.GetObjectStatus("Checkpoints",
                                         DataSources = DataSources)

    return(AssignmentInfo)
}
