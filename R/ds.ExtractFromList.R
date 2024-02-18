
#' ds.ExtractFromList
#'
#' Auxiliary function to trigger assignment of object extracted from a list on server
#'
#' Linked to server-side ASSIGN method ExtractFromListDS()
#'
#' @param ListName String | Name of a list object on server
#' @param ObjectName String | Name of object inside list
#' @param NewObjectName String | Optionally assigned name of object after extraction to server session | Default: NULL
#' @param DataSources List of DSConnection objects
#'
#' @return A list of messages about object assignment for monitoring purposes
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.ExtractFromList <- function(ListName,
                               ObjectName,
                               AssignedObjectName = NULL,
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
    ServerCall <- call("ExtractFromListDS",
                       .ListName = ListName,
                       .ObjectName = ObjectName)

    # Execute server-side assign function
    DSI::datashield.assign(conns = DataSources,
                           symbol = ifelse(!is.null(NewObjectName),      # Per default, assign same name to object as it was in the list, but optionally assign new name
                                           NewObjectName,
                                           ObjectName),
                           value = ServerCall)

    # Call helper function to check if object assignment succeeded
    AssignmentInfo <- ds.GetObjectInfo(ObjectName,
                                       DataSources = DataSources)

    return(AssignmentInfo)
}
