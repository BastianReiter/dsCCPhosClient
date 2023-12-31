
#' ds.CurateData
#'
#' What it does
#'
#' Linked to server-side ASSIGN method CurateDataDS()
#'
#' @param Name_RawData String | Name of raw data object (list) on server | Default: 'RawData'
#' @param Name_Output String | Name of assigned output object on server | Default: 'CurationOutput'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of variables containing messages about object assignment for monitoring purposes.
#' @export
#'
#' @examples
ds.CurateData <- function(Name_RawData = "RawData",
                          Name_Output = "CurationOutput",
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


    # Construct the the server-side function call
    ServerCall <- call("CurateDataDS", Name_RawData)

    # Execute the server-side function call
    DSI::datashield.assign(conns = DataSources,
                           symbol = Name_Output,
                           value = ServerCall)

    # Call helper function to check if object assignment succeeded
    AssignmentInfo <- ds.GetObjectInfo(ObjectName = Name_Output,
                                       DataSources = DataSources)

    return(AssignmentInfo)
}
