
#' ds.AugmentData
#'
#' Transforms curated CCP core data set (CDM) into augmented data set (ADM)
#'
#' Linked to server-side ASSIGN method AugmentDataDS()
#'
#' @param Name_CurationOutput String | Name of curation output object (list) on server | Default: 'CurationOutput'
#' @param Name_Output String | Name of assigned output object on server | Default: 'AugmentationOutput'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of variables containing messages about object assignment for monitoring purposes.
#' @export
#'
#' @examples
ds.AugmentData <- function(Name_CurationOutput = "CurationOutput",
                           Name_Output = "AugmentationOutput",
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
    ServerCall <- call("AugmentDataDS", Name_CurationOutput)

    # Execute the server-side function call
    DSI::datashield.assign(conns = DataSources,
                           symbol = Name_Output,
                           value = ServerCall)

    # Call helper function to check if object assignment succeeded
    AssignmentInfo <- ds.GetObjectInfo(ObjectName = Name_Output,
                                       DataSources = DataSources)

    return(AssignmentInfo)
}
