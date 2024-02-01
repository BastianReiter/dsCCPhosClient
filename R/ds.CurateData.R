
#' ds.CurateData
#'
#' Triggers transformation of Raw Data Set (RDS) into Curated Data Set (CDS) on server.
#'
#' Linked to server-side ASSIGN method CurateDataDS()
#'
#' @param Name_RawDataSet String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
#' @param Name_Output String | Name of assigned output object on server | Default: 'CurationOutput'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of variables containing messages about object assignment for monitoring purposes.
#' @export
#'
#' @examples
ds.CurateData <- function(Name_Output = "CurationOutput",
                          Name_RawDataSet = "RawDataSet",
                          RuleProfile_DiagnosisAssociation = "Default",
                          RuleProfile_DiagnosisRedundancy = "Default",
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
    ServerCall <- call("CurateDataDS", Name_RawDataSet)

    # Execute the server-side function call
    DSI::datashield.assign(conns = DataSources,
                           symbol = Name_Output,
                           value = ServerCall)

    # Call helper function to check if object assignment succeeded
    AssignmentInfo <- ds.GetObjectInfo(ObjectName = Name_Output,
                                       DataSources = DataSources)

    return(AssignmentInfo)
}
