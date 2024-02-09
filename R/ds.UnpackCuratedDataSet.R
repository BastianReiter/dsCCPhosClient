
#' ds.UnpackCuratedDataSet
#'
#' Make tables within Curated Data Set (which lives as a list within Curation Output) directly addressable in R server sessions
#'
#' Linked to server-side ASSIGN method UnpackCuratedDataSetDS()
#'
#' @param Name_CurationOutput String | Name of curation output object (list) on server | Default: 'CurationOutput'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of messages about object assignment for monitoring purposes
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.UnpackCuratedDataSet <- function(Name_CurationOutput = "CurationOutput",
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

    CCPTableNames_Curated <- dsCCPhos::Meta_TableNames$TableName_Curated

    AssignmentInfo <- list()

    for(i in 1:length(CCPTableNames_Curated))
    {
        # Construct the server-side function call
        ServerCall <- call("UnpackCuratedDataSetDS",
                           Name_CurationOutput,
                           TableName = CCPTableNames_Curated[i])

        # Execute server-side assign function
        DSI::datashield.assign(conns = DataSources,
                               symbol = paste0("CDS_", CCPTableNames_Curated[i]),      # E.g. 'CDS_Metastasis'
                               value = ServerCall)

        # Call helper function to check if object assignment succeeded
        AssignmentInfo <- c(AssignmentInfo,
                            ds.GetObjectInfo(ObjectName = paste0("CDS_", CCPTableNames_Curated[i]),
                                             DataSources = DataSources))
    }

    return(AssignmentInfo)
}
