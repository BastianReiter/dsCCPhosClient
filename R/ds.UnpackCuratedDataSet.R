
#' ds.UnpackCuratedDataSet
#'
#' Make tables within Curated Data Set (list object) directly addressable in R server sessions
#'
#' Linked to server-side ASSIGN method ExtractFromListDS()
#'
#' @param CuratedDataSetName String | Name of Curated Data Set object (list) on server | Default: 'CuratedDataSet'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of messages about object assignment for monitoring purposes
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.UnpackCuratedDataSet <- function(CuratedDataSetName = "CuratedDataSet",
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


    # Initiate output messaging objects
    Messages <- list()
    Messages$Assignment <- list()


    CCPTableNames_CDS <- dsCCPhos::Meta_TableNames$TableName_Curated

    AssignmentInfo <- list()

    for(i in 1:length(CCPTableNames_CDS))
    {
        # Construct the server-side function call
        ServerCall <- call("ExtractFromListDS",
                           ListName.S = CuratedDataSetName,
                           ObjectName.S = CCPTableNames_CDS[i])

        # Execute server-side assign function
        DSI::datashield.assign(conns = DataSources,
                               symbol = paste0("CDS_", CCPTableNames_CDS[i]),      # E.g. 'CDS_Metastasis'
                               value = ServerCall)

        # Call helper function to check if object assignment succeeded
        Messages$Assignment <- c(Messages$Assignment,
                                 ds.GetObjectInfo(ObjectName = paste0("CDS_", CCPTableNames_CDS[i]),
                                                  DataSources = DataSources))
    }

    # Turn list into (named) vector
    Messages$Assignment <- purrr::list_c(Messages$Assignment)

    # Add topic element to start of vector
    Messages$Assignment <- c(Topic = "Object assignment on servers",
                             Messages$Assignment)


    # Print messages and return Messages object
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Print messages on console
    PrintMessages(Messages)

    # Return Messages
    return(Messages)
}
