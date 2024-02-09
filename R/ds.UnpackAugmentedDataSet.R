
#' ds.UnpackAugmentedDataSet
#'
#' Make tables within Augmented Data Set (which lives as a list within Augmentation Output) directly addressable in R server sessions
#'
#' Linked to server-side ASSIGN method UnpackAugmentedDataSetDS()
#'
#' @param Name_AugmentationOutput String | Name of Augmentation Output object (list) on server | Default: 'AugmentationOutput'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of messages about object assignment for monitoring purposes
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.UnpackAugmentedDataSet <- function(Name_AugmentationOutput = "AugmentationOutput",
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

    CCPTableNames_ADS <- c("Patients", "Diagnoses", "Events")

    AssignmentInfo <- list()

    for(i in 1:length(CCPTableNames_ADS))
    {
        # Construct the server-side function call
        ServerCall <- call("UnpackAugmentedDataSetDS",
                           Name_AugmentationOutput,
                           TableName = CCPTableNames_ADS[i])

        # Execute server-side assign function
        DSI::datashield.assign(conns = DataSources,
                               symbol = paste0("ADS_", CCPTableNames_ADS[i]),      # E.g. 'ADS_Events'
                               value = ServerCall)

        # Call helper function to check if object assignment succeeded
        AssignmentInfo <- c(AssignmentInfo,
                            ds.GetObjectInfo(ObjectName = paste0("ADS_", CCPTableNames_ADS[i]),
                                             DataSources = DataSources))
    }

    return(AssignmentInfo)
}
