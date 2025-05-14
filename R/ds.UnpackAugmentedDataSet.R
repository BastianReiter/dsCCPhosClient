
#' ds.UnpackAugmentedDataSet
#'
#' Make tables within Augmented Data Set (list object) directly addressable in R server sessions
#'
#' Linked to server-side ASSIGN method ExtractFromListDS()
#'
#' @param AugmentedDataSetName String | Name of Augmented Data Set object (list) on server | Default: 'AugmentedDataSet'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of messages about object assignment for monitoring purposes
#' @export
#'
#' @author Bastian Reiter
ds.UnpackAugmentedDataSet <- function(AugmentedDataSetName = "AugmentedDataSet",
                                      DataSources = NULL)
{
    ### For testing purposes
    # AugmentedDataSetName <- "AugmentedDataSet"
    # DataSources <- CCPConnections

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


    CCPTableNames_ADS <- c("Patient", "Diagnosis", "Therapy", "DiseaseCourse", "Events")

    AssignmentInfo <- list()

    for(i in 1:length(CCPTableNames_ADS))
    {
        # Construct the server-side function call
        ServerCall <- call("ExtractFromListDS",
                           ListName.S = AugmentedDataSetName,
                           ObjectName.S = CCPTableNames_ADS[i])

        # Execute server-side assign function
        DSI::datashield.assign(conns = DataSources,
                               symbol = paste0("ADS_", CCPTableNames_ADS[i]),      # E.g. 'ADS_Events'
                               value = ServerCall)

        # Call helper function to check if object assignment succeeded
        Messages$Assignment <- c(Messages$Assignment,
                                 ds.GetObjectStatus(ObjectName = paste0("ADS_", CCPTableNames_ADS[i]),
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
