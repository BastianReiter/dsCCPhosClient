
#' ds.AugmentData
#'
#' Transforms curated CCP core data set (CDM) into augmented data set (ADM)
#'
#' Linked to server-side ASSIGN method AugmentDataDS()
#'
#' @param CuratedDataSetName String | Name of the Curated Data Set object on server | Default: 'CuratedDataSet'
#' @param OutputName String | Name of output object to be assigned on server | Default: 'AugmentationOutput'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of variables containing messages about object assignment for monitoring purposes.
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.AugmentData <- function(CuratedDataSetName = "CuratedDataSet",
                           OutputName = "AugmentationOutput",
                           DataSources = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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


    # For testing purposes
    # CuratedDataSetName <- "CuratedDataSet"
    # OutputName <- "AugmentationOutput"
    # DataSources <- CCPConnections


    # Initiate output messaging objects
    Messages <- list()
    #Messages$Completion <- character()
    Messages$Assignment <- list()



    # 1) Trigger dsCCPhos::AugmentDataDS()
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Construct the server-side function call
    ServerCall <- call("AugmentDataDS",
                       CuratedDataSetName.S = CuratedDataSetName)

    # Execute the server-side function call
    DSI::datashield.assign(conns = DataSources,
                           symbol = OutputName,
                           value = ServerCall)

    # Call helper function to check if assignment of AugmentationOutput succeeded
    Messages$Assignment <- c(Messages$Assignment,
                             ds.GetObjectStatus(ObjectName = OutputName,
                                                DataSources = DataSources))



    # 2) Extract objects from list returned by AugmentDataDS() and assign them to R server sessions
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    AugmentationOutputObjects <- c("AugmentedDataSet",
                                   "AugmentationReport",
                                   "AugmentationMessages")

    for(i in 1:length(AugmentationOutputObjects))
    {
        # Construct the server-side function call
        ServerCall <- call("ExtractFromListDS",
                           ListName.S = OutputName,
                           ObjectName.S = AugmentationOutputObjects[i])

        # Execute server-side assign function
        DSI::datashield.assign(conns = DataSources,
                               symbol = AugmentationOutputObjects[i],
                               value = ServerCall)

        # Call helper function to check if object assignment succeeded
        Messages$Assignment <- c(Messages$Assignment,
                                 ds.GetObjectStatus(ObjectName = AugmentationOutputObjects[i],
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
