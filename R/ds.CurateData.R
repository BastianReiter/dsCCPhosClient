
#' ds.CurateData
#'
#' Triggers transformation of Raw Data Set (RDS) into Curated Data Set (CDS) on server.
#'
#' Linked to server-side ASSIGN methods dsCCPhos::CurateDataDS() and dsCCPhos::ExtractFromListDS()
#'
#' @param RawDataSetName String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
#' @param OutputName String | Name of output object to be assigned on server | Default: 'CurationOutput'
#' @param RuleProfile_RawDataTransformation String | Profile name defining rule set to be used for data transformation. Profile name must be stated in \code{\link{RuleSet_RawDataTransformation}. | Default: 'Default'
#' @param RuleProfile_DiagnosisRedundancy String | Profile name defining rule set to be used for classification of diagnosis redundancies. Profile name must be stated in \code{\link{RuleSet_DiagnosisRedundancy}. | Default: 'Default'
#' @param RuleProfile_DiagnosisAssociation String | Profile name defining rule set to be used for classification of diagnosis associations. Profile name must be stated in \code{\link{RuleSet_DiagnosisAssociation}. | Default: 'Default'
#' @param DataSources List of DSConnection objects
#'
#' @return Info messages concerning completion of CurateDataDS() and assignment of the following objects on server:
#'         \itemize{\item CurationOutput (list)
#'                  \item CuratedDataSet (list)
#'                  \item CurationReport (list)
#'                  \item CurationMessages (list)}
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.CurateData <- function(RawDataSetName = "RawDataSet",
                          OutputName = "CurationOutput",
                          RuleProfile_RawDataTransformation = "Default",
                          RuleProfile_DiagnosisRedundancy = "Default",
                          RuleProfile_DiagnosisAssociation = "Default",
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


    # Initiate output messaging objects
    Messages <- list()
    #Messages$Completion <- character()
    Messages$Assignment <- list()



    # 1) Trigger dsCCPhos::CurateDataDS()
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Construct the the server-side function call
    ServerCall <- call("CurateDataDS",
                       RawDataSetName.S = RawDataSetName,
                       RuleProfile_RawDataTransformation.S = RuleProfile_RawDataTransformation,
                       RuleProfile_DiagnosisRedundancy.S = RuleProfile_DiagnosisRedundancy,
                       RuleProfile_DiagnosisAssociation.S = RuleProfile_DiagnosisAssociation)

    # Execute the server-side function call
    DSI::datashield.assign(conns = DataSources,
                           symbol = OutputName,
                           value = ServerCall)

    # Call helper function to check if assignment of CurationOutput succeeded
    Messages$Assignment <- c(Messages$Assignment,
                             ds.GetObjectInfo(ObjectName = OutputName,
                                              DataSources = DataSources))



    # 2) Extract objects from list returned by CurateDataDS() and assign them to R server sessions
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    CurationOutputObjects <- c("CuratedDataSet",
                               "CurationReport",
                               "CurationMessages")

    for(i in 1:length(CurationOutputObjects))
    {
        # Construct the server-side function call
        ServerCall <- call("ExtractFromListDS",
                           ListName.S = OutputName,
                           ObjectName.S = CurationOutputObjects[i])

        # Execute server-side assign function
        DSI::datashield.assign(conns = DataSources,
                               symbol = CurationOutputObjects[i],
                               value = ServerCall)

        # Call helper function to check if object assignment succeeded
        Messages$Assignment <- c(Messages$Assignment,
                                 ds.GetObjectInfo(ObjectName = CurationOutputObjects[i],
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
