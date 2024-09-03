
#' ds.GetRDSTableCheck
#'
#' Triggers a function that checks raw data set (RDS) tables on servers.
#'
#' Linked to server-side AGGREGATE method GetRDSTableCheckDS()
#'
#' @param DataSources List of DSConnection objects
#' @param RawDataSetName String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
#'
#' @return A list of reporting objects
#' @export
#'
#' @author Bastian Reiter
ds.GetRDSTableCheck <- function(DataSources = NULL,
                                RawDataSetName = "RawDataSet")
{
    # For Testing Purposes
    DataSources <- CCPConnections
    RawDataSetName <- "RawDataSet"


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check argument eligibility
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (!(is.character(RawDataSetName)))
    {
        stop("Error: Argument 'RawDataSetName' must be a character string.", call. = FALSE)
    }

    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Server function call
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ServerCall <- call("GetRDSTableCheckDS",
                       RawDataSetName.S = RawDataSetName)

    TableCheck <- DSI::datashield.aggregate(conns = DataSources,
                                            expr = ServerCall)

    return(TableCheck)
}
