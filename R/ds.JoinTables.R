
#' ds.JoinTables
#'
#' Join tables on server, making use of \code{dplyr} mutating join operations.
#'
#' Linked to server-side ASSIGN method JoinTablesDS()
#'
#' @param TableNameA \code{string} - Name of Table A on server
#' @param TableNameB \code{string} - Name of Table B on server
#' @param ByStatement \code{string} - The insides of a \code{dplyr::join_by()}-Statement defining how to join tables
#' @param JoinType \code{string} - Name of \code{dplyr::join}-function used, one of:
#'                     \itemize{\item 'left_join' (Default)
#'                              \item 'right_join'
#'                              \item 'full_join'
#'                              \item 'inner_join'}
#' @param OutputName \code{string} - Name of resulting table on server
#'
#' @return A list of messages about object assignment for monitoring purposes
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ds.JoinTables <- function(TableNameA,
                          TableNameB,
                          ByStatement,
                          JoinType = "left_join",
                          OutputName,
                          DataSources = NULL)
{
    ### For testing purposes
    # TableNameA <- "ADS_Patient"
    # TableNameB <- "ADS_Diagnosis"
    # ByStatement <- "PatientID"
    # JoinType <- "left_join"
    # OutputName <- "PatientAnalysis"
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


    # Construct the server-side function call
    ServerCall <- call("JoinTablesDS",
                       TableNameA.S = TableNameA,
                       TableNameB.S = TableNameB,
                       ByStatement.S = ByStatement,
                       JoinType.S = JoinType)

    # Execute server-side assign function
    DSI::datashield.assign(conns = DataSources,
                           symbol = OutputName,
                           value = ServerCall)

    # Call helper function to check if object assignment succeeded
    AssignmentInfo <- ds.GetObjectStatus(OutputName,
                                         DataSources = DataSources)

    return(AssignmentInfo)
}
