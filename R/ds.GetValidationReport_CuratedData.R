
#' ds.GetValidationReport_CuratedData
#'
#' Triggers validation of Curated Data Set on server and requests a report.
#'
#' Linked to server-side AGGREGATE method GetValidationReportDS_CuratedData()
#'
#' @param Name_CurationOutput String | Name of curation output object (list) on server | Default: 'CurationOutput'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of tibbles containing output of validation
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.GetValidationReport_CuratedData <- function(Name_CurationOutput = "CurationOutput",
                                               DataSources = NULL)
{
    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }

    ServerCall <- call("GetValidationReportDS_CuratedData", Name_CurationOutput)

    Outgoing <- DSI::datashield.aggregate(conns = DataSources,
                                          expr = ServerCall)

    return(Outgoing)
}
