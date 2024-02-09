
#' ds.GetValidationReport_RawData
#'
#' Triggers validation of Raw Data Set on server and requests a report.
#'
#' Linked to server-side AGGREGATE method GetValidationReportDS_RawData()
#'
#' @param Name_RawDataSet String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of tibbles containing output of validation
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.GetValidationReport_RawData <- function(Name_RawDataSet = "RawDataSet",
                                           DataSources = NULL)
{
    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }

    ServerCall <- call("GetValidationReportDS_RawData", Name_RawDataSet)

    Outgoing <- DSI::datashield.aggregate(conns = DataSources,
                                          expr = ServerCall)

    return(Outgoing)
}
