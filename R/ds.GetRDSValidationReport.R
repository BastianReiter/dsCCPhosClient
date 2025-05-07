
#' ds.GetRDSValidationReport
#'
#' Triggers validation of Raw Data Set on server and requests a report.
#'
#' Linked to server-side AGGREGATE method GetRDSValidationReportDS()
#'
#' @param RawDataSetName String | Name of Raw Data Set object (list) on server | Default: 'RawDataSet'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of tibbles containing output of validation
#' @export
#'
#' @author Bastian Reiter
ds.GetRDSValidationReport <- function(DataSources = NULL,
                                      RawDataSetName = "RawDataSet")
{
    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }

    ServerCall <- call("GetRDSValidationReportDS",
                       RawDataSetName.S = RawDataSetName)

    ValidationReport <- DSI::datashield.aggregate(conns = DataSources,
                                                  expr = ServerCall)

    return(ValidationReport)
}
