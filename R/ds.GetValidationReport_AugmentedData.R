
#' ds.GetValidationReport_AugmentedData
#'
#' Triggers validation of Augmented Data Set on server and requests a report.
#'
#' Linked to server-side AGGREGATE method GetValidationReportDS_AugmentedData()
#'
#' @param Name_AugmentedDataSet String | Name of Augmented Data Set object (list) on server | Default: 'AugmentedDataSet'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of tibbles containing output of validation
#' @export
#'
#' @examples
ds.GetValidationReport_AugmentedData <- function(Name_AugmentedDataSet = "AugmentedDataSet",
                                                 DataSources = NULL)
{
    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }

    ServerCall <- call("GetValidationReportDS_AugmentedData", Name_AugmentedDataSet)

    Outgoing <- DSI::datashield.aggregate(conns = DataSources,
                                          expr = ServerCall)

    return(Outgoing)
}
