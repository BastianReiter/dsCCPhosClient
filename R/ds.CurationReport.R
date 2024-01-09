
#' ds.CurationReport
#'
#' What it does
#'
#' Linked to server-side AGGREGATE method CurationReportDS()
#'
#' @param Name_CurationOutput String | Name of object on server previously assigned by dsCCPhos::CurateData() | Default: 'CurationOutput'
#' @param DataSources List of DSConnection objects
#'
#' @return
#' @export
#'
#' @examples
ds.CurationReport <- function(Name_CurationOutput = "CurationOutput",
                              DataSources = NULL)
{
    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }

    ServerCall <- call("CurationReportDS", Name_CurationOutput)

    Outgoing <- DSI::datashield.aggregate(conns = DataSources,
                                          expr = ServerCall)

    return(Outgoing)
}
