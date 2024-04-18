
#' ds.GetReportingObject
#'
#' Receives a reporting object from servers. Its name must be on a list of permitted object names to ensure data privacy.
#'
#' Linked to server-side AGGREGATE method GetReportingObjectDS()
#'
#' @param ObjectName String | Name of reporting object on server. Must be on a list of permitted object names.
#' @param DataSources List of DSConnection objects
#'
#' @return A list of reporting objects
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.GetReportingObject <- function(ObjectName,
                                  DataSources = NULL)
{
    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }

    ServerCall <- call("GetReportingObjectDS",
                       ObjectName.S = ObjectName)

    ReportingObjects <- DSI::datashield.aggregate(conns = DataSources,
                                                  expr = ServerCall)

    return(ReportingObjects)
}
