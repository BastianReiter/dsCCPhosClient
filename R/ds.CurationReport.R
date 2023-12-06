
#' ds.GetCurationMonitor
#'
#' @return
#' @export
#'
#' @examples
ds.CurationReport <- function(DataSources = NULL)
{
    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }

    ServerCall <- "CurationReportDS()"

    Outgoing <- DSI::datashield.aggregate(conns = DataSources,
                                        expr = as.symbol(ServerCall))

    return(Outgoing)
}
