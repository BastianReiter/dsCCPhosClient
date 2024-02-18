
#' ds.GetCurationReport
#'
#' Receive Curation Reports from servers.
#'
#' Linked to server-side AGGREGATE method GetReportingObjectDS()
#'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of Curation Reports
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.GetCurationReport <- function(DataSources = NULL)
{
    require(dplyr)

    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }


    # 1) Get CurationReport objects from servers (as a list of lists)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ServerCall <- call("GetReportingObjectDS",
                       ObjectName.S = "CurationReport")

    CurationReports <- DSI::datashield.aggregate(conns = DataSources,
                                                 expr = ServerCall)


    # 2) Get an additional list of data frames that summarize numbers from all server-specific report data frames
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Summary <- list()

    for (i in 1:length(CurationReports[[1]]))      # Loop through all tables (Diagnosis, Histology, etc.)
    {
        SummaryOfTable <- data.frame()

        for (j in 1:length(CurationReports))      # Loop through all servers
        {
            SummaryOfTable <- SummaryOfTable %>%
                                  bind_rows(CurationReports[[j]][[i]])
        }

        if (nrow(SummaryOfTable) > 0)      # In case 'SummaryOfTable' is not empty
        {
            SummaryOfTable <- SummaryOfTable %>%
                                  group_by(Feature, Value, IsValueEligible) %>%
                                  summarize(Raw = sum(Raw),
                                            Transformed = sum(Transformed),
                                            Final = sum(Final))
        }

        Summary <- c(Summary,
                     list(SummaryOfTable))
    }

    names(Summary) <- names(CurationReports[[1]])


    # Return list of server-specific and summarized Curation Reports
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    return(c(CurationReports,
             "All" = list(Summary)))
}
