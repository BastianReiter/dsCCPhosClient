
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

    # For testing purposes
    #DataSources <- CCPConnections

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


    # 2 A) Summarize server-specific reports: Unlinked Entries
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Initiate summary with vector from first server
    UnlinkedEntriesSummary <- CurationReports[[1]]$UnlinkedEntries

    if (length(CurationReports) > 1)
    {
        for (i in 2:length(CurationReports))      # Loop through all other servers
        {
            UnlinkedEntriesSummary <- UnlinkedEntriesSummary + CurationReports[[i]]$UnlinkedEntries
        }
    }


    # 2 B) Summarize server-specific reports: Transformation
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    TransformationMonitorsSummary <- list()

    for (i in 1:length(CurationReports[[1]]$Transformation))      # Loop through all transformation monitor tables (Diagnosis, Histology, etc.)
    {
        SummaryOfTable <- data.frame()

        for (j in 1:length(CurationReports))      # Loop through all servers
        {
            SummaryOfTable <- SummaryOfTable %>%
                                  bind_rows(CurationReports[[j]]$Transformation[[i]])
        }

        if (nrow(SummaryOfTable) > 0)      # In case 'SummaryOfTable' is not empty
        {
            SummaryOfTable <- SummaryOfTable %>%
                                  group_by(Feature, Value, IsValueEligible) %>%
                                  summarize(Raw = sum(Raw),
                                            Transformed = sum(Transformed),
                                            Final = sum(Final))

        }

        TransformationMonitorsSummary <- c(TransformationMonitorsSummary,
                                           list(SummaryOfTable))
    }

    names(TransformationMonitorsSummary) <- names(CurationReports[[1]]$Transformation)


    # 2 C) Summarize server-specific reports: Diagnosis Classification
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Initiate summary with vector from first server
    DiagnosisClassificationSummary <- CurationReports[[1]]$DiagnosisClassification

    if (length(CurationReports) > 1)
    {
        for (i in 2:length(CurationReports))      # Loop through all other servers
        {
            DiagnosisClassificationSummary <- DiagnosisClassificationSummary + CurationReports[[i]]$DiagnosisClassification
        }
    }


    # Return list of server-specific and summarized Curation Reports
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    return(c(CurationReports,
             "All" = list(list(UnlinkedEntries = UnlinkedEntriesSummary,
                               Transformation = TransformationMonitorsSummary,
                               DiagnosisClassification = DiagnosisClassificationSummary))))
}
