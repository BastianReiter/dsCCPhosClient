
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
        AllServersTable <- data.frame()

        for (j in 1:length(CurationReports))      # Loop through all servers
        {
            ServerTable <- CurationReports[[j]]$Transformation[[i]]

            if (!is.null(ServerTable))
            {
                ServerTable <- ServerTable %>%
                                    mutate(TemporaryServerID = j)      # Create temporary server ID for processing

                # Row-bind all server-specific transformation monitor tables
                AllServersTable <- AllServersTable %>%
                                      bind_rows(ServerTable)
            }
        }

        if (nrow(AllServersTable) > 0)      # In case 'AllServersTable' is not empty
        {
            # Get summarized counts of raw values
            SummaryRawValues <- AllServersTable %>%
                                    distinct(pick(TemporaryServerID,      # This makes sure that per server only one 'instance' of a particular value is counted
                                                  Feature,
                                                  Value_Raw,
                                                  Count_Raw)) %>%
                                    group_by(Feature,
                                             Value_Raw) %>%
                                    summarize(Count_Raw = sum(Count_Raw, na.rm = TRUE))

            # Get summarized counts of transformed values
            SummaryTransformedValues <- AllServersTable %>%
                                            distinct(pick(TemporaryServerID,      # This makes sure that per server only one 'instance' of a particular value is counted
                                                          Feature,
                                                          Value_Transformed,
                                                          Count_Transformed)) %>%
                                            group_by(Feature,
                                                     Value_Transformed) %>%
                                            summarize(Count_Transformed = sum(Count_Transformed, na.rm = TRUE))

            # Get summarized counts of final values
            SummaryFinalValues <- AllServersTable %>%
                                      distinct(pick(TemporaryServerID,      # This makes sure that per server only one 'instance' of a particular value is counted
                                                    Feature,
                                                    Value_Final,
                                                    Count_Final)) %>%
                                      group_by(Feature,
                                               Value_Final) %>%
                                      summarize(Count_Final = sum(Count_Final, na.rm = TRUE))


            AllServersTable <- AllServersTable %>%
                                  select(-TemporaryServerID,
                                         -Count_Raw,
                                         -Count_Transformed,
                                         -Count_Final) %>%
                                  distinct() %>%
                                  left_join(SummaryRawValues, by = join_by(Feature, Value_Raw)) %>%
                                  left_join(SummaryTransformedValues, by = join_by(Feature, Value_Transformed)) %>%
                                  left_join(SummaryFinalValues, by = join_by(Feature, Value_Final)) %>%
                                  arrange(Feature,
                                          desc(IsOccurring),
                                          desc(IsEligible_Raw),
                                          desc(IsEligible_Transformed),
                                          Value_Raw)
        }

        TransformationMonitorsSummary <- c(TransformationMonitorsSummary,
                                           list(AllServersTable))
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
