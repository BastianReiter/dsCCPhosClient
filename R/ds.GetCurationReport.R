
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
    UnlinkedEntriesCumulated <- CurationReports[[1]]$UnlinkedEntries

    if (length(CurationReports) > 1)
    {
        for (i in 2:length(CurationReports))      # Loop through all other servers
        {
            UnlinkedEntriesCumulated <- UnlinkedEntriesCumulated + CurationReports[[i]]$UnlinkedEntries
        }
    }


    # 2 B) Summarize server-specific reports: Transformation
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    TransformationMonitorsCumulated <- list()

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
                                    filter(IsOccurring == TRUE) %>%
                                    distinct(pick(TemporaryServerID,      # This makes sure that per server only one 'instance' of a particular value is counted
                                                  Feature,
                                                  Value_Raw,
                                                  Count_Raw)) %>%
                                    group_by(Feature,
                                             Value_Raw) %>%
                                    summarize(Count_Raw = sum(Count_Raw, na.rm = TRUE))

            # Get summarized counts of harmonized values
            SummaryHarmonizedValues <- AllServersTable %>%
                                            distinct(pick(TemporaryServerID,
                                                          Feature,
                                                          Value_Harmonized,
                                                          Count_Harmonized)) %>%
                                            group_by(Feature,
                                                     Value_Harmonized) %>%
                                            summarize(Count_Harmonized = sum(Count_Harmonized, na.rm = TRUE))

            # Get summarized counts of recoded values
            SummaryRecodedValues <- AllServersTable %>%
                                        distinct(pick(TemporaryServerID,
                                                      Feature,
                                                      Value_Recoded,
                                                      Count_Recoded)) %>%
                                        group_by(Feature,
                                                 Value_Recoded) %>%
                                        summarize(Count_Recoded = sum(Count_Recoded, na.rm = TRUE))

            # Get summarized counts of final values
            SummaryFinalValues <- AllServersTable %>%
                                      distinct(pick(TemporaryServerID,
                                                    Feature,
                                                    Value_Final,
                                                    Count_Final)) %>%
                                      group_by(Feature,
                                               Value_Final) %>%
                                      summarize(Count_Final = sum(Count_Final, na.rm = TRUE))


            AllServersTable <- AllServersTable %>%
                                  select(-TemporaryServerID,
                                         -Count_Raw,
                                         -Count_Harmonized,
                                         -Count_Recoded,
                                         -Count_Final) %>%
                                  distinct() %>%
                                  #--- Delete remnant values marked as non-occurring that actually occur on some server ---
                                  group_by(Feature, Value_Raw) %>%
                                      arrange(desc(IsOccurring), .by_group = TRUE) %>%
                                      slice_head() %>%
                                  ungroup() %>%
                                  #--- Add cumulated value counts of different transformation stages ---
                                  left_join(SummaryRawValues, by = join_by(Feature, Value_Raw)) %>%
                                  left_join(SummaryHarmonizedValues, by = join_by(Feature, Value_Harmonized)) %>%
                                  left_join(SummaryRecodedValues, by = join_by(Feature, Value_Recoded)) %>%
                                  left_join(SummaryFinalValues, by = join_by(Feature, Value_Final)) %>%
                                  arrange(Feature,
                                          desc(IsOccurring),
                                          desc(IsEligible_Raw),
                                          desc(IsEligible_Harmonized),
                                          Value_Raw)
        }

        TransformationMonitorsCumulated <- c(TransformationMonitorsCumulated,
                                             list(AllServersTable))
    }

    names(TransformationMonitorsCumulated) <- names(CurationReports[[1]]$Transformation)


    # 2 C) Summarize server-specific reports: Diagnosis Classification
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Initiate summary with vector from first server
    DiagnosisClassificationCumulated <- CurationReports[[1]]$DiagnosisClassification

    if (length(CurationReports) > 1)
    {
        for (i in 2:length(CurationReports))      # Loop through all other servers
        {
            DiagnosisClassificationCumulated <- DiagnosisClassificationCumulated + CurationReports[[i]]$DiagnosisClassification
        }
    }


    # Return list of server-specific and summarized Curation Reports
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    return(c(CurationReports,
             "All" = list(list(UnlinkedEntries = UnlinkedEntriesCumulated,
                               Transformation = TransformationMonitorsCumulated,
                               DiagnosisClassification = DiagnosisClassificationCumulated))))
}
