
#' ds.GetCurationReport
#'
#' Receive Curation Reports from servers and create cumulated reporting objects.
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
    #   i) Detailed monitors
    #   ii) Eligibility overviews
    #   iii) Value set overviews
    #---------------------------------------------------------------------------

    TransformationMonitorsCumulated <- list()
    EligibilityOverviewsCumulated <- list()
    ValueSetOverviewsCumulated <- list()

    for (i in 1:length(CurationReports[[1]]$Transformation$Monitors))      # Loop through all transformation monitor tables (Diagnosis, Histology, etc.)
    {
        AllServersMonitor <- data.frame()

        AllServersEligibilityOverview <- data.frame()

        AllServersValueSetOverview_Raw <- data.frame()
        AllServersValueSetOverview_Harmonized <- data.frame()
        AllServersValueSetOverview_Recoded <- data.frame()
        AllServersValueSetOverview_Final <- data.frame()


        # 1) Row-bind data frames from different servers
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        for (j in 1:length(CurationReports))      # Loop through all servers
        {
            ServerMonitor <- CurationReports[[j]]$Transformation$Monitors[[i]]
            ServerEligibilityOverview <- CurationReports[[j]]$Transformation$EligibilityOverviews[[i]]

            ServerValueSetOverview_Raw <- CurationReports[[j]]$Transformation$ValueSetOverviews[[i]]$Raw
            ServerValueSetOverview_Harmonized <- CurationReports[[j]]$Transformation$ValueSetOverviews[[i]]$Harmonized
            ServerValueSetOverview_Recoded <- CurationReports[[j]]$Transformation$ValueSetOverviews[[i]]$Recoded
            ServerValueSetOverview_Final <- CurationReports[[j]]$Transformation$ValueSetOverviews[[i]]$Final

            # Monitor table
            if (!is.null(ServerMonitor))
            {
                ServerMonitor <- ServerMonitor %>%
                                      mutate(TemporaryServerID = j)      # Create temporary server ID for processing

                # Row-bind all server-specific transformation monitor tables
                AllServersMonitor <- AllServersMonitor %>%
                                          bind_rows(ServerMonitor)
            }


            # Eligibility overview
            AllServersEligibilityOverview <- AllServersEligibilityOverview %>%
                                                  bind_rows(ServerEligibilityOverview)


            # Value set overview (separate for different transformation stages)
            AllServersValueSetOverview_Raw <- AllServersValueSetOverview_Raw %>%
                                                  bind_rows(ServerValueSetOverview_Raw)

            AllServersValueSetOverview_Harmonized <- AllServersValueSetOverview_Harmonized %>%
                                                          bind_rows(ServerValueSetOverview_Harmonized)

            AllServersValueSetOverview_Recoded <- AllServersValueSetOverview_Recoded %>%
                                                      bind_rows(ServerValueSetOverview_Recoded)

            AllServersValueSetOverview_Final <- AllServersValueSetOverview_Final %>%
                                                    bind_rows(ServerValueSetOverview_Final)
        }


        # 2) i) Consolidate cumulated detailed monitor tables
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (nrow(AllServersMonitor) > 0)      # In case 'AllServersMonitor' is not empty
        {
            # Get summarized counts of raw values
            SummaryRawValues <- AllServersMonitor %>%
                                    filter(IsOccurring == TRUE) %>%
                                    distinct(pick(TemporaryServerID,      # This makes sure that per server exactly one (and only one) 'instance' of a particular value is counted
                                                  Feature,
                                                  Value_Raw,
                                                  Count_Raw)) %>%
                                    group_by(Feature,
                                             Value_Raw) %>%
                                    summarize(Count_Raw = sum(Count_Raw, na.rm = TRUE))

            # Get summarized counts of harmonized values
            SummaryHarmonizedValues <- AllServersMonitor %>%
                                            distinct(pick(TemporaryServerID,
                                                          Feature,
                                                          Value_Harmonized,
                                                          Count_Harmonized)) %>%
                                            group_by(Feature,
                                                     Value_Harmonized) %>%
                                            summarize(Count_Harmonized = sum(Count_Harmonized, na.rm = TRUE))

            # Get summarized counts of recoded values
            SummaryRecodedValues <- AllServersMonitor %>%
                                        distinct(pick(TemporaryServerID,
                                                      Feature,
                                                      Value_Recoded,
                                                      Count_Recoded)) %>%
                                        group_by(Feature,
                                                 Value_Recoded) %>%
                                        summarize(Count_Recoded = sum(Count_Recoded, na.rm = TRUE))

            # Get summarized counts of final values
            SummaryFinalValues <- AllServersMonitor %>%
                                      distinct(pick(TemporaryServerID,
                                                    Feature,
                                                    Value_Final,
                                                    Count_Final)) %>%
                                      group_by(Feature,
                                               Value_Final) %>%
                                      summarize(Count_Final = sum(Count_Final, na.rm = TRUE))


            AllServersMonitor <- AllServersMonitor %>%
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
                                             list(AllServersMonitor))


        # 2) ii) Consolidate cumulated eligibility overview tables
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (nrow(AllServersEligibilityOverview) > 0)
        {
            AllServersEligibilityOverview <- AllServersEligibilityOverview %>%
                                                  select(Feature,
                                                         Eligibility,
                                                         Raw,
                                                         Harmonized,
                                                         Recoded,
                                                         Final) %>%
                                                  group_by(Feature, Eligibility) %>%
                                                      summarize(across(c(Raw, Harmonized, Recoded, Final), ~ sum(.x, na.rm = TRUE))) %>%
                                                  group_by(Feature) %>%
                                                      mutate(across(c(Raw, Harmonized, Recoded, Final), ~ .x / sum(.x, na.rm = TRUE), .names = "{.col}_Proportional")) %>%
                                                  ungroup()
        }

        EligibilityOverviewsCumulated <- c(EligibilityOverviewsCumulated,
                                           list(AllServersEligibilityOverview))


        # 2) iii) Consolidate cumulated value set overview tables
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (nrow(AllServersValueSetOverview_Raw) > 0)
        {
            AllServersValueSetOverview_Raw <- AllServersValueSetOverview_Raw %>%
                                                  group_by(Feature, Value_Raw, IsOccurring, IsEligible_Raw) %>%
                                                      summarize(Count_Raw = sum(Count_Raw, na.rm = TRUE)) %>%
                                                      arrange(desc(IsOccurring), .by_group = TRUE) %>%
                                                      slice_head() %>%      # Remove / Replace values marked as non-occurring
                                                  group_by(Feature) %>%
                                                      mutate(Proportion_Raw = Count_Raw / sum(Count_Raw, na.rm = TRUE)) %>%
                                                  ungroup()
        }

        if (nrow(AllServersValueSetOverview_Harmonized) > 0)
        {
            AllServersValueSetOverview_Harmonized <- AllServersValueSetOverview_Harmonized %>%
                                                          group_by(Feature, Value_Harmonized, IsEligible_Harmonized) %>%
                                                              summarize(Count_Harmonized = sum(Count_Harmonized, na.rm = TRUE)) %>%
                                                          group_by(Feature) %>%
                                                              mutate(Proportion_Harmonized = Count_Harmonized / sum(Count_Harmonized, na.rm = TRUE)) %>%
                                                          ungroup()
        }

        if (nrow(AllServersValueSetOverview_Recoded) > 0)
        {
            AllServersValueSetOverview_Recoded <- AllServersValueSetOverview_Recoded %>%
                                                      group_by(Feature, Value_Recoded, IsEligible_Recoded) %>%
                                                          summarize(Count_Recoded = sum(Count_Recoded, na.rm = TRUE)) %>%
                                                      group_by(Feature) %>%
                                                          mutate(Proportion_Recoded = Count_Recoded / sum(Count_Recoded, na.rm = TRUE)) %>%
                                                      ungroup()
        }

        if (nrow(AllServersValueSetOverview_Final) > 0)
        {
            AllServersValueSetOverview_Final <- AllServersValueSetOverview_Final %>%
                                                    group_by(Feature, Value_Final, IsEligible_Final) %>%
                                                        summarize(Count_Final = sum(Count_Final, na.rm = TRUE)) %>%
                                                    group_by(Feature) %>%
                                                        mutate(Proportion_Final = Count_Final / sum(Count_Final, na.rm = TRUE)) %>%
                                                    ungroup()
        }

        ValueSetOverviewsCumulated <- c(ValueSetOverviewsCumulated,
                                        list(Raw = AllServersValueSetOverview_Raw,
                                             Harmonized = AllServersValueSetOverview_Harmonized,
                                             Recoded = AllServersValueSetOverview_Recoded,
                                             Final = AllServersValueSetOverview_Final))
    }

    names(TransformationMonitorsCumulated) <- names(CurationReports[[1]]$Transformation$Monitors)
    names(EligibilityOverviewsCumulated) <- names(CurationReports[[1]]$Transformation$EligibilityOverviews)
    names(TransformationMonitorsCumulated) <- names(CurationReports[[1]]$Transformation$ValueSetOverviews)



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

    return(c("All" = list(list(UnlinkedEntries = UnlinkedEntriesCumulated,      # The list for "All" must have the same structure and naming of return of dsCCPhos::CurateData() for every single server
                               Transformation = list(Monitors = TransformationMonitorsCumulated,
                                                     EligibilityOverviews = EligibilityOverviewsCumulated,
                                                     ValueSetOverviews = ValueSetOverviewsCumulated),
                               DiagnosisClassification = DiagnosisClassificationCumulated)),
           CurationReports))
}
