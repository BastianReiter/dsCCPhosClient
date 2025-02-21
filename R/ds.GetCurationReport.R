
#' ds.GetCurationReport
#'
#' Receive Curation Reports from sites and create cumulated reporting objects.
#'
#' Linked to server-side AGGREGATE method GetReportingObjectDS()
#'
#' @param DataSources List of DSConnection objects
#'
#' @return A list of Curation Reports
#' @export
#'
#' @author Bastian Reiter
ds.GetCurationReport <- function(DataSources = NULL)
{
    require(dplyr)
    require(purrr)

    ### For testing purposes
    # DataSources <- CCPConnections

    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 1) Get CurationReport objects from sites (as a list of lists)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    SiteCall <- call("GetReportingObjectDS",
                       ObjectName.S = "CurationReport")

    CurationReports <- DSI::datashield.aggregate(conns = DataSources,
                                                 expr = SiteCall)

    # Turn returned list 'inside-out' using purrr::list_transpose() for easier processing
    CurationReports <- CurationReports %>% list_transpose()



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 2) Cumulation of site-specific reports
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #   a) Entry Counts
    #   b) Transformation Monitor objects
    #         i) Detailed monitors
    #         ii) Eligibility overviews
    #         iii) Value set overviews
    #   c) Diagnosis classification
    #---------------------------------------------------------------------------


    # 2 a) Entry Counts
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    AllSitesEntryCounts <- data.frame()

    # Row-bind data frames from different sites
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for (j in 1:length(DataSources))      # Loop through all sites
    {
        # Get site-specific entry count table and attach column with site name
        SiteEntryCounts <- CurationReports$EntryCounts[[j]] %>%
                                mutate(Site = names(DataSources)[j])

        # Row-bind all site-specific entry count tables
        AllSitesEntryCounts <- AllSitesEntryCounts %>%
                                    bind_rows(SiteEntryCounts)
    }

    # Create table of cumulated entry counts
    if (nrow(AllSitesEntryCounts) > 0)
    {
        EntryCountsCumulated <- AllSitesEntryCounts %>%
                                    group_by(Table) %>%
                                        summarize(across(c(everything(), -Site), sum)) %>%
                                    ungroup() %>%
                                    mutate(Site = "All")

    } else { EntryCountsCumulated <- data.frame() }

    # Row-binding site-specific and cumulated entry counts to get one coherent data.frame
    AllEntryCounts <- EntryCountsCumulated %>%
                          bind_rows(AllSitesEntryCounts) %>%
                          mutate(ExcludedPrimary_Proportion = ExcludedPrimary / InitialCount,
                                 AfterPrimaryExclusion_Proportion = AfterPrimaryExclusion / InitialCount,
                                 ExcludedSecondary_Proportion = ExcludedSecondary / InitialCount,
                                 AfterSecondaryExclusion_Proportion = AfterSecondaryExclusion / InitialCount) %>%
                          select(Table,
                                 Site,
                                 InitialCount,
                                 ExcludedPrimary,
                                 ExcludedPrimary_Proportion,
                                 AfterPrimaryExclusion,
                                 AfterPrimaryExclusion_Proportion,
                                 ExcludedSecondary,
                                 ExcludedSecondary_Proportion,
                                 AfterSecondaryExclusion,
                                 AfterSecondaryExclusion_Proportion)

    # Create list of data.frames (one per RDS table) containing data on entry counts, comparing all sites
    EntryCounts <- split(AllEntryCounts, AllEntryCounts$Table) %>%
                        imap(function(Table, tablename)
                             {
                                Table %>% select(-Table)
                             })


    # 2 b) Transformation objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #   i) Detailed monitors
    #   ii) Eligibility overviews
    #   iii) Value set overviews
    #---------------------------------------------------------------------------

    TransformationMonitorsCumulated <- list()
    EligibilityOverviewsCumulated <- list()
    ValueSetOverviewsCumulated <- list()

    for (i in 1:length(CurationReports$Transformation[[1]]$Monitors))      # Loop through all transformation monitor tables (Diagnosis, Histology, etc.)
    {
        AllSitesMonitor <- data.frame()

        AllSitesEligibilityOverview <- data.frame()

        AllSitesValueSetOverview_Raw <- data.frame()
        AllSitesValueSetOverview_Harmonized <- data.frame()
        AllSitesValueSetOverview_Recoded <- data.frame()
        AllSitesValueSetOverview_Final <- data.frame()


        # 1) Row-bind data frames from different sites
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        for (j in 1:length(DataSources))      # Loop through all sites
        {
            SiteMonitor <- CurationReports$Transformation[[j]]$Monitors[[i]]
            SiteEligibilityOverview <- CurationReports$Transformation[[j]]$EligibilityOverviews[[i]]

            SiteValueSetOverview_Raw <- CurationReports$Transformation[[j]]$ValueSetOverviews[[i]]$Raw
            SiteValueSetOverview_Harmonized <- CurationReports$Transformation[[j]]$ValueSetOverviews[[i]]$Harmonized
            SiteValueSetOverview_Recoded <- CurationReports$Transformation[[j]]$ValueSetOverviews[[i]]$Recoded
            SiteValueSetOverview_Final <- CurationReports$Transformation[[j]]$ValueSetOverviews[[i]]$Final

            # Monitor table
            if (!is.null(SiteMonitor))
            {
                SiteMonitor <- SiteMonitor %>%
                                    mutate(TemporarySiteID = j)      # Create temporary site ID for processing

                # Row-bind all site-specific transformation monitor tables
                AllSitesMonitor <- AllSitesMonitor %>%
                                        bind_rows(SiteMonitor)
            }


            # Eligibility overview
            AllSitesEligibilityOverview <- AllSitesEligibilityOverview %>%
                                                bind_rows(SiteEligibilityOverview)


            # Value set overview (separate for different transformation stages)
            AllSitesValueSetOverview_Raw <- AllSitesValueSetOverview_Raw %>%
                                                bind_rows(SiteValueSetOverview_Raw)

            AllSitesValueSetOverview_Harmonized <- AllSitesValueSetOverview_Harmonized %>%
                                                        bind_rows(SiteValueSetOverview_Harmonized)

            AllSitesValueSetOverview_Recoded <- AllSitesValueSetOverview_Recoded %>%
                                                    bind_rows(SiteValueSetOverview_Recoded)

            AllSitesValueSetOverview_Final <- AllSitesValueSetOverview_Final %>%
                                                  bind_rows(SiteValueSetOverview_Final)
        }


        # 2) i) Consolidate cumulated detailed monitor tables
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (nrow(AllSitesMonitor) > 0)      # In case 'AllSitesMonitor' is not empty
        {
            # Get summarized counts of raw values
            SummaryRawValues <- AllSitesMonitor %>%
                                    filter(IsOccurring == TRUE) %>%
                                    distinct(pick(TemporarySiteID,      # This makes sure that per site exactly one (and only one) 'instance' of a particular value is counted
                                                  Feature,
                                                  Value_Raw,
                                                  Count_Raw)) %>%
                                    group_by(Feature,
                                             Value_Raw) %>%
                                    summarize(Count_Raw = sum(Count_Raw, na.rm = TRUE))

            # Get summarized counts of harmonized values
            SummaryHarmonizedValues <- AllSitesMonitor %>%
                                            distinct(pick(TemporarySiteID,
                                                          Feature,
                                                          Value_Harmonized,
                                                          Count_Harmonized)) %>%
                                            group_by(Feature,
                                                     Value_Harmonized) %>%
                                            summarize(Count_Harmonized = sum(Count_Harmonized, na.rm = TRUE))

            # Get summarized counts of recoded values
            SummaryRecodedValues <- AllSitesMonitor %>%
                                        distinct(pick(TemporarySiteID,
                                                      Feature,
                                                      Value_Recoded,
                                                      Count_Recoded)) %>%
                                        group_by(Feature,
                                                 Value_Recoded) %>%
                                        summarize(Count_Recoded = sum(Count_Recoded, na.rm = TRUE))

            # Get summarized counts of final values
            SummaryFinalValues <- AllSitesMonitor %>%
                                      distinct(pick(TemporarySiteID,
                                                    Feature,
                                                    Value_Final,
                                                    Count_Final)) %>%
                                      group_by(Feature,
                                               Value_Final) %>%
                                      summarize(Count_Final = sum(Count_Final, na.rm = TRUE))


            AllSitesMonitor <- AllSitesMonitor %>%
                                    select(-TemporarySiteID,
                                           -Count_Raw,
                                           -Count_Harmonized,
                                           -Count_Recoded,
                                           -Count_Final) %>%
                                    distinct() %>%
                                    #--- Delete remnant values marked as non-occurring that actually occur on some site ---
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
                                             list(AllSitesMonitor))


        # 2) ii) Consolidate cumulated eligibility overview tables
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (nrow(AllSitesEligibilityOverview) > 0)
        {
            AllSitesEligibilityOverview <- AllSitesEligibilityOverview %>%
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
                                           list(AllSitesEligibilityOverview))


        # 2) iii) Consolidate cumulated value set overview tables
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        if (nrow(AllSitesValueSetOverview_Raw) > 0)
        {
            AllSitesValueSetOverview_Raw <- AllSitesValueSetOverview_Raw %>%
                                                group_by(Feature, Value_Raw, IsOccurring, IsEligible_Raw) %>%
                                                    summarize(Count_Raw = sum(Count_Raw, na.rm = TRUE)) %>%
                                                    arrange(desc(IsOccurring), .by_group = TRUE) %>%
                                                    slice_head() %>%      # Remove / Replace values marked as non-occurring
                                                group_by(Feature) %>%
                                                    mutate(Proportion_Raw = Count_Raw / sum(Count_Raw, na.rm = TRUE)) %>%
                                                ungroup()
        }

        if (nrow(AllSitesValueSetOverview_Harmonized) > 0)
        {
            AllSitesValueSetOverview_Harmonized <- AllSitesValueSetOverview_Harmonized %>%
                                                        group_by(Feature, Value_Harmonized, IsEligible_Harmonized) %>%
                                                            summarize(Count_Harmonized = sum(Count_Harmonized, na.rm = TRUE)) %>%
                                                        group_by(Feature) %>%
                                                            mutate(Proportion_Harmonized = Count_Harmonized / sum(Count_Harmonized, na.rm = TRUE)) %>%
                                                        ungroup()
        }

        if (nrow(AllSitesValueSetOverview_Recoded) > 0)
        {
            AllSitesValueSetOverview_Recoded <- AllSitesValueSetOverview_Recoded %>%
                                                    group_by(Feature, Value_Recoded, IsEligible_Recoded) %>%
                                                        summarize(Count_Recoded = sum(Count_Recoded, na.rm = TRUE)) %>%
                                                    group_by(Feature) %>%
                                                        mutate(Proportion_Recoded = Count_Recoded / sum(Count_Recoded, na.rm = TRUE)) %>%
                                                    ungroup()
        }

        if (nrow(AllSitesValueSetOverview_Final) > 0)
        {
            AllSitesValueSetOverview_Final <- AllSitesValueSetOverview_Final %>%
                                                  group_by(Feature, Value_Final, IsEligible_Final) %>%
                                                      summarize(Count_Final = sum(Count_Final, na.rm = TRUE)) %>%
                                                  group_by(Feature) %>%
                                                      mutate(Proportion_Final = Count_Final / sum(Count_Final, na.rm = TRUE)) %>%
                                                  ungroup()
        }

        ValueSetOverviewsCumulated <- c(ValueSetOverviewsCumulated,
                                        list(Raw = AllSitesValueSetOverview_Raw,
                                             Harmonized = AllSitesValueSetOverview_Harmonized,
                                             Recoded = AllSitesValueSetOverview_Recoded,
                                             Final = AllSitesValueSetOverview_Final))
    }

    names(TransformationMonitorsCumulated) <- names(CurationReports$Transformation[[1]]$Monitors)
    names(EligibilityOverviewsCumulated) <- names(CurationReports$Transformation[[1]]$EligibilityOverviews)
    names(TransformationMonitorsCumulated) <- names(CurationReports$Transformation[[1]]$ValueSetOverviews)


    # 2 c) Summarize site-specific reports: Diagnosis Classification
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Bind rows of site-specific vectors
    DiagnosisClassification_Sites <- as_tibble(do.call(rbind, CurationReports$DiagnosisClassification))

    # Add row with column sums and add site name feature
    DiagnosisClassificationTable <- colSums(DiagnosisClassification_Sites) %>%
                                        bind_rows(DiagnosisClassification_Sites) %>%
                                        mutate(SiteName = c("All", names(DataSources)), .before = 1)



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Return list
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    return(list(EntryCounts = EntryCounts,
                Transformation = c(list(All = list(Monitors = TransformationMonitorsCumulated,
                                                   EligibilityOverviews = EligibilityOverviewsCumulated,
                                                   ValueSetOverviews = ValueSetOverviewsCumulated)),
                                   CurationReports$Transformation),
                DiagnosisClassification = DiagnosisClassificationTable))
}
