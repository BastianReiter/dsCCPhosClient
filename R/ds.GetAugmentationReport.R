



    # # 2 c) Summarize site-specific reports: Diagnosis Classification
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    # # Bind rows of site-specific vectors
    # DiagnosisClassification_Sites <- as_tibble(do.call(rbind, CurationReports$DiagnosisClassification))
    #
    # # Add row with column sums and add site name feature
    # DiagnosisClassificationTable <- colSums(DiagnosisClassification_Sites) %>%
    #                                     bind_rows(DiagnosisClassification_Sites) %>%
    #                                     mutate(SiteName = c("All", names(DataSources)), .before = 1)
