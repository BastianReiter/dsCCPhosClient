
#' ds.GetSampleStatistics
#'
#' Making use of dsBaseClient::ds.meanSdGp() and dsBaseClient::ds.quantileMean() to provide common parametric and nonparametric statistics about a metric feature.
#'
#' Linked to server-side AGGREGATE dsBase-functions
#'
#' @param TableName String | Name of the table containing the metric feature of concern
#' @param MetricFeatureName String | Name of metric feature
#' @param GroupingFeatureName String | Name of optional grouping feature from the same table
#' @param DataSources List of DSConnection objects
#'
#' @return
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.GetSampleStatistics <- function(TableName,
                                   MetricFeatureName,
                                   GroupingFeatureName = NULL,
                                   DataSources = NULL)
{
    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }

    require(dsBaseClient)

    # ServerCall <- call("GetSampleStatisticsDS",
    #                    TableName,
    #                    MetricFeatureName,
    #                    GroupingFeatureName,
    #                    na.rm)
    #
    # ServerResults <- DSI::datashield.aggregate(conns = DataSources,
    #                                            expr = ServerCall)

    # For Testing Purposes
    # TableName <- "ADS_Patients"
    # MetricFeatureName <- "PatientAgeAtDiagnosis"
    # DataSources <- CCPConnections


    # --- TO DO --- : Implement grouping on server and execute functions below on grouped vectors


    ls_ParametricStatistics <- ds.meanSdGp(x = paste0(TableName, "$", MetricFeatureName),
                                           y = "1",
                                           datasources = DataSources)

    ls_NonParametricStatistics_Split <- ds.quantileMean(x = paste0(TableName, "$", MetricFeatureName),
                                                        type = "split",
                                                        datasources = DataSources)

    vc_NonParametricStatistics_Combined <- ds.quantileMean(x = paste0(TableName, "$", MetricFeatureName),
                                                           type = "combine",
                                                           datasources = DataSources)

    df_NonParametricStatistics <- as.data.frame(rbind(t(as.data.frame(ls_NonParametricStatistics_Split)),
                                                      "All" = vc_NonParametricStatistics_Combined))

    #--- Site names and sample sizes ---
    Col_SiteNames <- colnames(ls_ParametricStatistics$Nvalid_gp_study)
    Col_SiteNames <- replace(Col_SiteNames, Col_SiteNames == "COMBINE", "All")
    Col_N <- as.vector(ls_ParametricStatistics$Nvalid_gp_study)
    #--- Nonparametric Statistics ---
    Col_q5 <- df_NonParametricStatistics$`5%`
    Col_Q1 <- df_NonParametricStatistics$`25%`
    Col_Median <- df_NonParametricStatistics$`50%`
    Col_Q3 <- df_NonParametricStatistics$`75%`
    Col_q95 <- df_NonParametricStatistics$`95%`
    #--- Parametric Statistics ---
    Col_Mean <- as.vector(ls_ParametricStatistics$Mean_gp_study)
    Col_SD <- as.vector(ls_ParametricStatistics$StDev_gp_study)
    Col_SEM <- as.vector(ls_ParametricStatistics$SEM_gp_study)


    df_Output <- tibble(Site = Col_SiteNames,
                        N = Col_N,
                        q5 = Col_q5,
                        Q1 = Col_Q1,
                        Median = Col_Median,
                        Q3 = Col_Q3,
                        q95 = Col_q95,
                        Mean = Col_Mean,
                        SD = Col_SD,
                        SEM = Col_SEM)

    return(df_Output)
}
