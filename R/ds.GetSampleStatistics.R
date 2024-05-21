
#' ds.GetSampleStatistics
#'
#' Obtain common parametric and nonparametric statistics about a metric feature.
#' Making use of \code{dsBaseClient::ds.meanSdGp()} and \code{dsBaseClient::ds.quantileMean()}.
#'
#' Linked to server-side \code{AGGREGATE} function \code{dsCCPhos::GetSampleStatisticsDS()}.
#'
#' @param DataSources \code{list} of DSConnection objects
#' @param TableName \code{string} | Name of the table containing the feature of concern
#' @param MetricFeatureName \code{string} | Name of feature
#' @param GroupingFeatureName \code{string} | Name of optional grouping feature from the same table
#'
#' @return A \code{tibble} containing parametric and non-parametric sample statistics
#' @export
#'
#' @author Bastian Reiter
ds.GetSampleStatistics <- function(DataSources = NULL,
                                   TableName,
                                   MetricFeatureName,
                                   GroupingFeatureName = NULL,
                                   RemoveMissings = TRUE)
{
    # For Testing Purposes
    # DataSources <- CCPConnections
    # TableName <- "ADS_Patients"
    # MetricFeatureName <- "TNM_T"
    # GroupingFeatureName <- NULL
    # RemoveMissings <- TRUE


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check argument eligibility
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (!(is.character(TableName) & is.character(MetricFeatureName)))
    {
        stop("Error: Arguments 'TableName' and 'MetricFeatureName' must be character strings.", call. = FALSE)
    }

    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Package requirements
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    require(dsBaseClient)
    require(dplyr)
    require(purrr)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check if addressed objects (Table and Feature) are eligible
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get meta data of table object
    TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                          DataSources = DataSources)

    # Stop execution if referred table object is not a data.frame
    if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}

    # Get data type of feature in question
    FeatureType <- TableMetaData$FirstEligible$DataTypes[MetricFeatureName]

    # Stop function if referred feature is not of class 'numeric' or similar
    if (!(FeatureType %in% c("double", "integer", "numeric"))) { stop(paste0("Error: The referred feature '", MetricFeatureName, "' is of class '", FeatureType, "' and therefore not suitable."), call. = FALSE) }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Separate returns
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # SiteReturns: Obtain sample statistics for each server calling dsCCPhos::GetSampleStatisticsDS()
    ls_SiteReturns <- DSI::datashield.aggregate(conns = DataSources,
                                                expr = call("GetSampleStatisticsDS",
                                                            TableName.S = TableName,
                                                            MetricFeatureName.S = MetricFeatureName,
                                                            GroupingFeatureName.S = GroupingFeatureName,
                                                            RemoveMissings.S = RemoveMissings))

    # --- TO DO --- : Implement grouping on server and execute functions below on grouped vectors


    # Convert site returns into tibble containing separate statistics
    df_SeparateStatistics <- ls_SiteReturns %>%
                                  list_rbind(names_to = "Site")


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Cumulation
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Making use of dsBaseClient::ds.meadSdGp() to obtain CUMULATED parametric statistics
    ls_CumulatedStatistics_Parametric <- ds.meanSdGp(x = paste0(TableName, "$", MetricFeatureName),
                                                     y = "1",
                                                     datasources = DataSources)

    # Making use of dsBaseClient::ds.quantileMean() to obtain CUMULATED non-parametric statistics
    vc_CumulatedStatistics_Nonparametric <- ds.quantileMean(x = paste0(TableName, "$", MetricFeatureName),
                                                            type = "combine",
                                                            datasources = DataSources)

    # Compiling cumulated statistics
    df_CumulatedStatistics <- tibble(Site = "All",
                                     N = ls_CumulatedStatistics_Parametric$Nvalid_gp_study[1, "COMBINE"],
                                     q5 = vc_CumulatedStatistics_Nonparametric["5%"],
                                     Q1 = vc_CumulatedStatistics_Nonparametric["25%"],
                                     Median = vc_CumulatedStatistics_Nonparametric["50%"],
                                     Q3 = vc_CumulatedStatistics_Nonparametric["75%"],
                                     q95 = vc_CumulatedStatistics_Nonparametric["95%"],
                                     MAD = NA,
                                     Mean = ls_CumulatedStatistics_Parametric$Mean_gp_study[1, "COMBINE"],
                                     SD = ls_CumulatedStatistics_Parametric$StDev_gp_study[1, "COMBINE"],
                                     SEM = ls_CumulatedStatistics_Parametric$SEM_gp_study[1, "COMBINE"])


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Glue cumulated and separate statistics together and return tibble
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(bind_rows(df_CumulatedStatistics,
                     df_SeparateStatistics))
}
