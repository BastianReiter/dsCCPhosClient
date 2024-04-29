
#' ds.GetSampleStatistics
#'
#' Get summarizing univariate statistics about a feature of arbitrary data type.
#' Making use of dsBaseClient::ds.meanSdGp() and dsBaseClient::ds.quantileMean() to provide common parametric and nonparametric statistics about a metric feature.
#'
#' Linked to server-side AGGREGATE dsBase-functions
#'
#' @param TableName String | Name of the table containing the feature of concern
#' @param FeatureName String | Name of feature
#' @param GroupingFeatureName String | Name of optional grouping feature from the same table
#' @param DataSources List of DSConnection objects
#'
#' @return
#' @export
#'
#' @examples
#' @author Bastian Reiter
ds.GetSampleStatistics <- function(DataSources = NULL,
                                   TableName,
                                   FeatureName,
                                   GroupingFeatureName = NULL,
                                   MaxNumberCategories = NULL,
                                   RemoveMissingsInNumeric = TRUE)
{
    # For Testing Purposes
    DataSources <- CCPConnections
    TableName <- "ADS_Patients"
    FeatureName <- "PatientAgeAtDiagnosis"
    GroupingFeatureName <- NULL
    MaxNumberCategories <- NULL
    RemoveMissingsInNumeric <- TRUE



    if (!(is.character(TableName) & is.character(FeatureName) & is.character(GroupingFeatureName)))
    {
        stop("Error: Arguments 'TableName', 'FeatureName' and (optionally) 'GroupingFeatureName' must be character strings.", call. = FALSE)
    }

    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }


    require(dsBaseClient)
    require(dplyr)
    require(purrr)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Inspect meta data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get meta data of table object
    TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                          DataSources = DataSources)

    if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}

    # Get data type of feature in question
    FeatureType <- TableMetaData$FirstEligible$DataTypes[FeatureName]


    # Initiate output object
    Output <- tibble()      # Returned in this form only if no eligible feature type is present


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Numeric feature
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (FeatureType %in% c("integer", "double", "numeric"))
    {
        # --- TO DO --- : Implement grouping on server and execute functions below on grouped vectors

        # A) Sample Statistics
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Obtain SEPARATE parametric and non-parametric statistics for each server calling dsCCPhos::GetSampleStatisticsDS()
        ls_SeparateStatistics <- DSI::datashield.aggregate(conns = DataSources,
                                                           expr = call("GetSampleStatisticsDS",
                                                                       TableName.S = TableName,
                                                                       FeatureName.S = FeatureName,
                                                                       GroupingFeatureName.S = GroupingFeatureName,
                                                                       RemoveMissingsInNumeric.S = RemoveMissingsInNumeric))

        # Convert site return into tibble containing separate statistics
        df_SeparateStatistics <- ls_SeparateStatistics %>%
                                      map(\(SiteReturn) SiteReturn$Statistics) %>%
                                      list_rbind() %>%
                                      mutate(Site = names(DataSources), .before = 1)



        # Making use of dsBaseClient::ds.meadSdGp() to obtain CUMULATED parametric statistics
        ls_CumulatedStatistics_Parametric <- ds.meanSdGp(x = paste0(TableName, "$", FeatureName),
                                                         y = "1",
                                                         datasources = DataSources)

        # Making use of dsBaseClient::ds.quantileMean() to obtain CUMULATED non-parametric statistics
        vc_CumulatedStatistics_Nonparametric <- ds.quantileMean(x = paste0(TableName, "$", FeatureName),
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

        # Glue separate and cumulated statistics together
        df_Statistics <- bind_rows(df_SeparateStatistics,
                                   df_CumulatedStatistics)


        # B) Sample Meta Data
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

        # Convert site return into tibble containing separate statistics
        df_SeparateMetaData <- ls_SeparateStatistics %>%
                                    map(\(SiteReturn) SiteReturn$MetaData) %>%
                                    list_rbind() %>%
                                    mutate(Site = names(DataSources), .before = 1)

        # Calculate cumulated meta data
        df_CumulatedMetaData <- tibble(Site = "All",
                                       N_Total = sum(df_SeparateMetaData$N_Total),
                                       N_Valid = sum(df_SeparateMetaData$N_Valid),
                                       ValidProportion = N_Valid / N_Total,
                                       N_Missing = sum(df_SeparateMetaData$N_Missing),
                                       MissingProportion = N_Missing / N_Total)

        # Glue separate and cumulated meta data together
        df_MetaData <- bind_rows(df_SeparateMetaData,
                                 df_CumulatedMetaData)

    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Categorical / Character feature
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (FeatureType == "character")
    {

    }

    return(Output)
}
