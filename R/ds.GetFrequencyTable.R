
#' ds.GetFrequencyTable
#'
#' Get table of absolute and relative value frequencies (proportions) for a nominal / ordinal feature.
#'
#' Linked to server-side \code{AGGREGATE} function \code{dsCCPhos::GetFrequencyTableDS()}.
#'
#' @param DataSources List of DSConnection objects
#' @param TableName String | Name of the table containing the feature of concern
#' @param FeatureName String | Name of feature
#' @param GroupingFeatureName String | Name of optional grouping feature from the same table
#' @param MaxNumberCategories \code{integer} | Maximum number of categories analyzed individually before frequencies are cumulated in 'Other' category.
#'
#' @return A \code{list} containing:
#'         \itemize{\item MetaData (\code{tibble}): Meta data about data type and sample size
#'                  \item FrequencyTable (\code{tibble}: Absolute and relative frequencies)}
#' @export
#'
#' @author Bastian Reiter
ds.GetFrequencyTable <- function(DataSources = NULL,
                                 TableName,
                                 FeatureName,
                                 GroupingFeatureName = NULL,
                                 MaxNumberCategories = NULL)
{
    # For Testing Purposes
    # DataSources <- CCPConnections
    # TableName <- "ADS_Patients"
    # FeatureName <- "TNM_T"
    # GroupingFeatureName <- NULL
    # MaxNumberCategories <- NULL


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check argument eligibility
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (!(is.character(TableName) & is.character(FeatureName)))
    {
        stop("Error: Arguments 'TableName' and 'FeatureName' must be character strings.", call. = FALSE)
    }

    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Package requirements
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    require(dplyr)
    require(purrr)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check if addressed objects (Table and Feature) are eligible
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get meta data of table object
    TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                          DataSources = DataSources)

    if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}

    # Get data type of feature in question
    FeatureType <- TableMetaData$FirstEligible$DataTypes[FeatureName]

    # Stop function if referred feature is of class "numeric
    if (FeatureType %in% c("integer", "double", "numeric")) { stop("Error: The referred feature can not be of class 'numeric'.", call. = FALSE) }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Separate returns
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # SiteReturns: Obtain sample statistics for each server calling dsCCPhos::GetFrequencyTableDS()
    ls_SiteReturns <- DSI::datashield.aggregate(conns = DataSources,
                                                expr = call("GetFrequencyTableDS",
                                                            TableName.S = TableName,
                                                            FeatureName.S = FeatureName,
                                                            GroupingFeatureName.S = GroupingFeatureName))


    # --- TO DO --- : Implement grouping on server and execute functions below on grouped vectors


    # Convert site returns into tibble containing separate frequency tables
    df_SeparateFrequencyTables <- ls_SiteReturns %>%
                                      list_rbind(names_to = "Site")

    # Get cumulated frequencies and proportions
    df_CumulatedFrequencyTable <- df_SeparateFrequencyTables %>%
                                      group_by(Value) %>%
                                          summarize(Frequency = sum(Frequency)) %>%
                                      ungroup() %>%
                                      arrange(desc(Frequency))

    # If the number of unique values exceeds 'MaxNumberCategories', cumulate less frequent categories under 'Other' category
    if (!is.null(MaxNumberCategories))
    {
      if (nrow(df_CumulatedFrequencyTable) > MaxNumberCategories)
      {
           FrequenciesKeep <- df_CumulatedFrequencyTable %>%
                                  slice_head(n = MaxNumberCategories)

           FrequenciesCumulate <- df_CumulatedFrequencyTable %>%
                                      slice_tail(n = nrow(df_CumulatedFrequencyTable) - MaxNumberCategories)

           df_CumulatedFrequencyTable <- bind_rows(FrequenciesKeep,
                                                   tibble(Value = "Other",
                                                          Frequency = sum(FrequenciesCumulate$Frequency)))
      }
    }

    # Calculate proportions
    df_CumulatedFrequencyTable <- df_CumulatedFrequencyTable %>%
                                      mutate(Site = "All",
                                             Proportion = Frequency / sum(Frequency))


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Return statement
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(bind_rows(df_SeparateFrequencyTables,
                     df_CumulatedFrequencyTable))
}
