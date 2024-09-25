
#' ds.GetFrequencyTable
#'
#' Get table of absolute and relative value frequencies for a nominal / ordinal feature.
#'
#' Linked to server-side \code{AGGREGATE} function \code{dsCCPhos::GetFrequencyTableDS()}.
#'
#' @param DataSources List of DSConnection objects
#' @param TableName String | Name of the table containing the feature of concern
#' @param FeatureName String | Name of feature
#' @param GroupingFeatureName String | Name of optional grouping feature from the same table
#' @param MaxNumberCategories \code{integer} | Maximum number of categories analyzed individually before frequencies are cumulated in 'Other' category. | Default: 10
#'
#' @return A \code{list} containing:
#'         \itemize{\item AbsoluteFrequencies (\code{tibble}: Absolute value frequencies)
#'                  \item RelativeFrequencies (\code{tibble}: Relative value frequencies)}
#' @export
#'
#' @author Bastian Reiter
ds.GetFrequencyTable <- function(DataSources = NULL,
                                 TableName,
                                 FeatureName,
                                 GroupingFeatureName = NULL,
                                 MaxNumberCategories = 10)
{
    # For Testing Purposes
    # DataSources <- CCPConnections
    # TableName <- "ADS_Patients"
    # FeatureName <- "TNM_T"
    # GroupingFeatureName <- NULL
    # MaxNumberCategories <- 5


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
    require(stringr)
    require(tibble)
    require(tidyr)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check if addressed objects (Table and Feature) are eligible
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get meta data of table object
    TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                          DataSources = DataSources)

    if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}

    # Get data type of feature in question
    FeatureType <- TableMetaData$FirstEligible$DataTypes[FeatureName]

    # Stop function if referred feature is of class 'numeric' or similar
    if (FeatureType %in% c("double", "integer", "numeric")) { stop(paste0("Error: The referred feature '", FeatureName, "' is of class '", FeatureType, "' and therefore not suitable."), call. = FALSE) }


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


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Cumulation
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    SiteNames <- names(DataSources)

    # Convert site returns into tibble containing separate frequency tables
    df_FrequencyTable <- ls_SiteReturns %>%
                              list_rbind(names_to = "Site") %>%
                              pivot_wider(names_from = Site,
                                          names_glue = "{Site}_{.value}",
                                          names_vary = "slowest",
                                          values_from = c(AbsoluteFrequency, RelativeFrequency)) %>%
                              mutate(All_AbsoluteFrequency = rowSums(pick(paste0(SiteNames, "_AbsoluteFrequency")), na.rm = TRUE),
                                     All_RelativeFrequency = All_AbsoluteFrequency / sum(All_AbsoluteFrequency),
                                     .after = Value) %>%
                              arrange(desc(All_AbsoluteFrequency))

    # If the number of unique values exceeds 'MaxNumberCategories', cumulate less frequent categories under 'Other' category
    if (!is.null(MaxNumberCategories))
    {
      if (nrow(df_FrequencyTable) > MaxNumberCategories)
      {
           FrequenciesKeep <- df_FrequencyTable %>%
                                  slice_head(n = MaxNumberCategories)

           FrequenciesCumulate <- df_FrequencyTable %>%
                                      slice_tail(n = nrow(df_FrequencyTable) - MaxNumberCategories) %>%
                                      select(-Value) %>%
                                      colSums(na.rm = TRUE) %>%
                                      as_tibble_row() %>%
                                      mutate(Value = "Other")

           df_FrequencyTable <- bind_rows(FrequenciesKeep,
                                          FrequenciesCumulate)
      }
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Restructuring output
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Select columns containing absolute frequencies and transpose tibble using combination of pivot_longer() and pivot_wider()
    df_AbsoluteFrequencies <- df_FrequencyTable %>%
                                  select(Value,
                                         contains("AbsoluteFrequency")) %>%
                                  rename_with(.fn = \(colnames) str_remove(colnames, "_AbsoluteFrequency"),
                                              .cols = contains("AbsoluteFrequency")) %>%
                                  pivot_longer(cols = -Value,
                                               names_to = "Site") %>%
                                  pivot_wider(names_from = Value,
                                              values_from = value)

    # Select columns containing relative frequencies and transpose tibble using combination of pivot_longer() and pivot_wider()
    df_RelativeFrequencies <- df_FrequencyTable %>%
                                  select(Value,
                                         contains("RelativeFrequency")) %>%
                                  rename_with(.fn = \(colnames) str_remove(colnames, "_RelativeFrequency"),
                                              .cols = contains("RelativeFrequency")) %>%
                                  pivot_longer(cols = -Value,
                                               names_to = "Site",
                                               values_to = "RelativeFrequency") %>%
                                  pivot_wider(names_from = Value,
                                              values_from = RelativeFrequency)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Return statement
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(list(AbsoluteFrequencies = df_AbsoluteFrequencies,
                RelativeFrequencies = df_RelativeFrequencies))
}
