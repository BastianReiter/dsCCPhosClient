
#' ds.GetFeatureInfo
#'
#' Obtain data about feature type and sample size.
#'
#' Linked to server-side \code{AGGREGATE} function \code{dsCCPhos::GetFeatureInfoDS()}.
#'
#' @param DataSources \code{list} of DSConnection objects
#' @param TableName \code{string} | Name of the table containing the feature of concern
#' @param FeatureName \code{string} | Name of feature
#'
#' @return A \code{tibble} containing separate and cumulated meta data about feature type and sample size.
#' @export
#'
#' @author Bastian Reiter
ds.GetFeatureInfo <- function(DataSources = NULL,
                              TableName,
                              FeatureName)
{
    # For Testing Purposes
    # DataSources <- CCPConnections
    # TableName <- "ADS_Patients"
    # FeatureName <- "TNM_T"

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

    require(dsBaseClient)
    require(dplyr)
    require(purrr)


    # Get meta data of table object
    TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                          DataSources = DataSources)

    # Stop execution if referred table object is not a data.frame
    if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}


    # SiteReturns: Obtain feature meta data for each server calling dsCCPhos::GetFeatureInfoDS()
    ls_SiteReturns <- DSI::datashield.aggregate(conns = DataSources,
                                                expr = call("GetFeatureInfoDS",
                                                            TableName.S = TableName,
                                                            FeatureName.S = FeatureName))

    # Convert site returns into tibble containing separate feature meta data
    df_SeparateMetaData <- ls_SiteReturns %>%
                                list_rbind(names_to = "Site")

    # Obtaining return value for cumulated feature data type
    ReturnedFeatureDataTypes <- unique(df_SeparateMetaData$DataType[!is.na(df_SeparateMetaData$DataType)])
    CumulatedDataType <- NA
    if (length(ReturnedFeatureDataTypes) == 1) { CumulatedDataType <- ReturnedFeatureDataTypes }
    if (length(ReturnedFeatureDataTypes) > 1) { CumulatedDataType <- "Inconclusive"}

    # Obtain cumulated feature meta data
    df_CumulatedMetaData <- tibble(Site = "All",
                                   DataType = CumulatedDataType,
                                   N_Total = sum(df_SeparateMetaData$N_Total),
                                   N_Valid = sum(df_SeparateMetaData$N_Valid),
                                   ValidProportion = N_Valid / N_Total,
                                   N_Missing = sum(df_SeparateMetaData$N_Missing),
                                   MissingProportion = N_Missing / N_Total)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Glue separate and cumulated feature meta data together and return tibble
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(bind_rows(df_SeparateMetaData,
                     df_CumulatedMetaData))
}
