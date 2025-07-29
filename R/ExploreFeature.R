
#' ExploreFeature
#'
#' Get characterizing statistics about a feature of arbitrary data type.
#'
#' @param DataSources \code{list} of \code{DSConnection} objects
#' @param TableName \code{string} - Name of the table containing the feature of concern
#' @param FeatureName \code{string} - Name of feature
#'
#' @return
#' @export
#'
#' @author Bastian Reiter
ExploreFeature <- function(DataSources = NULL,
                           TableName,
                           FeatureName,
                           ...)
{
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


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check if addressed table object is a data.frame
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Get meta data of table object
    TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                          DataSources = DataSources)

    # Stop execution if referred table object is not a data.frame
    if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get feature meta data (total and effective/valid sample size)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    df_FeatureInfo <- ds.GetFeatureInfo(DataSources = DataSources,
                                        TableName = TableName,
                                        FeatureName = FeatureName)


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get statistics depending on feature data type
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    # Initiate df_Statistics
    df_Statistics <- tibble()

    # Get data type of feature in question
    FeatureType <- filter(df_FeatureInfo, Site == "All")$DataType


    if (FeatureType == "numeric")
    {
        df_Statistics <- ds.GetSampleStatistics(DataSources = DataSources,
                                                TableName = TableName,
                                                MetricFeatureName = FeatureName,
                                                ...)
    }

    if (FeatureType %in% c("character", "logical"))
    {
        df_Statistics <- ds.GetFrequencyTable(DataSources = DataSources,
                                              TableName = TableName,
                                              FeatureName = FeatureName,
                                              ...)
    }

    if (FeatureType == "Date")
    {
        df_Statistics <- ds.GetSampleStatistics(DataSources = DataSources,
                                                TableName = TableName,
                                                MetricFeatureName = FeatureName,
                                                ...)
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Return statement
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    return(list(FeatureInfo = df_FeatureInfo,
                Statistics = df_Statistics))
}
