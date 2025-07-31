
#' ds.GetFeatureInfo
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Obtain data about feature type and sample size.
#'
#' Linked to server-side \code{AGGREGATE} function \code{GetFeatureInfoDS()}.
#'
#' @param TableName \code{string} - Name of the table containing the feature of concern
#' @param FeatureName \code{string} - Name of feature
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{tibble} containing separate and cumulated meta data about feature type and sample size.
#' @export
#'
#' @author Bastian Reiter
ds.GetFeatureInfo <- function(TableName,
                              FeatureName,
                              DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dsBaseClient)
  require(dplyr)
  require(purrr)

  # --- For Testing Purposes ---
  # TableName <- "ADS_Patients"
  # FeatureName <- "TNM_T"
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Get meta data of table object
  TableMetaData <- ds.GetObjectMetaData(ObjectName = TableName,
                                        DSConnections = DSConnections)

  # Stop execution if referred table object is not a data.frame
  if (TableMetaData$FirstEligible$Class != "data.frame") { stop("Error: The referred table object does not seem to be a data.frame.", call. = FALSE)}


  # ServerReturns: Obtain feature meta data for each server calling dsCCPhos::GetFeatureInfoDS()
  ls_ServerReturns <- DSI::datashield.aggregate(conns = DSConnections,
                                              expr = call("GetFeatureInfoDS",
                                                          TableName.S = TableName,
                                                          FeatureName.S = FeatureName))

  # Convert Server returns into tibble containing separate feature meta data
  df_SeparateMetaData <- ls_ServerReturns %>%
                              list_rbind(names_to = "Server")

  # Obtaining return value for cumulated feature data type
  ReturnedFeatureDataTypes <- unique(df_SeparateMetaData$DataType[!is.na(df_SeparateMetaData$DataType)])
  CumulatedDataType <- NA
  if (length(ReturnedFeatureDataTypes) == 1) { CumulatedDataType <- ReturnedFeatureDataTypes }
  if (length(ReturnedFeatureDataTypes) > 1) { CumulatedDataType <- "Inconclusive"}

  # Obtain cumulated feature meta data
  df_CumulatedMetaData <- tibble(Server = "All",
                                 DataType = CumulatedDataType,
                                 N_Total = sum(df_SeparateMetaData$N_Total),
                                 N_Valid = sum(df_SeparateMetaData$N_Valid),
                                 ValidProportion = N_Valid / N_Total,
                                 N_Missing = sum(df_SeparateMetaData$N_Missing),
                                 MissingProportion = N_Missing / N_Total)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Glue cumulated and separate feature meta data together and return tibble
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(bind_rows(df_CumulatedMetaData,
                   df_SeparateMetaData))
}
