
#' ds.CheckDataSet
#'
#' `r lifecycle::badge("stable")` \cr\cr
#' Check out a data set (\code{list} of \code{data.frames}) on servers and return a coherent summary across servers.
#'
#' Linked to server-side AGGREGATE method \code{CheckDataSetDS()}
#'
#' @param DataSetName \code{string} - Name of Data Set object (list) on server, usually "RawDataSet", "CuratedDataSet" or "AugmentedDataSet"
#' @param RequiredTableNames \code{character vector} - Names of tables that are expected/required to be in the data set - Default: Names of elements in list evaluated from \code{DataSetName.S}
#' @param RequiredFeatureNames \code{list} of \code{character vectors} - Features that are expected/required in each table of the data set - Default: Names of features in respective table
#' @param AssumeCCPDataSet \code{logical} - Whether or not the data set to be checked out is one of the main data sets used in CCPhos - Default: FALSE
#' @param DSConnections \code{list} of \code{DSConnection} objects. This argument may be omitted if such an object is already uniquely specified in the global environment.
#'
#' @return A \code{list} containing compiled meta data about RDS table:
#'         \itemize{\item TableStatus
#'                  \item TableRowCounts
#'                  \item FeatureStatus
#'                  \item FeatureTypes
#'                  \item NonMissingValueRates }
#' @export
#'
#' @author Bastian Reiter
ds.CheckDataSet <- function(DataSetName,
                            RequiredTableNames = NULL,
                            RequiredFeatureNames = NULL,
                            AssumeCCPDataSet = FALSE,
                            DSConnections = NULL)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(dplyr)
  require(purrr)
  require(stringr)
  require(tidyr)

  #--- For testing purposes ---
  # DataSetName <- "CuratedDataSet"
  # AssumeCCPDataSet <- TRUE
  # RequiredTableNames = paste0("RDS_", dsCCPhos::Meta_Tables$TableName_Curated)
  # RequiredFeatureNames = RequiredTableNames.S %>%
  #                             map(\(tablename) filter(dsCCPhos::Meta_Features, TableName_Curated == str_remove(tablename, "RDS_"))$FeatureName_Raw) %>%
  #                             set_names(RequiredTableNames.S)
  # DSConnections <- CCPConnections

  # Check validity of 'DSConnections' or find them programmatically if none are passed
  DSConnections <- CheckDSConnections(DSConnections)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Check argument eligibility
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!(is.character(DataSetName)))
  {
      stop("Error: Argument 'DataSetName.S' must be a character string.", call. = FALSE)
  }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Server function call to get list of lists
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  TableCheck <- DSI::datashield.aggregate(conns = DSConnections,
                                          expr = call("CheckDataSetDS",
                                                      DataSetName.S = DataSetName,
                                                      RequiredTableNames.S = RequiredTableNames,
                                                      RequiredFeatureNames.S = RequiredFeatureNames,
                                                      AssumeCCPDataSet.S = AssumeCCPDataSet))


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Transform into cumulated report objects
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Create data frame containing "traffic light" info about existence/completeness of RDS tables
  TableStatus <- TableCheck %>%
                      map(function(SiteTableCheck)
                          {
                              SiteTableStatus <- SiteTableCheck %>%
                                                    map_chr(function(TableInfo)
                                                            {
                                                                Status <- "grey"

                                                                if (TableInfo$TableExists == FALSE) { Status <- "red" }
                                                                else if (TableInfo$TableExists == TRUE & TableInfo$TableComplete == TRUE) { Status <- "green" }
                                                                else if (TableInfo$TableExists == TRUE & TableInfo$TableComplete == FALSE) { Status <- "yellow" }

                                                                CountExistingFeatures <- sum(TableInfo$FeatureCheck$Exists)
                                                                CountTotalFeatures <- nrow(TableInfo$FeatureCheck)

                                                                if (Status != "grey") { Status <- paste0(Status, " (", CountExistingFeatures, "/", CountTotalFeatures, ")") }

                                                            }) %>%
                                                    rbind() %>%
                                                    as_tibble()
                          }) %>%
                      list_rbind(names_to = "SiteName") %>%
                      mutate(CheckRDSTables = case_when(if_all(-SiteName, ~ str_starts(.x, "green")) ~ "green",
                                                        if_any(-SiteName, ~ str_starts(.x, "red")) ~ "red",
                                                        if_any(-SiteName, ~ str_starts(.x, "yellow")) ~ "yellow",
                                                        TRUE ~ "grey"))


  # Create list of data frames (one per RDS table) containing table row counts at different sites
  TableRowCounts <- TableCheck %>%
                        list_transpose() %>%
                        map(function(TableInfo)
                            {
                                TableInfo %>%
                                     map(\(SiteTableInfo) tibble(RowCount = SiteTableInfo$RowCount)) %>%
                                     list_rbind(names_to = "SiteName")
                            })


  # Create list of data frames (one per RDS table) containing info about existence of table features
  FeatureExistence <- TableCheck %>%
                          list_transpose() %>%
                          map(function(TableInfo)
                              {
                                  TableInfo %>%
                                      map(\(SiteTableInfo) SiteTableInfo$FeatureCheck %>% select(Feature, Exists)) %>%
                                      list_rbind(names_to = "SiteName") %>%
                                      pivot_wider(names_from = Feature,
                                                  values_from = Exists)
                              })


  # Create list of data frames (one per RDS table) containing table's feature types
  FeatureTypes <- TableCheck %>%
                      list_transpose() %>%
                      map(function(TableInfo)
                          {
                              TableInfo %>%
                                  map(\(SiteTableInfo) SiteTableInfo$FeatureCheck %>% select(Feature, Type)) %>%
                                  list_rbind(names_to = "SiteName") %>%
                                  pivot_wider(names_from = Feature,
                                              values_from = Type)
                          })


  # Create list of data frames (one per RDS table) containing feature-specific non-missing value rates
  NonMissingValueRates <- TableCheck %>%
                              list_transpose() %>%
                              map(function(TableInfo)
                                  {
                                      TableInfo %>%
                                          map(\(SiteTableInfo) SiteTableInfo$FeatureCheck %>% select(Feature, NonMissingValueRate)) %>%
                                          list_rbind(names_to = "SiteName") %>%
                                          pivot_wider(names_from = Feature,
                                                      values_from = NonMissingValueRate)
                                  })


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return statement
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  return(list(TableStatus = TableStatus,
              TableRowCounts = TableRowCounts,
              FeatureExistence = FeatureExistence,
              FeatureTypes = FeatureTypes,
              NonMissingValueRates = NonMissingValueRates))
}
