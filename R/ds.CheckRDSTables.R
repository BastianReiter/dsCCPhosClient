
#' ds.CheckRDSTables
#'
#' Check Raw Data Set (RDS) tables on servers on existence and completeness and return a coherent summary across servers.
#'
#' Linked to server-side AGGREGATE method CheckRDSTablesDS()
#'
#' @param DataSources \code{list} of \code{DSConnection} objects
#'
#' @return A \code{list} containing compiled meta data about RDS table:
#'          \itemize{ \item TableStatus
#'                    \item TableRowCounts
#'                    \item FeatureStatus
#'                    \item FeatureTypes
#'                    \item NonMissingValueRates }
#' @export
#'
#' @author Bastian Reiter
ds.CheckRDSTables <- function(DataSources = NULL,
                              RawDataSetName = "RawDataSet")
{
    require(dplyr)
    require(purrr)
    require(stringr)
    require(tidyr)

    ### For testing purposes
    # DataSources <- CCPConnections
    # RawDataSetName <- "RawDataSet"

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Check argument eligibility
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    if (!(is.character(RawDataSetName)))
    {
        stop("Error: Argument 'RawDataSetName' must be a character string.", call. = FALSE)
    }

    if (is.null(DataSources))
    {
        DataSources <- DSI::datashield.connections_find()
    }


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Server function call to get list of lists
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ServerCall <- call("CheckRDSTablesDS",
                       RawDataSetName.S = RawDataSetName)

    TableCheck <- DSI::datashield.aggregate(conns = DataSources,
                                            expr = ServerCall)


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
